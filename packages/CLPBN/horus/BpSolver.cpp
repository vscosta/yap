#include <cassert>
#include <limits>

#include <algorithm>

#include <iostream>

#include "BpSolver.h"
#include "FactorGraph.h"
#include "Factor.h"
#include "Indexer.h"
#include "Horus.h"


BpSolver::BpSolver (const FactorGraph& fg) : Solver (fg)
{
  fg_ = &fg;
  runned_ = false;
}



BpSolver::~BpSolver (void)
{
  for (size_t i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  for (size_t i = 0; i < facsI_.size(); i++) {
    delete facsI_[i];
  }
  for (size_t i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



Params
BpSolver::solveQuery (VarIds queryVids)
{
  assert (queryVids.empty() == false);
  return queryVids.size() == 1
      ? getPosterioriOf (queryVids[0])
      : getJointDistributionOf (queryVids);
}



void
BpSolver::printSolverFlags (void) const
{
  stringstream ss;
  ss << "belief propagation [" ;
  ss << "schedule=" ;
  typedef BpOptions::Schedule Sch;
  switch (BpOptions::schedule) {
    case Sch::SEQ_FIXED:    ss << "seq_fixed";    break;
    case Sch::SEQ_RANDOM:   ss << "seq_random";   break;
    case Sch::PARALLEL:     ss << "parallel";     break;
    case Sch::MAX_RESIDUAL: ss << "max_residual"; break;
  }
  ss << ",max_iter="   << Util::toString (BpOptions::maxIter);
  ss << ",accuracy="   << Util::toString (BpOptions::accuracy);
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}



Params
BpSolver::getPosterioriOf (VarId vid)
{
  if (runned_ == false) {
    runSolver();
  }
  assert (fg_->getVarNode (vid));
  VarNode* var = fg_->getVarNode (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const SpLinkSet& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        probs += links[i]->getMessage();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        probs *= links[i]->getMessage();
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



Params
BpSolver::getJointDistributionOf (const VarIds& jointVarIds)
{
  if (runned_ == false) {
    runSolver();
  }
  VarNode* vn = fg_->getVarNode (jointVarIds[0]);
  const FacNodes& facNodes = vn->neighbors();
  size_t idx = facNodes.size();
  for (size_t i = 0; i < facNodes.size(); i++) {
    if (facNodes[i]->factor().contains (jointVarIds)) {
      idx = i;
      break;
    }
  }
  if (idx == facNodes.size()) {
    return getJointByConditioning (jointVarIds);
  } else {
    Factor res (facNodes[idx]->factor());
    const SpLinkSet& links = ninf(facNodes[idx])->getLinks();
    for (size_t i = 0; i < links.size(); i++) {
      Factor msg ({links[i]->getVariable()->varId()},
                  {links[i]->getVariable()->range()},
                  getVar2FactorMsg (links[i]));
      res.multiply (msg);
    }
    res.sumOutAllExcept (jointVarIds);
    res.reorderArguments (jointVarIds);
    res.normalize();
    Params jointDist = res.params();
    if (Globals::logDomain) {
      Util::exp (jointDist);
    }
    return jointDist;
  }
}



void
BpSolver::runSolver (void)
{
  initializeSolver();
  nIters_ = 0;
  while (!converged() && nIters_ < BpOptions::maxIter) {
    nIters_ ++;
    if (Globals::verbosity > 1) {
      Util::printHeader (string ("Iteration ") + Util::toString (nIters_));
    }
    switch (BpOptions::schedule) {
     case BpOptions::Schedule::SEQ_RANDOM:
       random_shuffle (links_.begin(), links_.end());
       // no break
      case BpOptions::Schedule::SEQ_FIXED:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateAndUpdateMessage (links_[i]);
        }
        break;
      case BpOptions::Schedule::PARALLEL:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (size_t i = 0; i < links_.size(); i++) {
          updateMessage(links_[i]);
        }
        break;
      case BpOptions::Schedule::MAX_RESIDUAL:
        maxResidualSchedule();
        break;
    }
  }
  if (Globals::verbosity > 0) {
    if (nIters_ < BpOptions::maxIter) {
      cout << "Sum-Product converged in " ; 
      cout << nIters_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
    cout << endl;
  }
  runned_ = true;
}



void
BpSolver::createLinks (void)
{
  const FacNodes& facNodes = fg_->facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighbors = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighbors.size(); j++) {
      links_.push_back (new SpLink (facNodes[i], neighbors[j]));
    }
  }
}



void
BpSolver::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      cout << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); ++it) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    SpLink* link = *it;
    if (link->getResidual() < BpOptions::accuracy) {
      return;
    }
    updateMessage (link);
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->getVariable()->neighbors();
    for (size_t i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->getFactor()) {
        const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
        for (size_t j = 0; j < links.size(); j++) {
          if (links[j]->getVariable() != link->getVariable()) {
            calculateMessage (links[j]);
            SpLinkMap::iterator iter = linkMap_.find (links[j]);
            sortedOrder_.erase (iter->second);
            iter->second = sortedOrder_.insert (links[j]);
          }
        }
      }
    }
    if (Globals::verbosity > 1) {
      Util::printDashedLine();
    }
  }
}



void
BpSolver::calculateFactor2VariableMsg (SpLink* link)
{
  FacNode* src = link->getFactor();
  const VarNode* dst = link->getVariable();
  const SpLinkSet& links = ninf(src)->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned msgSize = 1;
  for (size_t i = 0; i < links.size(); i++) {
    msgSize *= links[i]->getVariable()->range();
  }
  unsigned repetitions = 1;
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->getVariable() != dst) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->getVariable()->label();
          cout << ": " ;
        }
        Util::add (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->range();
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
        }
      } else {
        unsigned range = links[i]->getVariable()->range();
        Util::add (msgProduct, Params (range, 0.0), repetitions);
        repetitions *= range;
      }
    }
  } else {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->getVariable() != dst) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->getVariable()->label();
          cout << ": " ;
        }
        Util::multiply (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->range();
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
        }
      } else {
        unsigned range = links[i]->getVariable()->range();
        Util::multiply (msgProduct, Params (range, 1.0), repetitions);
        repetitions *= range;
      }
    }
  }
  Factor result (src->factor().arguments(),
      src->factor().ranges(), msgProduct);
  result.multiply (src->factor());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    message product:  " << msgProduct << endl;
    cout << "    original factor:  " << src->factor().params() << endl;
    cout << "    factor product:   " << result.params() << endl;
  }
  result.sumOutAllExcept (dst->varId());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    marginalized:     " << result.params() << endl;
  }
  link->getNextMessage() = result.params();
  LogAware::normalize (link->getNextMessage());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    curr msg:         " << link->getMessage() << endl;
    cout << "    next msg:         " << link->getNextMessage() << endl;
  }
}



Params
BpSolver::getVar2FactorMsg (const SpLink* link) const
{
  const VarNode* src = link->getVariable();
  const FacNode* dst = link->getFactor();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), LogAware::noEvidence());
    msg[src->getEvidence()] = LogAware::withEvidence();
  } else {
    msg.resize (src->range(), LogAware::one());
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << msg;
  }
  const SpLinkSet& links = ninf (src)->getLinks();
  if (Globals::logDomain) {
    SpLinkSet::const_iterator it;
    for (it = links.begin(); it != links.end(); ++ it) {
      msg += (*it)->getMessage();
      if (Constants::SHOW_BP_CALCS) {
        cout << " x " << (*it)->getMessage();
      }
    }
    msg -= link->getMessage();
  } else {
    for (size_t i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        msg *= links[i]->getMessage();
        if (Constants::SHOW_BP_CALCS) {
          cout << " x " << links[i]->getMessage();
        }
      }
    }
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << " = " << msg;
  }
  return msg;
}



Params
BpSolver::getJointByConditioning (const VarIds& jointVarIds) const
{
  VarNodes jointVars;
  for (size_t i = 0; i < jointVarIds.size(); i++) {
    assert (fg_->getVarNode (jointVarIds[i]));
    jointVars.push_back (fg_->getVarNode (jointVarIds[i]));
  }

  FactorGraph* fg = new FactorGraph (*fg_);
  BpSolver solver (*fg);
  solver.runSolver();
  Params prevBeliefs = solver.getPosterioriOf (jointVarIds[0]);

  VarIds observedVids = {jointVars[0]->varId()};

  for (size_t i = 1; i < jointVarIds.size(); i++) {
    assert (jointVars[i]->hasEvidence() == false);
    Params newBeliefs;
    Vars observedVars;
    Ranges observedRanges;
    for (size_t j = 0; j < observedVids.size(); j++) {
      observedVars.push_back (fg->getVarNode (observedVids[j]));
      observedRanges.push_back (observedVars.back()->range());
    }
    Indexer indexer (observedRanges, false);
    while (indexer.valid()) {
      for (size_t j = 0; j < observedVars.size(); j++) {
        observedVars[j]->setEvidence (indexer[j]);
      }
      BpSolver solver (*fg);
      solver.runSolver();
      Params beliefs = solver.getPosterioriOf (jointVarIds[i]);
      for (size_t k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
      ++ indexer;
    }

    int count = -1;
    for (size_t j = 0; j < newBeliefs.size(); j++) {
      if (j % jointVars[i]->range() == 0) {
        count ++;
      }
      newBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = newBeliefs;
    observedVids.push_back (jointVars[i]->varId());
  }
  return prevBeliefs;
}



void
BpSolver::initializeSolver (void)
{
  const VarNodes& varNodes = fg_->varNodes();
  varsI_.reserve (varNodes.size());
  for (size_t i = 0; i < varNodes.size(); i++) {
    varsI_.push_back (new SPNodeInfo());
  }
  const FacNodes& facNodes = fg_->facNodes();
  facsI_.reserve (facNodes.size());
  for (size_t i = 0; i < facNodes.size(); i++) {
    facsI_.push_back (new SPNodeInfo());
  }
  createLinks();
  for (size_t i = 0; i < links_.size(); i++) {
    FacNode* src = links_[i]->getFactor();
    VarNode* dst = links_[i]->getVariable();
    ninf (dst)->addSpLink (links_[i]);
    ninf (src)->addSpLink (links_[i]);
  }
}



bool
BpSolver::converged (void)
{
  if (links_.size() == 0) {
    return true;
  }
  if (nIters_ == 0) {
    return false;
  }
  if (Globals::verbosity > 2) {
    cout << endl;
  }
  if (nIters_ == 1) {
    if (Globals::verbosity > 1) {
      cout << "no residuals" << endl << endl;
    }
    return false;
  }
  bool converged = true;
  if (BpOptions::schedule == BpOptions::Schedule::MAX_RESIDUAL) {
    double maxResidual = (*(sortedOrder_.begin()))->getResidual();
    if (maxResidual > BpOptions::accuracy) {
      converged = false;
    } else {
      converged = true;
    }
  } else {
    for (size_t i = 0; i < links_.size(); i++) {
      double residual = links_[i]->getResidual();
      if (Globals::verbosity > 1) {
        cout << links_[i]->toString() + " residual = " << residual << endl;
      }
      if (residual > BpOptions::accuracy) {
        converged = false;
        if (Globals::verbosity < 2) {
          break;
        }
      }
    }
    if (Globals::verbosity > 1) {
      cout << endl;
    }
  }
  return converged;
}



void
BpSolver::printLinkInformation (void) const
{
  for (size_t i = 0; i < links_.size(); i++) {
    SpLink* l = links_[i]; 
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << l->getMessage() << endl;
    cout << "    next msg = " ;
    cout << l->getNextMessage() << endl;
    cout << "    residual = " << l->getResidual() << endl;
  }
}

