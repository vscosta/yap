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
  factorGraph_ = &fg;
}



BpSolver::~BpSolver (void)
{
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  for (unsigned i = 0; i < facsI_.size(); i++) {
    delete facsI_[i];
  }
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



void
BpSolver::runSolver (void)
{
  clock_t start;
  if (Constants::COLLECT_STATS) {
    start = clock();
  }
  runLoopySolver();
  if (Constants::DEBUG >= 2) {
    cout << endl;
    if (nIters_ < BpOptions::maxIter) {
      cout << "Sum-Product converged in " ; 
      cout << nIters_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
  }
  unsigned size = factorGraph_->varNodes().size();
  if (Constants::COLLECT_STATS) {
    unsigned nIters = 0;
    bool loopy = factorGraph_->isTree() == false;
    if (loopy) nIters = nIters_;
    double time = (double (clock() - start)) / CLOCKS_PER_SEC;
    Statistics::updateStatistics (size, loopy, nIters, time);
  }
}



Params
BpSolver::getPosterioriOf (VarId vid)
{
  assert (factorGraph_->getVarNode (vid));
  VarNode* var = factorGraph_->getVarNode (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const SpLinkSet& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
      for (unsigned i = 0; i < links.size(); i++) {
        Util::add (probs, links[i]->getMessage());
      }
      LogAware::normalize (probs);
      Util::fromLog (probs);
    } else {
      for (unsigned i = 0; i < links.size(); i++) {
        Util::multiply (probs, links[i]->getMessage());
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



Params
BpSolver::getJointDistributionOf (const VarIds& jointVarIds)
{
  int idx = -1;  
  VarNode* vn = factorGraph_->getVarNode (jointVarIds[0]);
  const FacNodes& facNodes = vn->neighbors();
  for (unsigned i = 0; i < facNodes.size(); i++) {
    if (facNodes[i]->factor().contains (jointVarIds)) {
      idx = i;
      break;
    }
  }
  if (idx == -1) {
    return getJointByConditioning (jointVarIds);
  } else {
    Factor res (facNodes[idx]->factor());
    const SpLinkSet& links = ninf(facNodes[idx])->getLinks();
    for (unsigned i = 0; i < links.size(); i++) {
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
      Util::fromLog (jointDist);
    }
    return jointDist;
  }
}



void
BpSolver::runLoopySolver (void)
{
  initializeSolver();
  nIters_ = 0;

  while (!converged() && nIters_ < BpOptions::maxIter) {

    nIters_ ++;
    if (Constants::DEBUG >= 2) {
      Util::printHeader (" Iteration " + nIters_);
      cout << endl;
    }

    switch (BpOptions::schedule) {
      case BpOptions::Schedule::SEQ_RANDOM:
        random_shuffle (links_.begin(), links_.end());
        // no break

      case BpOptions::Schedule::SEQ_FIXED:
        for (unsigned i = 0; i < links_.size(); i++) {
          calculateAndUpdateMessage (links_[i]);
        }
        break;

      case BpOptions::Schedule::PARALLEL:
        for (unsigned i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (unsigned i = 0; i < links_.size(); i++) {
          updateMessage(links_[i]);
        }
        break;

      case BpOptions::Schedule::MAX_RESIDUAL:
        maxResidualSchedule();
        break;
    }
    if (Constants::DEBUG >= 2) {
      cout << endl;
    }
  }
}



void
BpSolver::initializeSolver (void)
{
  const VarNodes& varNodes = factorGraph_->varNodes();
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  varsI_.reserve (varNodes.size());
  for (unsigned i = 0; i < varNodes.size(); i++) {
    varsI_.push_back (new SPNodeInfo());
  }

  const FacNodes& facNodes = factorGraph_->facNodes();
  for (unsigned i = 0; i < facsI_.size(); i++) {
    delete facsI_[i];
  }
  facsI_.reserve (facNodes.size());
  for (unsigned i = 0; i < facNodes.size(); i++) {
    facsI_.push_back (new SPNodeInfo());
  }

  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];	
  }
  createLinks();

  for (unsigned i = 0; i < links_.size(); i++) {
    FacNode* src = links_[i]->getFactor();
    VarNode* dst = links_[i]->getVariable();
    ninf (dst)->addSpLink (links_[i]);
    ninf (src)->addSpLink (links_[i]);
  }
}



void
BpSolver::createLinks (void)
{
  const FacNodes& facNodes = factorGraph_->facNodes();
  for (unsigned i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighbors = facNodes[i]->neighbors();
    for (unsigned j = 0; j < neighbors.size(); j++) {
      links_.push_back (new SpLink (facNodes[i], neighbors[j]));
    }
  }
}



bool
BpSolver::converged (void)
{
  if (links_.size() == 0) {
    return true;
  }
  if (nIters_ == 0 || nIters_ == 1) {
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
    for (unsigned i = 0; i < links_.size(); i++) {
      double residual = links_[i]->getResidual();
      if (Constants::DEBUG >= 2) {
        cout << links_[i]->toString() + " residual = " << residual << endl;
      }
      if (residual > BpOptions::accuracy) {
        converged = false;
        if (Constants::DEBUG == 0) break;
      }
    }
  }
  return converged;
}



void
BpSolver::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
    }
    return;
  }

  for (unsigned c = 0; c < links_.size(); c++) {
    if (Constants::DEBUG >= 2) {
      cout << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
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
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->getFactor()) {
        const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
        for (unsigned j = 0; j < links.size(); j++) {
          if (links[j]->getVariable() != link->getVariable()) {
            calculateMessage (links[j]);
            SpLinkMap::iterator iter = linkMap_.find (links[j]);
            sortedOrder_.erase (iter->second);
            iter->second = sortedOrder_.insert (links[j]);
          }
        }
      }
    }
    if (Constants::DEBUG >= 2) {
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
  for (unsigned i = 0; i < links.size(); i++) {
    msgSize *= links[i]->getVariable()->range();
  }
  unsigned repetitions = 1;
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (int i = links.size() - 1; i >= 0; i--) {
      if (links[i]->getVariable() != dst) {
        Util::add (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->range();
      } else {
        unsigned ds = links[i]->getVariable()->range();
        Util::add (msgProduct, Params (ds, 1.0), repetitions);
        repetitions *= ds;
      }
    }
  } else {
    for (int i = links.size() - 1; i >= 0; i--) {
      if (links[i]->getVariable() != dst) {
        if (Constants::DEBUG >= 5) {
          cout << "    message from " << links[i]->getVariable()->label();
          cout << ": " << endl;
        }
        Util::multiply (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->range();
      } else {
        unsigned ds = links[i]->getVariable()->range();
        Util::multiply (msgProduct, Params (ds, 1.0), repetitions);
        repetitions *= ds;
      }
    }
  }

  Factor result (src->factor().arguments(),
      src->factor().ranges(), msgProduct);
  result.multiply (src->factor());
  if (Constants::DEBUG >= 5) {
    cout << "    message product:  " << msgProduct << endl;
    cout << "    original factor:  " << src->factor().params() << endl;
    cout << "    factor product:   " << result.params() << endl;
  }
  result.sumOutAllExcept (dst->varId());
  if (Constants::DEBUG >= 5) {
    cout << "    marginalized:     " ;
    cout << result.params() << endl;
  }
  const Params& resultParams = result.params();
  Params& message = link->getNextMessage();
  for (unsigned i = 0; i < resultParams.size(); i++) {
     message[i] = resultParams[i];
  }
  LogAware::normalize (message);
  if (Constants::DEBUG >= 5) {
    cout << "    curr msg:         " << link->getMessage() << endl;
    cout << "    next msg:         " << message << endl;
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
    if (Constants::DEBUG >= 5) {
      cout << msg;
    }
  } else {
    msg.resize (src->range(), LogAware::one());
  }
  if (Constants::DEBUG >= 5) {
    cout << msg;
  }
  const SpLinkSet& links = ninf (src)->getLinks();
  if (Globals::logDomain) {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        Util::add (msg, links[i]->getMessage());
      }
    }
  } else {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        Util::multiply (msg, links[i]->getMessage());
        if (Constants::DEBUG >= 5) {
          cout << " x " << links[i]->getMessage();
        }
      }
    }
  }
  if (Constants::DEBUG >= 5) {
    cout << " = " << msg;
  }
  return msg;
}



Params
BpSolver::getJointByConditioning (const VarIds& jointVarIds) const
{
  VarNodes jointVars;
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    assert (factorGraph_->getVarNode (jointVarIds[i]));
    jointVars.push_back (factorGraph_->getVarNode (jointVarIds[i]));
  }

  FactorGraph* fg = new FactorGraph (*factorGraph_);
  BpSolver solver (*fg);
  solver.runSolver();
  Params prevBeliefs = solver.getPosterioriOf (jointVarIds[0]);

  VarIds observedVids = {jointVars[0]->varId()};

  for (unsigned i = 1; i < jointVarIds.size(); i++) {
    assert (jointVars[i]->hasEvidence() == false);
    Params newBeliefs;
    Vars observedVars;
    for (unsigned j = 0; j < observedVids.size(); j++) {
      observedVars.push_back (fg->getVarNode (observedVids[j]));
    }
    StatesIndexer idx (observedVars, false);
    while (idx.valid()) {
      for (unsigned j = 0; j < observedVars.size(); j++) {
        observedVars[j]->setEvidence (idx[j]);
      }
      ++ idx;
      BpSolver solver (*fg);
      solver.runSolver();
      Params beliefs = solver.getPosterioriOf (jointVarIds[i]);
      for (unsigned k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
    }

    int count = -1;
    for (unsigned j = 0; j < newBeliefs.size(); j++) {
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
BpSolver::printLinkInformation (void) const
{
  for (unsigned i = 0; i < links_.size(); i++) {
    SpLink* l = links_[i]; 
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << l->getMessage() << endl;
    cout << "    next msg = " ;
    cout << l->getNextMessage() << endl;
    cout << "    residual = " << l->getResidual() << endl;
  }
}

