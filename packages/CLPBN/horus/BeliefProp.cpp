#include <cassert>

#include <algorithm>

#include <iostream>

#include "BeliefProp.h"
#include "Indexer.h"
#include "Horus.h"


double      BeliefProp::accuracy_ = 0.0001;
unsigned    BeliefProp::maxIter_  = 1000;
MsgSchedule BeliefProp::schedule_ = MsgSchedule::SEQ_FIXED;


BeliefProp::BeliefProp (const FactorGraph& fg) : GroundSolver (fg)
{
  runned_ = false;
}



BeliefProp::~BeliefProp (void)
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
BeliefProp::solveQuery (VarIds queryVids)
{
  assert (queryVids.empty() == false);
  return queryVids.size() == 1
      ? getPosterioriOf (queryVids[0])
      : getJointDistributionOf (queryVids);
}



void
BeliefProp::printSolverFlags (void) const
{
  stringstream ss;
  ss << "belief propagation [" ;
  ss << "schedule=" ;
  switch (schedule_) {
    case MsgSchedule::SEQ_FIXED:    ss << "seq_fixed";    break;
    case MsgSchedule::SEQ_RANDOM:   ss << "seq_random";   break;
    case MsgSchedule::PARALLEL:     ss << "parallel";     break;
    case MsgSchedule::MAX_RESIDUAL: ss << "max_residual"; break;
  }
  ss << ",max_iter="   << Util::toString (maxIter_);
  ss << ",accuracy="   << Util::toString (accuracy_);
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}



Params
BeliefProp::getPosterioriOf (VarId vid)
{
  if (runned_ == false) {
    runSolver();
  }
  assert (fg.getVarNode (vid));
  VarNode* var = fg.getVarNode (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const BpLinks& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        probs += links[i]->message();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        probs *= links[i]->message();
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



Params
BeliefProp::getJointDistributionOf (const VarIds& jointVarIds)
{
  if (runned_ == false) {
    runSolver();
  }
  VarNode* vn = fg.getVarNode (jointVarIds[0]);
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
  }
  return getFactorJoint (facNodes[idx], jointVarIds);
}



Params
BeliefProp::getFactorJoint (
    FacNode* fn,
    const VarIds& jointVarIds)
{
  if (runned_ == false) {
    runSolver();
  }
  Factor res (fn->factor());
  const BpLinks& links = ninf(fn)->getLinks();
  for (size_t i = 0; i < links.size(); i++) {
    Factor msg ({links[i]->varNode()->varId()},
                {links[i]->varNode()->range()},
                getVarToFactorMsg (links[i]));
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



void
BeliefProp::runSolver (void)
{
  initializeSolver();
  nIters_ = 0;
  while (!converged() && nIters_ < maxIter_) {
    nIters_ ++;
    if (Globals::verbosity > 1) {
      Util::printHeader (string ("Iteration ") + Util::toString (nIters_));
    }
    switch (schedule_) {
     case MsgSchedule::SEQ_RANDOM:
       std::random_shuffle (links_.begin(), links_.end());
       // no break
      case MsgSchedule::SEQ_FIXED:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateAndUpdateMessage (links_[i]);
        }
        break;
      case MsgSchedule::PARALLEL:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (size_t i = 0; i < links_.size(); i++) {
          updateMessage(links_[i]);
        }
        break;
      case MsgSchedule::MAX_RESIDUAL:
        maxResidualSchedule();
        break;
    }
  }
  if (Globals::verbosity > 0) {
    if (nIters_ < maxIter_) {
      cout << "Belief propagation converged in " ;
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
BeliefProp::createLinks (void)
{
  const FacNodes& facNodes = fg.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighbors = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighbors.size(); j++) {
      links_.push_back (new BpLink (facNodes[i], neighbors[j]));
    }
  }
}



void
BeliefProp::maxResidualSchedule (void)
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
        cout << "residual = " << (*it)->residual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    BpLink* link = *it;
    if (link->residual() < accuracy_) {
      return;
    }
    updateMessage (link);
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->varNode()->neighbors();
    for (size_t i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->facNode()) {
        const BpLinks& links = ninf(factorNeighbors[i])->getLinks();
        for (size_t j = 0; j < links.size(); j++) {
          if (links[j]->varNode() != link->varNode()) {
            calculateMessage (links[j]);
            BpLinkMap::iterator iter = linkMap_.find (links[j]);
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
BeliefProp::calcFactorToVarMsg (BpLink* link)
{
  FacNode* src = link->facNode();
  const VarNode* dst = link->varNode();
  const BpLinks& links = ninf(src)->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned reps    = 1;
  unsigned msgSize = Util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->varNode() != dst) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->varNode()->label();
          cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::plus<double>());
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  } else {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->varNode() != dst) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->varNode()->label();
          cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::multiplies<double>());
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
        }
      }
      reps *= links[i]->varNode()->range();
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
  link->nextMessage() = result.params();
  LogAware::normalize (link->nextMessage());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    curr msg:         " << link->message() << endl;
    cout << "    next msg:         " << link->nextMessage() << endl;
  }
}



Params
BeliefProp::getVarToFactorMsg (const BpLink* link) const
{
  const VarNode* src = link->varNode();
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
  BpLinks::const_iterator it;
  const BpLinks& links = ninf (src)->getLinks();
  if (Globals::logDomain) {
    for (it = links.begin(); it != links.end(); ++it) {
      if (*it != link) {
        msg += (*it)->message();
      }
      if (Constants::SHOW_BP_CALCS) {
        cout << " x " << (*it)->message();
      }
    }
  } else {
    for (it = links.begin(); it != links.end(); ++it) {
      if (*it != link) {
        msg *= (*it)->message();
      }
      if (Constants::SHOW_BP_CALCS) {
        cout << " x " << (*it)->message();
      }
    }
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << " = " << msg;
  }
  return msg;
}



Params
BeliefProp::getJointByConditioning (const VarIds& jointVarIds) const
{
  return GroundSolver::getJointByConditioning (
      GroundSolverType::BP, fg, jointVarIds);
}



void
BeliefProp::initializeSolver (void)
{
  const VarNodes& varNodes = fg.varNodes();
  varsI_.reserve (varNodes.size());
  for (size_t i = 0; i < varNodes.size(); i++) {
    varsI_.push_back (new SPNodeInfo());
  }
  const FacNodes& facNodes = fg.facNodes();
  facsI_.reserve (facNodes.size());
  for (size_t i = 0; i < facNodes.size(); i++) {
    facsI_.push_back (new SPNodeInfo());
  }
  createLinks();
  for (size_t i = 0; i < links_.size(); i++) {
    FacNode* src = links_[i]->facNode();
    VarNode* dst = links_[i]->varNode();
    ninf (dst)->addBpLink (links_[i]);
    ninf (src)->addBpLink (links_[i]);
  }
}



bool
BeliefProp::converged (void)
{
  if (links_.empty()) {
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
  if (schedule_ == MsgSchedule::MAX_RESIDUAL) {
    double maxResidual = (*(sortedOrder_.begin()))->residual();
    if (maxResidual > accuracy_) {
      converged = false;
    } else {
      converged = true;
    }
  } else {
    for (size_t i = 0; i < links_.size(); i++) {
      double residual = links_[i]->residual();
      if (Globals::verbosity > 1) {
        cout << links_[i]->toString() + " residual = " << residual << endl;
      }
      if (residual > accuracy_) {
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
BeliefProp::printLinkInformation (void) const
{
  for (size_t i = 0; i < links_.size(); i++) {
    BpLink* l = links_[i];
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << l->message() << endl;
    cout << "    next msg = " ;
    cout << l->nextMessage() << endl;
    cout << "    residual = " << l->residual() << endl;
  }
}

