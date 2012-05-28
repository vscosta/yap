#include "CbpSolver.h"


CbpSolver::CbpSolver (const FactorGraph& fg) : BpSolver (fg)
{
  cfg_ = new CFactorGraph (fg);
  fg_  = cfg_->getGroundFactorGraph();
}



CbpSolver::~CbpSolver (void)
{
  delete cfg_;
  delete fg_;
  for (size_t i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
  links_.clear();
}



void
CbpSolver::printSolverFlags (void) const
{
  stringstream ss;
  ss << "counting bp [" ;
  ss << "schedule=" ;
  typedef BpOptions::Schedule Sch;
  switch (BpOptions::schedule) {
    case Sch::SEQ_FIXED:    ss << "seq_fixed";    break;
    case Sch::SEQ_RANDOM:   ss << "seq_random";   break;
    case Sch::PARALLEL:     ss << "parallel";     break;
    case Sch::MAX_RESIDUAL: ss << "max_residual"; break;
  }
  ss << ",max_iter=" << BpOptions::maxIter;
  ss << ",accuracy=" << BpOptions::accuracy;
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << ",chkif=" << 
      Util::toString (CFactorGraph::checkForIdenticalFactors);
  ss << "]" ;
  cout << ss.str() << endl;
}



Params
CbpSolver::getPosterioriOf (VarId vid)
{
  if (runned_ == false) {
    runSolver();
  }
  assert (cfg_->getEquivalent (vid));
  VarNode* var = cfg_->getEquivalent (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const SpLinkSet& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        probs += l->powMessage();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        probs *= l->powMessage();
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



Params
CbpSolver::getJointDistributionOf (const VarIds& jointVids)
{
  VarIds eqVarIds;
  for (size_t i = 0; i < jointVids.size(); i++) {
    VarNode* vn = cfg_->getEquivalent (jointVids[i]);
    eqVarIds.push_back (vn->varId());
  }
  return BpSolver::getJointDistributionOf (eqVarIds);
}



void
CbpSolver::createLinks (void)
{
  if (Globals::verbosity > 0) {
    cout << "compressed factor graph contains " ;
    cout << fg_->nrVarNodes() << " variables and " ;
    cout << fg_->nrFacNodes() << " factors " << endl;
    cout << endl;
  }
  const FacClusters& fcs = cfg_->facClusters();
  for (size_t i = 0; i < fcs.size(); i++) {
    const VarClusters& vcs = fcs[i]->varClusters();
    for (size_t j = 0; j < vcs.size(); j++) {
      unsigned count = cfg_->getEdgeCount (fcs[i], vcs[j], j);
      if (Globals::verbosity > 1) {
        cout << "creating link " ;
        cout << fcs[i]->representative()->getLabel();
        cout << " -- " ;
        cout << vcs[j]->representative()->label();
        cout << " idx=" << j << ", count=" << count << endl;
      }
      links_.push_back (new CbpSolverLink (
          fcs[i]->representative(), vcs[j]->representative(), j, count));
    }
  }
  if (Globals::verbosity > 1) {
    cout << endl;
  }
}



void
CbpSolver::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (Globals::verbosity >= 1) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); ++it) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->residual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    SpLink* link = *it;
    if (Globals::verbosity >= 1) {
      cout << "updating " << (*sortedOrder_.begin())->toString() << endl;
    }
    if (link->residual() < BpOptions::accuracy) {
      return;
    }
    link->updateMessage();
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->varNode()->neighbors();
    for (size_t i = 0; i < factorNeighbors.size(); i++) {
      const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
      for (size_t j = 0; j < links.size(); j++) {
        if (links[j]->varNode() != link->varNode()) {
          if (Globals::verbosity > 1) {
            cout << "    calculating " << links[j]->toString() << endl;
          }
          calculateMessage (links[j]);
          SpLinkMap::iterator iter = linkMap_.find (links[j]);
          sortedOrder_.erase (iter->second);
          iter->second = sortedOrder_.insert (links[j]);
        }
      }
    }
    // in counting bp, the message that a variable X sends to
    // to a factor F depends on the message that F sent to the X 
    const SpLinkSet& links = ninf(link->facNode())->getLinks();
    for (size_t i = 0; i < links.size(); i++) {
      if (links[i]->varNode() != link->varNode()) {
        if (Globals::verbosity > 1) {
          cout << "    calculating " << links[i]->toString() << endl;
        }
        calculateMessage (links[i]);
        SpLinkMap::iterator iter = linkMap_.find (links[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (links[i]);
      }
    }
  }
}



void
CbpSolver::calcFactorToVarMsg (SpLink* _link)
{
  CbpSolverLink* link = static_cast<CbpSolverLink*> (_link);
  FacNode* src = link->facNode();
  const VarNode* dst = link->varNode();
  const SpLinkSet& links = ninf(src)->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned reps = 1;
  unsigned msgSize = Util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      const CbpSolverLink* cl = static_cast<const CbpSolverLink*> (links[i]);
      if ( ! (cl->varNode() == dst && cl->index() == link->index())) {
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
      const CbpSolverLink* cl = static_cast<const CbpSolverLink*> (links[i]);
      if ( ! (cl->varNode() == dst && cl->index() == link->index())) {
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
  assert (msgProduct.size() == src->factor().size());
  if (Globals::logDomain) {
    result.params() += src->factor().params();
  } else {
    result.params() *= src->factor().params();
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << "    message product:  " << msgProduct << endl;
    cout << "    original factor:  " << src->factor().params() << endl;
    cout << "    factor product:   " << result.params() << endl;
  }
  result.sumOutAllExceptIndex (link->index());
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
CbpSolver::getVarToFactorMsg (const SpLink* _link) const
{
  const CbpSolverLink* link = static_cast<const CbpSolverLink*> (_link);
  const VarNode* src = link->varNode();
  const FacNode* dst = link->facNode();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), LogAware::noEvidence());
    double value = link->message()[src->getEvidence()];
    if (Constants::SHOW_BP_CALCS) {
      msg[src->getEvidence()] = value;
      cout << msg << "^" << link->nrEdges() << "-1" ;
    }
    msg[src->getEvidence()] = LogAware::pow (value, link->nrEdges() - 1);
  } else {
    msg = link->message();
    if (Constants::SHOW_BP_CALCS) {
      cout << msg << "^" << link->nrEdges() << "-1" ;
    }
    LogAware::pow (msg, link->nrEdges() - 1);
  }
  const SpLinkSet& links = ninf(src)->getLinks();
  if (Globals::logDomain) {
    for (size_t i = 0; i < links.size(); i++) {
      CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
      if ( ! (cl->facNode() == dst && cl->index() == link->index())) {
        CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
        msg += cl->powMessage();
      }
    }
  } else {
    for (size_t i = 0; i < links.size(); i++) {
      CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
      if ( ! (cl->facNode() == dst && cl->index() == link->index())) {
        msg *= cl->powMessage();
        if (Constants::SHOW_BP_CALCS) {
          cout << " x " << cl->nextMessage() << "^" << link->nrEdges(); 
        }
      }
    }
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << " = " << msg;
  }
  return msg;
}



void
CbpSolver::printLinkInformation (void) const
{
  for (size_t i = 0; i < links_.size(); i++) {
    CbpSolverLink* cl = static_cast<CbpSolverLink*> (links_[i]); 
    cout << cl->toString() << ":" << endl;
    cout << "    curr msg = " << cl->message() << endl;
    cout << "    next msg = " << cl->nextMessage() << endl;
    cout << "    index    = " << cl->index() << endl;
    cout << "    nr edges = " << cl->nrEdges() << endl;
    cout << "    powered  = " << cl->powMessage() << endl;
    cout << "    residual = " << cl->residual() << endl;
  }
}

