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
  for (unsigned i = 0; i < links_.size(); i++) {
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
      for (unsigned i = 0; i < links.size(); i++) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        probs += l->poweredMessage();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (unsigned i = 0; i < links.size(); i++) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        probs *= l->poweredMessage();
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
  for (unsigned i = 0; i < jointVids.size(); i++) {
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
  for (unsigned i = 0; i < fcs.size(); i++) {
    const VarClusters& vcs = fcs[i]->varClusters();
    for (unsigned j = 0; j < vcs.size(); j++) {
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
    for (unsigned i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (Globals::verbosity >= 1) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (unsigned c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    SpLink* link = *it;
    if (Globals::verbosity >= 1) {
      cout << "updating " << (*sortedOrder_.begin())->toString() << endl;
    }
    if (link->getResidual() < BpOptions::accuracy) {
      return;
    }
    link->updateMessage();
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->getVariable()->neighbors();
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
      for (unsigned j = 0; j < links.size(); j++) {
        if (links[j]->getVariable() != link->getVariable()) {
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
    const SpLinkSet& links = ninf(link->getFactor())->getLinks();
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getVariable() != link->getVariable()) {
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
CbpSolver::calculateFactor2VariableMsg (SpLink* _link)
{
  CbpSolverLink* link = static_cast<CbpSolverLink*> (_link);
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
      const CbpSolverLink* cl = static_cast<const CbpSolverLink*> (links[i]);
      if ( ! (cl->getVariable() == dst && cl->index() == link->index())) {
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
    for (int i = links.size() - 1; i >= 0; i--) {
      const CbpSolverLink* cl = static_cast<const CbpSolverLink*> (links[i]);
      if ( ! (cl->getVariable() == dst && cl->index() == link->index())) {
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
  assert (msgProduct.size() == src->factor().size());
  if (Globals::logDomain) {
    for (unsigned i = 0; i < result.size(); i++) {
      result[i] += src->factor()[i];
    }
  } else {
    for (unsigned i = 0; i < result.size(); i++) {
      result[i] *= src->factor()[i];
    }
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << "    message product:  " << msgProduct << endl;
    cout << "    original factor:  " << src->factor().params() << endl;
    cout << "    factor product:   " << result.params() << endl;
  }
  result.sumOutAllExceptIndex (link->index());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    marginalized:     "  << result.params() << endl;
  }
  link->getNextMessage() = result.params();
  LogAware::normalize (link->getNextMessage());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    curr msg:         " << link->getMessage() << endl;
    cout << "    next msg:         " << link->getNextMessage() << endl;
  }
}



Params
CbpSolver::getVar2FactorMsg (const SpLink* _link) const
{
  const CbpSolverLink* link = static_cast<const CbpSolverLink*> (_link);
  const VarNode* src = link->getVariable();
  const FacNode* dst = link->getFactor();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), LogAware::noEvidence());
    double value = link->getMessage()[src->getEvidence()];
    if (Constants::SHOW_BP_CALCS) {
      msg[src->getEvidence()] = value;
      cout << msg << "^" << link->nrEdges() << "-1" ;
    }
    msg[src->getEvidence()] = LogAware::pow (value, link->nrEdges() - 1);
  } else {
    msg = link->getMessage();
    if (Constants::SHOW_BP_CALCS) {
      cout << msg << "^" << link->nrEdges() << "-1" ;
    }
    LogAware::pow (msg, link->nrEdges() - 1);
  }
  const SpLinkSet& links = ninf(src)->getLinks();
  if (Globals::logDomain) {
    for (unsigned i = 0; i < links.size(); i++) {
      CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
      if ( ! (cl->getFactor() == dst && cl->index() == link->index())) {
        CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
        msg += cl->poweredMessage();
      }
    }
  } else {
    for (unsigned i = 0; i < links.size(); i++) {
      CbpSolverLink* cl = static_cast<CbpSolverLink*> (links[i]);
      if ( ! (cl->getFactor() == dst && cl->index() == link->index())) {
        msg *= cl->poweredMessage();
        if (Constants::SHOW_BP_CALCS) {
          cout << " x " << cl->getNextMessage() << "^" << link->nrEdges(); 
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
  for (unsigned i = 0; i < links_.size(); i++) {
    CbpSolverLink* cl = static_cast<CbpSolverLink*> (links_[i]); 
    cout << cl->toString() << ":" << endl;
    cout << "    curr msg = " << cl->getMessage() << endl;
    cout << "    next msg = " << cl->getNextMessage() << endl;
    cout << "    index    = " << cl->index() << endl;
    cout << "    nr edges = " << cl->nrEdges() << endl;
    cout << "    powered  = " << cl->poweredMessage() << endl;
    cout << "    residual = " << cl->getResidual() << endl;
  }
}

