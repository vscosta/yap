#include "CbpSolver.h"


CbpSolver::~CbpSolver (void)
{
  delete lfg_;
  delete factorGraph_;
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
  links_.clear();
}



Params
CbpSolver::getPosterioriOf (VarId vid)
{
  assert (lfg_->getEquivalentVariable (vid));
  FgVarNode* var = lfg_->getEquivalentVariable (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->nrStates(), Util::noEvidence());
    probs[var->getEvidence()] = Util::withEvidence();
  } else {
    probs.resize (var->nrStates(), Util::multIdenty());
    const SpLinkSet& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
        for (unsigned i = 0; i < links.size(); i++) {
          CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
          Util::add (probs, l->getPoweredMessage());
        }
        Util::normalize (probs);
        Util::fromLog (probs);
    } else {
      for (unsigned i = 0; i < links.size(); i++) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        Util::multiply (probs, l->getPoweredMessage());
      }
      Util::normalize (probs);
    }
  }
  return probs;
}



Params
CbpSolver::getJointDistributionOf (const VarIds& jointVarIds)
{
  VarIds eqVarIds;
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    eqVarIds.push_back (lfg_->getEquivalentVariable (jointVarIds[i])->varId());
  }
  return FgBpSolver::getJointDistributionOf (eqVarIds);
}




void
CbpSolver::initializeSolver (void)
{
  unsigned nGroundVars, nGroundFacs, nWithoutNeighs;
  if (COLLECT_STATISTICS) {
    nGroundVars = factorGraph_->getVarNodes().size();
    nGroundFacs = factorGraph_->getFactorNodes().size();
    const FgVarSet& vars = factorGraph_->getVarNodes();
    nWithoutNeighs = 0;
    for (unsigned i = 0; i < vars.size(); i++) {
      const FgFacSet& factors = vars[i]->neighbors();
      if (factors.size() == 1 && factors[0]->neighbors().size() == 1) {
        nWithoutNeighs ++;
      }
    }
  }

  lfg_ = new CFactorGraph (*factorGraph_);

  // cout << "Uncompressed Factor Graph" << endl;
  // factorGraph_->printGraphicalModel();
  // factorGraph_->exportToGraphViz ("uncompressed_fg.dot");
  factorGraph_  = lfg_->getCompressedFactorGraph();

  if (COLLECT_STATISTICS) {
    unsigned nClusterVars = factorGraph_->getVarNodes().size();
    unsigned nClusterFacs = factorGraph_->getFactorNodes().size();
    Statistics::updateCompressingStatistics (nGroundVars,  nGroundFacs,
                                             nClusterVars, nClusterFacs,
                                             nWithoutNeighs);
  }

  // cout << "Compressed Factor Graph" << endl;
  // factorGraph_->printGraphicalModel();
  // factorGraph_->exportToGraphViz ("compressed_fg.dot");
  // abort();
  FgBpSolver::initializeSolver();
}



void
CbpSolver::createLinks (void)
{
  const FacClusterSet fcs = lfg_->getFacClusters();
  for (unsigned i = 0; i < fcs.size(); i++) {
    const VarClusterSet vcs = fcs[i]->getVarClusters();
    for (unsigned j = 0; j < vcs.size(); j++) {
      unsigned c = lfg_->getGroundEdgeCount (fcs[i], vcs[j]);
      links_.push_back (new CbpSolverLink (fcs[i]->getRepresentativeFactor(),
          vcs[j]->getRepresentativeVariable(), c));
    }
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
      if (DL >= 2 && DL < 5) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (unsigned c = 0; c < links_.size(); c++) {
    if (DL >= 2) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    SpLink* link = *it;
    if (DL >= 2) {
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
    const FgFacSet& factorNeighbors = link->getVariable()->neighbors();
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
      for (unsigned j = 0; j < links.size(); j++) {
        if (links[j]->getVariable() != link->getVariable()) {
          if (DL >= 2 && DL < 5) {
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
        if (DL >= 2 && DL < 5) {
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



Params
CbpSolver::getVar2FactorMsg (const SpLink* link) const
{
  Params msg;
  const FgVarNode* src = link->getVariable();
  const FgFacNode* dst = link->getFactor();
  const CbpSolverLink* l = static_cast<const CbpSolverLink*> (link);
  if (src->hasEvidence()) {
    msg.resize (src->nrStates(), Util::noEvidence());
    double value = link->getMessage()[src->getEvidence()];
    msg[src->getEvidence()] = Util::pow (value, l->getNumberOfEdges() - 1);
  } else {
    msg = link->getMessage();
    Util::pow (msg, l->getNumberOfEdges() - 1);
  }
  if (DL >= 5) {
    cout << "        " << "init: " << Util::parametersToString (msg) << endl;
  }
  const SpLinkSet& links = ninf(src)->getLinks();
  if (Globals::logDomain) {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        Util::add (msg, l->getPoweredMessage());
      }
    }
  } else {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        CbpSolverLink* l = static_cast<CbpSolverLink*> (links[i]);
        Util::multiply (msg, l->getPoweredMessage());
        if (DL >= 5) {
          cout << "        msg from " << l->getFactor()->getLabel() << ": " ;
          cout << Util::parametersToString (l->getPoweredMessage()) << endl;
        }
      }
    }
  }

  if (DL >= 5) {
    cout << "        result = " << Util::parametersToString (msg) << endl;
  }
  return msg;
}



void
CbpSolver::printLinkInformation (void) const
{
  for (unsigned i = 0; i < links_.size(); i++) {
    CbpSolverLink* l = static_cast<CbpSolverLink*> (links_[i]); 
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << Util::parametersToString (l->getMessage()) << endl;
    cout << "    next msg = " ;
    cout << Util::parametersToString (l->getNextMessage()) << endl;
    cout << "    powered  = " ;
    cout << Util::parametersToString (l->getPoweredMessage()) << endl;
    cout << "    residual = " << l->getResidual() << endl;
  }
}

