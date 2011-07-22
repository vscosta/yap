#include "CountingBP.h"


CountingBP::~CountingBP (void)
{
  delete lfg_;
  delete fg_;
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
  links_.clear();
}



ParamSet
CountingBP::getPosterioriOf (Vid vid) const
{
  FgVarNode* var = lfg_->getEquivalentVariable (vid);
  ParamSet probs;

  if (var->hasEvidence()) {
    probs.resize (var->getDomainSize(), 0.0);
    probs[var->getEvidence()] = 1.0;
  } else {
    probs.resize (var->getDomainSize(), 1.0);
    CLinkSet links = varsI_[var->getIndex()]->getLinks();
    for (unsigned i = 0; i < links.size(); i++) {
      ParamSet msg = links[i]->getMessage();
      CountingBPLink* l = static_cast<CountingBPLink*> (links[i]);
      Util::pow (msg, l->getNumberOfEdges());
      for (unsigned j = 0; j < msg.size(); j++) {
        probs[j] *= msg[j];
      }
    }
    Util::normalize (probs);
  }
  return probs;
}



void
CountingBP::initializeSolver (void)
{
  lfg_ = new LiftedFG (*fg_);
  unsigned nUncVars    = fg_->getFgVarNodes().size();
  unsigned nUncFactors = fg_->getFactors().size();
  CFgVarSet vars = fg_->getFgVarNodes();
  unsigned nNeighborLessVars = 0;
  for (unsigned i = 0; i < vars.size(); i++) {
    CFactorSet factors = vars[i]->getFactors();
    if (factors.size() == 1 && factors[0]->getFgVarNodes().size() == 1) {
      nNeighborLessVars ++;
    }
  }
  // cout << "UNCOMPRESSED FACTOR GRAPH" << endl;
  // fg_->printGraphicalModel();
  fg_->exportToDotFormat ("uncompress.dot");

  FactorGraph *temp; 
  temp = fg_;
  fg_  = lfg_->getCompressedFactorGraph();
  unsigned nCompVars    = fg_->getFgVarNodes().size();
  unsigned nCompFactors = fg_->getFactors().size();
  
  Statistics::updateCompressingStats (nUncVars,
                                      nUncFactors,
                                      nCompVars,
                                      nCompFactors,
                                      nNeighborLessVars);

  cout << "COMPRESSED FACTOR GRAPH" << endl;
  fg_->printGraphicalModel();
  //fg_->exportToDotFormat ("compress.dot");

  SPSolver::initializeSolver();
}



void
CountingBP::createLinks (void)
{
  const FactorClusterSet fcs = lfg_->getFactorClusters();
  for (unsigned i = 0; i < fcs.size(); i++) {
    const VarClusterSet vcs = fcs[i]->getVarClusters();
    for (unsigned j = 0; j < vcs.size(); j++) {
      unsigned c = lfg_->getGroundEdgeCount (fcs[i], vcs[j]);
      links_.push_back (
        new CountingBPLink (fcs[i]->getRepresentativeFactor(),
                            vcs[j]->getRepresentativeVariable(), c));
      //cout << (links_.back())->toString() << " edge count =" << c << endl;
    }
  }
  return;
}



void 
CountingBP::deleteJunction (Factor* f, FgVarNode*)
{
  f->freeDistribution();
}



void
CountingBP::maxResidualSchedule (void)
{
  if (nIter_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      links_[i]->setNextMessage (getFactor2VarMsg (links_[i]));
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
    Link* link = *it;
    if (DL >= 2) {
      cout << "updating " << (*sortedOrder_.begin())->toString() << endl;
    }
    if (link->getResidual() < SolverOptions::accuracy) {
      return;
    }
    link->updateMessage();
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    CFactorSet factorNeighbors = link->getVariable()->getFactors();
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      CLinkSet links = factorsI_[factorNeighbors[i]->getIndex()]->getLinks();
      for (unsigned j = 0; j < links.size(); j++) {
        if (links[j]->getVariable() != link->getVariable()) { //FIXMEFIXME
          if (DL >= 2 && DL < 5) {
            cout << "    calculating " << links[j]->toString() << endl;
          }
          links[j]->setNextMessage (getFactor2VarMsg (links[j]));
          LinkMap::iterator iter = linkMap_.find (links[j]);
          sortedOrder_.erase (iter->second);
          iter->second = sortedOrder_.insert (links[j]);
        }
      }
    }
  }
}



ParamSet
CountingBP::getVar2FactorMsg (const Link* link) const
{
  const FgVarNode* src = link->getVariable();
  const Factor* dest   = link->getFactor();
  ParamSet msg;
  if (src->hasEvidence()) {
    cout << "has evidence" << endl;
    msg.resize (src->getDomainSize(), 0.0);
    msg[src->getEvidence()] = link->getMessage()[src->getEvidence()];
    cout << "-> " << link->getVariable()->getLabel()	 << " " << link->getFactor()->getLabel() << endl;
    cout << "-> p2s " << Util::parametersToString (msg) << endl;
  } else {
    msg = link->getMessage();
  }
  const CountingBPLink* l = static_cast<const CountingBPLink*> (link);
  Util::pow (msg, l->getNumberOfEdges() - 1);
  CLinkSet links = varsI_[src->getIndex()]->getLinks();
  for (unsigned i = 0; i < links.size(); i++) {
    if (links[i]->getFactor() != dest) {
      ParamSet msgFromFactor = links[i]->getMessage();
      CountingBPLink* l = static_cast<CountingBPLink*> (links[i]);
      Util::pow (msgFromFactor, l->getNumberOfEdges());
      for (unsigned j = 0; j < msgFromFactor.size(); j++) {
        msg[j] *= msgFromFactor[j];
      }
    }
  }
  return msg;
}

