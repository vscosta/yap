#include <algorithm>

#include "VarElimSolver.h"
#include "ElimGraph.h"
#include "Factor.h"


VarElimSolver::VarElimSolver (const BayesNet& bn) : Solver (&bn)
{
  bayesNet_     = &bn;
  factorGraph_  = new FactorGraph (bn);
}



VarElimSolver::VarElimSolver (const FactorGraph& fg) : Solver (&fg)
{
  bayesNet_    = 0;
  factorGraph_ = &fg;
}



VarElimSolver::~VarElimSolver (void)
{
  if (bayesNet_) {
    delete factorGraph_;
  }
}



ParamSet
VarElimSolver::getPosterioriOf (VarId vid)
{
  FgVarNode* vn = factorGraph_->getFgVarNode (vid);
  assert (vn);
  if (vn->hasEvidence()) {
    ParamSet params (vn->nrStates(), 0.0);
    params[vn->getEvidence()] = 1.0;
    return params;
  }
  return getJointDistributionOf (VarIdSet() = {vid});
}



ParamSet
VarElimSolver::getJointDistributionOf (const VarIdSet& vids)
{
  factorList_.clear();
  varFactors_.clear();
  elimOrder_.clear();
  createFactorList();
  introduceEvidence();
  chooseEliminationOrder (vids);
  processFactorList (vids);
  ParamSet params = factorList_.back()->getParameters();
  factorList_.back()->freeDistribution();
  delete factorList_.back();
  Util::normalize (params);
  return params;
}



void
VarElimSolver::createFactorList (void)
{
  const FgFacSet& factorNodes = factorGraph_->getFactorNodes();
  factorList_.reserve (factorNodes.size() * 2);
  for (unsigned i = 0; i < factorNodes.size(); i++) {
    factorList_.push_back (new Factor (*factorNodes[i]->factor()));
    const FgVarSet& neighs = factorNodes[i]->neighbors();
    for (unsigned j = 0; j < neighs.size(); j++) {
      unordered_map<VarId,vector<unsigned> >::iterator it 
          = varFactors_.find (neighs[j]->varId());
      if (it == varFactors_.end()) {
        it = varFactors_.insert (make_pair (
            neighs[j]->varId(), vector<unsigned>())).first;
      }
      it->second.push_back (i);
    }
  }
}	



void
VarElimSolver::introduceEvidence (void)
{
  const FgVarSet& varNodes = factorGraph_->getVarNodes();
  for (unsigned i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      const vector<unsigned>& idxs =
          varFactors_.find (varNodes[i]->varId())->second;
      for (unsigned j = 0; j < idxs.size(); j++) {
        Factor* factor = factorList_[idxs[j]];
        if (factor->nrVariables() == 1) {
          factorList_[idxs[j]] = 0;
        } else {
          factorList_[idxs[j]]->removeInconsistentEntries (
              varNodes[i]->varId(), varNodes[i]->getEvidence());
        }
      }
    }
  }
}



void
VarElimSolver::chooseEliminationOrder (const VarIdSet& vids)
{
  if (bayesNet_) {
    ElimGraph graph = ElimGraph (*bayesNet_);
    elimOrder_ = graph.getEliminatingOrder (vids);
  } else {
    const FgVarSet& varNodes = factorGraph_->getVarNodes();
    for (unsigned i = 0; i < varNodes.size(); i++) {
      VarId vid = varNodes[i]->varId();
      if (std::find (vids.begin(), vids.end(), vid) == vids.end() 
          && !varNodes[i]->hasEvidence()) {
        elimOrder_.push_back (vid);
      }
    }
  }
}



void
VarElimSolver::processFactorList (const VarIdSet& vids)
{
  for (unsigned i = 0; i < elimOrder_.size(); i++) {
    // cout << "-----------------------------------------" << endl;
    // cout << "Eliminating " << elimOrder_[i];
    // cout << " in the following factors:" << endl;
    // printActiveFactors();
    eliminate (elimOrder_[i]);
  }
  Factor* thisIsTheEnd = new Factor();

  for (unsigned i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      thisIsTheEnd->multiplyByFactor (*factorList_[i]);
      factorList_[i]->freeDistribution();
      delete factorList_[i];
      factorList_[i] = 0;
    }
  }
  VarIdSet vidsWithoutEvidence;
  for (unsigned i = 0; i < vids.size(); i++) {
    if (factorGraph_->getFgVarNode (vids[i])->hasEvidence() == false) {
      vidsWithoutEvidence.push_back (vids[i]);
    }
  }
  thisIsTheEnd->orderVariables (vidsWithoutEvidence);
  factorList_.push_back (thisIsTheEnd);
}



void
VarElimSolver::eliminate (VarId elimVar)
{
  FgVarNode* vn = factorGraph_->getFgVarNode (elimVar);
  Factor* result = 0;
  vector<unsigned>& idxs = varFactors_.find (elimVar)->second;
  //cout << "eliminating " << setw (5) << elimVar << ":" ;
  for (unsigned i = 0; i < idxs.size(); i++) {
    unsigned idx = idxs[i];
    if (factorList_[idx]) {
      if (result == 0) {
        result = new Factor(*factorList_[idx]);
        //cout << " " << factorList_[idx]->label();
      } else {
        result->multiplyByFactor (*factorList_[idx]);
        //cout << " x " << factorList_[idx]->label();
      }
      factorList_[idx]->freeDistribution();
      delete factorList_[idx];
      factorList_[idx] = 0;
    }
  }
  if (result != 0 && result->nrVariables() != 1) {
    result->removeVariable (vn->varId());
    factorList_.push_back (result);
    // cout << endl <<"    factor size=" << result->size() << endl;
    const VarIdSet& resultVarIds = result->getVarIds();
    for (unsigned i = 0; i < resultVarIds.size(); i++) {
      vector<unsigned>& idxs =
          varFactors_.find (resultVarIds[i])->second;
      idxs.push_back (factorList_.size() - 1);
    }
  }
}



void
VarElimSolver::printActiveFactors (void)
{
  for (unsigned i = 0; i < factorList_.size(); i++) {
    if (factorList_[i] != 0) {
      factorList_[i]->printFactor();
      cout << endl;
    }
  }
}

