#include <algorithm>

#include "VarElimSolver.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


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



Params
VarElimSolver::getPosterioriOf (VarId vid)
{
  assert (factorGraph_->getFgVarNode (vid));
  FgVarNode* vn = factorGraph_->getFgVarNode (vid);
  if (vn->hasEvidence()) {
    Params params (vn->nrStates(), 0.0);
    params[vn->getEvidence()] = 1.0;
    return params;
  }
  return getJointDistributionOf (VarIds() = {vid});
}



Params
VarElimSolver::getJointDistributionOf (const VarIds& vids)
{
  factorList_.clear();
  varFactors_.clear();
  elimOrder_.clear();
  createFactorList();
  introduceEvidence();
  chooseEliminationOrder (vids);
  processFactorList (vids);
  Params params = factorList_.back()->getParameters();
  if (Globals::logDomain) {
    Util::fromLog (params);
  }
  delete factorList_.back();
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
          factorList_[idxs[j]]->absorveEvidence (
              varNodes[i]->varId(), varNodes[i]->getEvidence());
        }
      }
    }
  }
}



void
VarElimSolver::chooseEliminationOrder (const VarIds& vids)
{
  if (bayesNet_) {
    ElimGraph graph (*bayesNet_);
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
VarElimSolver::processFactorList (const VarIds& vids)
{
  for (unsigned i = 0; i < elimOrder_.size(); i++) {
    eliminate (elimOrder_[i]);
  }

  Factor* finalFactor = new Factor();
  for (unsigned i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      finalFactor->multiply (*factorList_[i]);
      delete factorList_[i];
      factorList_[i] = 0;
    }
  }

  VarIds unobservedVids;
  for (unsigned i = 0; i < vids.size(); i++) {
    if (factorGraph_->getFgVarNode (vids[i])->hasEvidence() == false) {
      unobservedVids.push_back (vids[i]);
    }
  }

  finalFactor->reorderVariables (unobservedVids);
  finalFactor->normalize();
  factorList_.push_back (finalFactor);
}



void
VarElimSolver::eliminate (VarId elimVar)
{
  Factor* result = 0;
  FgVarNode* vn = factorGraph_->getFgVarNode (elimVar);
  vector<unsigned>& idxs = varFactors_.find (elimVar)->second;
  for (unsigned i = 0; i < idxs.size(); i++) {
    unsigned idx = idxs[i];
    if (factorList_[idx]) {
      if (result == 0) {
        result = new Factor(*factorList_[idx]);
      } else {
        result->multiply (*factorList_[idx]);
      }
      delete factorList_[idx];
      factorList_[idx] = 0;
    }
  }
  if (result != 0 && result->nrVariables() != 1) {
    result->sumOut (vn->varId());
    factorList_.push_back (result);
    const VarIds& resultVarIds = result->getVarIds();
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
      factorList_[i]->print();
      cout << endl;
    }
  }
}

