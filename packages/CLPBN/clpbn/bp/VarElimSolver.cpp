#include <algorithm>

#include "VarElimSolver.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


VarElimSolver::VarElimSolver (const FactorGraph& fg) : Solver (&fg)
{
  factorGraph_ = &fg;
}



VarElimSolver::~VarElimSolver (void)
{
  delete factorList_.back();
}



Params
VarElimSolver::getPosterioriOf (VarId vid)
{
  assert (factorGraph_->getFgVarNode (vid));
  FgVarNode* vn = factorGraph_->getFgVarNode (vid);
  if (vn->hasEvidence()) {
    Params params (vn->range(), 0.0);
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
  absorveEvidence();
  findEliminationOrder (vids);
  processFactorList (vids);
  Params params = factorList_.back()->params();
  if (Globals::logDomain) {
    Util::fromLog (params);
  }
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
VarElimSolver::absorveEvidence (void)
{
  const FgVarSet& varNodes = factorGraph_->getVarNodes();
  for (unsigned i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      const vector<unsigned>& idxs =
          varFactors_.find (varNodes[i]->varId())->second;
      for (unsigned j = 0; j < idxs.size(); j++) {
        Factor* factor = factorList_[idxs[j]];
        if (factor->nrArguments() == 1) {
          factorList_[idxs[j]] = 0;
        } else {
          factorList_[idxs[j]]->absorveEvidence (
              varNodes[i]->varId(), varNodes[i]->getEvidence());
        }
      }
    }
  }
  printActiveFactors();
}



void
VarElimSolver::findEliminationOrder (const VarIds& vids)
{
  elimOrder_ = ElimGraph::getEliminationOrder (factorList_, vids);
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

  finalFactor->reorderArguments (unobservedVids);
  finalFactor->normalize();
  factorList_.push_back (finalFactor);
}



void
VarElimSolver::eliminate (VarId elimVar)
{
  Factor* result = 0;
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
  if (result != 0 && result->nrArguments() != 1) {
    result->sumOut (elimVar);
    factorList_.push_back (result);
    const VarIds& resultVarIds = result->arguments();
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
    }
  }
}

