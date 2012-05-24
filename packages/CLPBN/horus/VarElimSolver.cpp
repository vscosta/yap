#include <algorithm>

#include "VarElimSolver.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


VarElimSolver::~VarElimSolver (void)
{
  delete factorList_.back();
}



Params
VarElimSolver::solveQuery (VarIds queryVids)
{
  if (Globals::verbosity > 1) {
    cout << "Solving query on " ;
    for (unsigned i = 0; i < queryVids.size(); i++) {
      if (i != 0) cout << ", " ;
      cout << fg.getVarNode (queryVids[i])->label();
    }
    cout << endl;
  }
  factorList_.clear();
  varFactors_.clear();
  elimOrder_.clear();
  createFactorList();
  absorveEvidence();
  findEliminationOrder (queryVids);
  processFactorList (queryVids);
  Params params = factorList_.back()->params();
  if (Globals::logDomain) {
    Util::exp (params);
  }
  return params;
}



void
VarElimSolver::printSolverFlags (void) const
{
  stringstream ss;
  ss << "variable elimination [" ;
  ss << "elim_heuristic=" ;
  ElimHeuristic eh = ElimGraph::elimHeuristic;
  switch (eh) {
    case MIN_NEIGHBORS:     ss << "min_neighbors";     break;
    case MIN_WEIGHT:        ss << "min_weight";        break;
    case MIN_FILL:          ss << "min_fill";          break;
    case WEIGHTED_MIN_FILL: ss << "weighted_min_fill"; break;
  }
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}



void
VarElimSolver::createFactorList (void)
{
  const FacNodes& facNodes = fg.facNodes();
  factorList_.reserve (facNodes.size() * 2);
  for (unsigned i = 0; i < facNodes.size(); i++) {
    factorList_.push_back (new Factor (facNodes[i]->factor()));
    const VarNodes& neighs = facNodes[i]->neighbors();
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
  if (Globals::verbosity > 2) {
    Util::printDashedLine();
    cout << "(initial factor list)" << endl;
    printActiveFactors();
  }
  const VarNodes& varNodes = fg.varNodes();
  for (unsigned i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      if (Globals::verbosity > 1) {
        cout << "-> aborving evidence on ";
        cout << varNodes[i]->label() << " = " ;
        cout << varNodes[i]->getEvidence() << endl;
      }
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
}



void
VarElimSolver::findEliminationOrder (const VarIds& vids)
{
  elimOrder_ = ElimGraph::getEliminationOrder (factorList_, vids);
}



void
VarElimSolver::processFactorList (const VarIds& vids)
{
  totalFactorSize_   = 0;
  largestFactorSize_ = 0;
  for (unsigned i = 0; i < elimOrder_.size(); i++) {
    if (Globals::verbosity >= 2) {
      if (Globals::verbosity >= 3) {
        Util::printDashedLine();
        printActiveFactors();
      }
      cout << "-> summing out " ;
      cout << fg.getVarNode (elimOrder_[i])->label() << endl;
    }
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
    if (fg.getVarNode (vids[i])->hasEvidence() == false) {
      unobservedVids.push_back (vids[i]);
    }
  }

  finalFactor->reorderArguments (unobservedVids);
  finalFactor->normalize();
  factorList_.push_back (finalFactor);
  if (Globals::verbosity > 0) {
    cout << "total factor size:   " << totalFactorSize_ << endl;
    cout << "largest factor size: " << largestFactorSize_ << endl;
    cout << endl;
  }
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
        result = new Factor (*factorList_[idx]);
      } else {
        result->multiply (*factorList_[idx]);
      }
      delete factorList_[idx];
      factorList_[idx] = 0;
    }
  }
  totalFactorSize_ += result->size();
  if (result->size() > largestFactorSize_) {
    largestFactorSize_ = result->size();
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
      cout << factorList_[i]->getLabel() << " " ;
      cout << factorList_[i]->params() << endl;
    }
  }
}

