#include <algorithm>

#include "VarElim.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


VarElim::~VarElim (void)
{
  delete factorList_.back();
}



Params
VarElim::solveQuery (VarIds queryVids)
{
  if (Globals::verbosity > 1) {
    cout << "Solving query on " ;
    for (size_t i = 0; i < queryVids.size(); i++) {
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
VarElim::printSolverFlags (void) const
{
  stringstream ss;
  ss << "variable elimination [" ;
  ss << "elim_heuristic=" ;
  ElimHeuristic eh = ElimGraph::elimHeuristic;
  switch (eh) {
    case SEQUENTIAL:        ss << "sequential";        break;
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
VarElim::createFactorList (void)
{
  const FacNodes& facNodes = fg.facNodes();
  factorList_.reserve (facNodes.size() * 2);
  for (size_t i = 0; i < facNodes.size(); i++) {
    factorList_.push_back (new Factor (facNodes[i]->factor()));
    const VarNodes& neighs = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      unordered_map<VarId, vector<size_t>>::iterator it 
          = varFactors_.find (neighs[j]->varId());
      if (it == varFactors_.end()) {
        it = varFactors_.insert (make_pair (
            neighs[j]->varId(), vector<size_t>())).first;
      }
      it->second.push_back (i);
    }
  }
}	



void
VarElim::absorveEvidence (void)
{
  if (Globals::verbosity > 2) {
    Util::printDashedLine();
    cout << "(initial factor list)" << endl;
    printActiveFactors();
  }
  const VarNodes& varNodes = fg.varNodes();
  for (size_t i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      if (Globals::verbosity > 1) {
        cout << "-> aborving evidence on ";
        cout << varNodes[i]->label() << " = " ;
        cout << varNodes[i]->getEvidence() << endl;
      }
      const vector<size_t>& idxs =
          varFactors_.find (varNodes[i]->varId())->second;
      for (size_t j = 0; j < idxs.size(); j++) {
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
VarElim::findEliminationOrder (const VarIds& vids)
{
  elimOrder_ = ElimGraph::getEliminationOrder (factorList_, vids);
}



void
VarElim::processFactorList (const VarIds& vids)
{
  totalFactorSize_   = 0;
  largestFactorSize_ = 0;
  for (size_t i = 0; i < elimOrder_.size(); i++) {
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
  for (size_t i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      finalFactor->multiply (*factorList_[i]);
      delete factorList_[i];
      factorList_[i] = 0;
    }
  }

  VarIds unobservedVids;
  for (size_t i = 0; i < vids.size(); i++) {
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
VarElim::eliminate (VarId elimVar)
{
  Factor* result = 0;
  vector<size_t>& idxs = varFactors_.find (elimVar)->second;
  for (size_t i = 0; i < idxs.size(); i++) {
    size_t idx = idxs[i];
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
    for (size_t i = 0; i < resultVarIds.size(); i++) {
      vector<size_t>& idxs =
          varFactors_.find (resultVarIds[i])->second;
      idxs.push_back (factorList_.size() - 1);
    }
  }
}



void
VarElim::printActiveFactors (void)
{
  for (size_t i = 0; i < factorList_.size(); i++) {
    if (factorList_[i] != 0) {
      cout << factorList_[i]->getLabel() << " " ;
      cout << factorList_[i]->params() << endl;
    }
  }
}

