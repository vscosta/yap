#include <algorithm>

#include "VarElim.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


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
  totalFactorSize_   = 0;
  largestFactorSize_ = 0;
  factorList_.clear();
  varMap_.clear();
  createFactorList();
  absorveEvidence();
  Params params = processFactorList (queryVids);
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
  switch (ElimGraph::elimHeuristic()) {
    case ElimHeuristic::SEQUENTIAL:        ss << "sequential";        break;
    case ElimHeuristic::MIN_NEIGHBORS:     ss << "min_neighbors";     break;
    case ElimHeuristic::MIN_WEIGHT:        ss << "min_weight";        break;
    case ElimHeuristic::MIN_FILL:          ss << "min_fill";          break;
    case ElimHeuristic::WEIGHTED_MIN_FILL: ss << "weighted_min_fill"; break;
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
    const VarIds& args = facNodes[i]->factor().arguments();
    for (size_t j = 0; j < args.size(); j++) {
      unordered_map<VarId, vector<size_t>>::iterator it;
      it = varMap_.find (args[j]);
      if (it != varMap_.end()) {
        it->second.push_back (i);
      } else {
        varMap_[args[j]] = { i };
      }
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
      const vector<size_t>& indices = varMap_[varNodes[i]->varId()];
      for (size_t j = 0; j < indices.size(); j++) {
        size_t idx = indices[j];
        if (factorList_[idx]->nrArguments() > 1) {
          factorList_[idx]->absorveEvidence (
              varNodes[i]->varId(), varNodes[i]->getEvidence());
        } else {
          delete factorList_[idx];
          factorList_[idx] = 0;
        }
      }
    }
  }
}



Params
VarElim::processFactorList (const VarIds& queryVids)
{
  VarIds elimOrder = ElimGraph::getEliminationOrder (
      factorList_, queryVids);
  for (size_t i = 0; i < elimOrder.size(); i++) {
    if (Globals::verbosity >= 2) {
      if (Globals::verbosity >= 3) {
        Util::printDashedLine();
        printActiveFactors();
      }
      cout << "-> summing out " ;
      cout << fg.getVarNode (elimOrder[i])->label() << endl;
    }
    eliminate (elimOrder[i]);
  }

  Factor result;
  for (size_t i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      result.multiply (*factorList_[i]);
      delete factorList_[i];
      factorList_[i] = 0;
    }
  }

  VarIds unobservedVids;
  for (size_t i = 0; i < queryVids.size(); i++) {
    if (fg.getVarNode (queryVids[i])->hasEvidence() == false) {
      unobservedVids.push_back (queryVids[i]);
    }
  }

  result.reorderArguments (unobservedVids);
  result.normalize();
  if (Globals::verbosity > 0) {
    cout << "total factor size:   " << totalFactorSize_ << endl;
    cout << "largest factor size: " << largestFactorSize_ << endl;
    cout << endl;
  }
  return result.params();
}



void
VarElim::eliminate (VarId vid)
{
  Factor* result = new Factor();
  const vector<size_t>& indices = varMap_[vid];
  for (size_t i = 0; i < indices.size(); i++) {
    size_t idx = indices[i];
    if (factorList_[idx]) {
      result->multiply (*factorList_[idx]);
      delete factorList_[idx];
      factorList_[idx] = 0;
    }
  }
  totalFactorSize_ += result->size();
  if (result->size() > largestFactorSize_) {
    largestFactorSize_ = result->size();
  }
  if (result->nrArguments() > 1) {
    result->sumOut (vid);
    const VarIds& args = result->arguments();
    for (size_t i = 0; i < args.size(); i++) {
      vector<size_t>& indices2 = varMap_[args[i]];
      indices2.push_back (factorList_.size());
    }
    factorList_.push_back (result);
  } else {
    delete result;
  }
}



void
VarElim::printActiveFactors (void)
{
  for (size_t i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      cout << factorList_[i]->getLabel() << " " ;
      cout << factorList_[i]->params();
      cout << endl;
    }
  }
}

