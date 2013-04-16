#include <algorithm>
#include <iostream>
#include <sstream>

#include "VarElim.h"
#include "ElimGraph.h"
#include "Factor.h"
#include "Util.h"


namespace Horus {

Params
VarElim::solveQuery (VarIds queryVids)
{
  if (Globals::verbosity > 1) {
    std::cout << "Solving query on " ;
    for (size_t i = 0; i < queryVids.size(); i++) {
      if (i != 0) std::cout << ", " ;
      std::cout << fg.getVarNode (queryVids[i])->label();
    }
    std::cout << std::endl;
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
VarElim::printSolverFlags() const
{
  std::stringstream ss;
  ss << "variable elimination [" ;
  ss << "elim_heuristic=" ;
  typedef ElimGraph::ElimHeuristic ElimHeuristic;
  switch (ElimGraph::elimHeuristic()) {
    case ElimHeuristic::sequentialEh:      ss << "sequential";        break;
    case ElimHeuristic::minNeighborsEh:    ss << "min_neighbors";     break;
    case ElimHeuristic::minWeightEh:       ss << "min_weight";        break;
    case ElimHeuristic::minFillEh:         ss << "min_fill";          break;
    case ElimHeuristic::weightedMinFillEh: ss << "weighted_min_fill"; break;
  }
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  std::cout << ss.str() << std::endl;
}



void
VarElim::createFactorList()
{
  const FacNodes& facNodes = fg.facNodes();
  factorList_.reserve (facNodes.size() * 2);
  for (size_t i = 0; i < facNodes.size(); i++) {
    factorList_.push_back (new Factor (facNodes[i]->factor()));
    const VarIds& args = facNodes[i]->factor().arguments();
    for (size_t j = 0; j < args.size(); j++) {
      std::unordered_map<VarId, std::vector<size_t>>::iterator it;
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
VarElim::absorveEvidence()
{
  if (Globals::verbosity > 2) {
    Util::printDashedLine();
    std::cout << "(initial factor list)" << std::endl;
    printActiveFactors();
  }
  const VarNodes& varNodes = fg.varNodes();
  for (size_t i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      if (Globals::verbosity > 1) {
        std::cout << "-> aborving evidence on ";
        std::cout << varNodes[i]->label() << " = " ;
        std::cout << varNodes[i]->getEvidence() << std::endl;
      }
      const std::vector<size_t>& indices = varMap_[varNodes[i]->varId()];
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
      std::cout << "-> summing out " ;
      std::cout << fg.getVarNode (elimOrder[i])->label() << std::endl;
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
    std::cout << "total factor size:   " << totalFactorSize_   << std::endl;
    std::cout << "largest factor size: " << largestFactorSize_ << std::endl;
    std::cout << std::endl;
  }
  return result.params();
}



void
VarElim::eliminate (VarId vid)
{
  Factor* result = new Factor();
  const std::vector<size_t>& indices = varMap_[vid];
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
      std::vector<size_t>& indices2 = varMap_[args[i]];
      indices2.push_back (factorList_.size());
    }
    factorList_.push_back (result);
  } else {
    delete result;
  }
}



void
VarElim::printActiveFactors()
{
  for (size_t i = 0; i < factorList_.size(); i++) {
    if (factorList_[i]) {
      std::cout << factorList_[i]->getLabel() << " " ;
      std::cout << factorList_[i]->params();
      std::cout << std::endl;
    }
  }
}

}  // namespace Horus

