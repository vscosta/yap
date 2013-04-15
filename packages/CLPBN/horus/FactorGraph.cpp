#include <cassert>

#include <algorithm>
#include <iostream>

#include "FactorGraph.h"
#include "BayesBall.h"
#include "Util.h"


namespace Horus {

bool FactorGraph::exportLd_  = false;
bool FactorGraph::exportUai_ = false;
bool FactorGraph::exportGv_  = false;
bool FactorGraph::printFg_   = false;


FactorGraph::FactorGraph (const FactorGraph& fg)
{
  clone (fg);
}



FactorGraph::~FactorGraph()
{
  for (size_t i = 0; i < varNodes_.size(); i++) {
    delete varNodes_[i];
  }
  for (size_t i = 0; i < facNodes_.size(); i++) {
    delete facNodes_[i];
  }
}



void
FactorGraph::addFactor (const Factor& factor)
{
  FacNode* fn = new FacNode (factor);
  addFacNode (fn);
  const VarIds& vids = fn->factor().arguments();
  for (size_t i = 0; i < vids.size(); i++) {
    VarMap::const_iterator it = varMap_.find (vids[i]);
    if (it != varMap_.end()) {
      addEdge (it->second, fn);
    } else {
      VarNode* vn = new VarNode (vids[i], fn->factor().range (i));
      addVarNode (vn);
      addEdge (vn, fn);
    }
  }
}



void
FactorGraph::addVarNode (VarNode* vn)
{
  varNodes_.push_back (vn);
  vn->setIndex (varNodes_.size() - 1);
  varMap_.insert (std::make_pair (vn->varId(), vn));
}



void
FactorGraph::addFacNode (FacNode* fn)
{
  facNodes_.push_back (fn);
  fn->setIndex (facNodes_.size() - 1);
}



void
FactorGraph::addEdge (VarNode* vn, FacNode* fn)
{
  vn->addNeighbor (fn);
  fn->addNeighbor (vn);
}



bool
FactorGraph::isTree() const
{
  return !containsCycle();
}



BayesBallGraph&
FactorGraph::getStructure()
{
  assert (bayesFactors_);
  if (structure_.empty()) {
    for (size_t i = 0; i < varNodes_.size(); i++) {
      structure_.addNode (new BBNode (varNodes_[i]));
    }
    for (size_t i = 0; i < facNodes_.size(); i++) {
      const VarIds& vids = facNodes_[i]->factor().arguments();
      for (size_t j = 1; j < vids.size(); j++) {
        structure_.addEdge (vids[j], vids[0]);
      }
    }
  }
  return structure_;
}



void
FactorGraph::print() const
{
  using std::cout;
  using std::endl;
  for (size_t i = 0; i < varNodes_.size(); i++) {
    cout << "var id   = " << varNodes_[i]->varId() << endl;
    cout << "label    = " << varNodes_[i]->label() << endl;
    cout << "range    = " << varNodes_[i]->range() << endl;
    cout << "evidence = " << varNodes_[i]->getEvidence() << endl;
    cout << "factors  = " ;
    for (size_t j = 0; j < varNodes_[i]->neighbors().size(); j++) {
      cout << varNodes_[i]->neighbors()[j]->getLabel() << " " ;
    }
    cout << endl << endl;
  }
  for (size_t i = 0; i < facNodes_.size(); i++) {
    facNodes_[i]->factor().print();
  }
}



void
FactorGraph::exportToLibDai (const char* fileName) const
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << facNodes_.size() << std::endl << std::endl;
  for (size_t i = 0; i < facNodes_.size(); i++) {
    Factor f (facNodes_[i]->factor());
    out << f.nrArguments() << std::endl;
    out << Util::elementsToString (f.arguments()) << std::endl;
    out << Util::elementsToString (f.ranges()) << std::endl;
    VarIds args = f.arguments();
    std::reverse (args.begin(), args.end());
    f.reorderArguments (args);
    if (Globals::logDomain) {
      Util::exp (f.params());
    }
    out << f.size() << std::endl;
    for (size_t j = 0; j < f.size(); j++) {
      out << j << " " << f[j] << std::endl;
    }
    out << std::endl;
  }
  out.close();
}



void
FactorGraph::exportToUai (const char* fileName) const
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << (bayesFactors_ ? "BAYES" : "MARKOV") ;
  out << std::endl << std::endl;
  out << varNodes_.size() << std::endl;
  VarNodes sortedVns = varNodes_;
  std::sort (sortedVns.begin(), sortedVns.end(), sortByVarId());
  for (size_t i = 0; i < sortedVns.size(); i++) {
    out << ((i != 0) ? " " : "") << sortedVns[i]->range();
  }
  out << std::endl << facNodes_.size() << std::endl;
  for (size_t i = 0; i < facNodes_.size(); i++) {
    VarIds args = facNodes_[i]->factor().arguments();
    if (bayesFactors_) {
      std::swap (args.front(), args.back());
    }
    out << args.size() << " " << Util::elementsToString (args);
    out << std::endl;
  }
  out << std::endl;
  for (size_t i = 0; i < facNodes_.size(); i++) {
    Factor f = facNodes_[i]->factor();
    if (bayesFactors_) {
      VarIds args = f.arguments();
      std::swap (args.front(), args.back());
      f.reorderArguments (args);
    }
    Params params = f.params();
    if (Globals::logDomain) {
      Util::exp (params);
    }
    out << params.size() << std::endl << " " ;
    out << Util::elementsToString (params);
    out << std::endl << std::endl;
  }
  out.close();
}



void
FactorGraph::exportToGraphViz (const char* fileName) const
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << "graph \"" << fileName << "\" {" << std::endl;
  for (size_t i = 0; i < varNodes_.size(); i++) {
    if (varNodes_[i]->hasEvidence()) {
      out << '"' << varNodes_[i]->label() << '"' ;
      out << " [style=filled, fillcolor=yellow]" << std::endl;
    }
  }
  for (size_t i = 0; i < facNodes_.size(); i++) {
    out << '"' << facNodes_[i]->getLabel() << '"' ;
    out << " [label=\"" << facNodes_[i]->getLabel();
    out << "\"" << ", shape=box]" << std::endl;
  }
  for (size_t i = 0; i < facNodes_.size(); i++) {
    const VarNodes& myVars = facNodes_[i]->neighbors();
    for (size_t j = 0; j < myVars.size(); j++) {
      out << '"' << facNodes_[i]->getLabel() << '"' ;
      out << " -- " ;
      out << '"' << myVars[j]->label() << '"' << std::endl;
    }
  }
  out << "}" << std::endl;
  out.close();
}



FactorGraph&
FactorGraph::operator= (const FactorGraph& fg)
{
  if (this != &fg) {
    for (size_t i = 0; i < varNodes_.size(); i++) {
      delete varNodes_[i];
    }
    varNodes_.clear();
    for (size_t i = 0; i < facNodes_.size(); i++) {
      delete facNodes_[i];
    }
    facNodes_.clear();
    varMap_.clear();
    clone (fg);
  }
  return *this;
}



FactorGraph
FactorGraph::readFromUaiFormat (const char* fileName)
{
  std::ifstream is (fileName);
  if (!is.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    exit (EXIT_FAILURE);
  }
  FactorGraph fg;
  ignoreLines (is);
  std::string line;
  getline (is, line);
  if (line == "BAYES") {
    fg.bayesFactors_ = true;
  } else if (line == "MARKOV") {
    fg.bayesFactors_ = false;
  } else {
    std::cerr << "Error: the type of network is missing." << std::endl;
    exit (EXIT_FAILURE);
  }
  // read the number of vars
  ignoreLines (is);
  unsigned nrVars;
  is >> nrVars;
  // read the range of each var
  ignoreLines (is);
  Ranges ranges (nrVars);
  for (unsigned i = 0; i < nrVars; i++) {
    is >> ranges[i];
  }
  unsigned nrFactors;
  unsigned nrArgs;
  unsigned vid;
  is >> nrFactors;
  std::vector<VarIds> allVarIds;
  std::vector<Ranges> allRanges;
  for (unsigned i = 0; i < nrFactors; i++) {
    ignoreLines (is);
    is >> nrArgs;
    allVarIds.push_back ({ });
    allRanges.push_back ({ });
    for (unsigned j = 0; j < nrArgs; j++) {
      is >> vid;
      if (vid >= ranges.size()) {
        std::cerr << "Error: invalid variable identifier `" << vid << "'" ;
        std::cerr << ". Identifiers must be between 0 and " ;
        std::cerr << ranges.size() - 1 << "." << std::endl;
        exit (EXIT_FAILURE);
      }
      allVarIds.back().push_back (vid);
      allRanges.back().push_back (ranges[vid]);
    }
  }
  // read the parameters
  unsigned nrParams;
  for (unsigned i = 0; i < nrFactors; i++) {
    ignoreLines (is);
    is >> nrParams;
    if (nrParams != Util::sizeExpected (allRanges[i])) {
      std::cerr << "Error: invalid number of parameters for factor nº " ;
      std::cerr << i << ", " << Util::sizeExpected (allRanges[i]);
      std::cerr << " expected, " << nrParams << " given." << std::endl;
      exit (EXIT_FAILURE);
    }
    Params params (nrParams);
    for (unsigned j = 0; j < nrParams; j++) {
      is >> params[j];
    }
    if (Globals::logDomain) {
      Util::log (params);
    }
    Factor f (allVarIds[i], allRanges[i], params);
    if (fg.bayesFactors_ && allVarIds[i].size() > 1) {
      // In this format the child is the last variable,
      // move it to be the first
      std::swap (allVarIds[i].front(), allVarIds[i].back());
      f.reorderArguments (allVarIds[i]);
    }
    fg.addFactor (f);
  }
  is.close();
  return fg;
}



FactorGraph
FactorGraph::readFromLibDaiFormat (const char* fileName)
{
  std::ifstream is (fileName);
  if (!is.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    exit (EXIT_FAILURE);
  }
  FactorGraph fg;
  ignoreLines (is);
  unsigned nrFactors;
  unsigned nrArgs;
  VarId vid;
  is >> nrFactors;
  for (unsigned i = 0; i < nrFactors; i++) {
    ignoreLines (is);
    // read the factor arguments
    is >> nrArgs;
    VarIds vids;
    for (unsigned j = 0; j < nrArgs; j++) {
      ignoreLines (is);
      is >> vid;
      vids.push_back (vid);
    }
    // read ranges
    Ranges ranges (nrArgs);
    for (unsigned j = 0; j < nrArgs; j++) {
      ignoreLines (is);
      is >> ranges[j];
      VarNode* var = fg.getVarNode (vids[j]);
      if (var && ranges[j] != var->range()) {
        std::cerr << "Error: variable `" << vids[j] << "' appears" ;
        std::cerr << " in two or more factors with a different range." ;
        std::cerr << std::endl;
        exit (EXIT_FAILURE);
      }
    }
    // read parameters
    ignoreLines (is);
    unsigned nNonzeros;
    is >> nNonzeros;
    Params params (Util::sizeExpected (ranges), 0);
    for (unsigned j = 0; j < nNonzeros; j++) {
      ignoreLines (is);
      unsigned index;
      is >> index;
      ignoreLines (is);
      double val;
      is >> val;
      params[index] = val;
    }
    if (Globals::logDomain) {
      Util::log (params);
    }
    std::reverse (vids.begin(), vids.end());
    std::reverse (ranges.begin(), ranges.end());
    Factor f (vids, ranges, params);
    std::reverse (vids.begin(), vids.end());
    f.reorderArguments (vids);
    fg.addFactor (f);
  }
  is.close();
  return fg;
}



void
FactorGraph::clone (const FactorGraph& fg)
{
  const VarNodes& varNodes = fg.varNodes();
  for (size_t i = 0; i < varNodes.size(); i++) {
    addVarNode (new VarNode (varNodes[i]));
  }
  const FacNodes& facNodes = fg.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    FacNode* facNode = new FacNode (facNodes[i]->factor());
    addFacNode (facNode);
    const VarNodes& neighs = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      addEdge (varNodes_[neighs[j]->getIndex()], facNode);
    }
  }
  bayesFactors_ = fg.bayesianFactors();
}



bool
FactorGraph::containsCycle() const
{
  std::vector<bool> visitedVars (varNodes_.size(), false);
  std::vector<bool> visitedFactors (facNodes_.size(), false);
  for (size_t i = 0; i < varNodes_.size(); i++) {
    int v = varNodes_[i]->getIndex();
    if (!visitedVars[v]) {
      if (containsCycle (varNodes_[i], 0, visitedVars, visitedFactors)) {
        return true;
      }
    }
  }
  return false;
}



bool
FactorGraph::containsCycle (
    const VarNode* v,
    const FacNode* p,
    std::vector<bool>& visitedVars,
    std::vector<bool>& visitedFactors) const
{
  visitedVars[v->getIndex()] = true;
  const FacNodes& adjacencies = v->neighbors();
  for (size_t i = 0; i < adjacencies.size(); i++) {
    int w = adjacencies[i]->getIndex();
    if (!visitedFactors[w]) {
      if (containsCycle (adjacencies[i], v, visitedVars, visitedFactors)) {
        return true;
      }
    }
    else if (visitedFactors[w] && adjacencies[i] != p) {
      return true;
    }
  }
  return false; // no cycle detected in this component
}



bool
FactorGraph::containsCycle (
    const FacNode* v,
    const VarNode* p,
    std::vector<bool>& visitedVars,
    std::vector<bool>& visitedFactors) const
{
  visitedFactors[v->getIndex()] = true;
  const VarNodes& adjacencies = v->neighbors();
  for (size_t i = 0; i < adjacencies.size(); i++) {
    int w = adjacencies[i]->getIndex();
    if (!visitedVars[w]) {
      if (containsCycle (adjacencies[i], v, visitedVars, visitedFactors)) {
        return true;
      }
    }
    else if (visitedVars[w] && adjacencies[i] != p) {
      return true;
    }
  }
  return false; // no cycle detected in this component
}



void
FactorGraph::ignoreLines (std::ifstream& is)
{
  std::string ignoreStr;
  while (is.peek() == '#' || is.peek() == '\n') {
    getline (is, ignoreStr);
  }
}

}  // namespace Horus

