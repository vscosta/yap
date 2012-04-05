#include <set>
#include <vector>
#include <algorithm>

#include <iostream>
#include <fstream>
#include <sstream>

#include "FactorGraph.h"
#include "Factor.h"
#include "BayesNet.h"
#include "BayesBall.h"
#include "Util.h"


bool FactorGraph::orderFactorVariables = false;


FactorGraph::FactorGraph (const FactorGraph& fg)
{
  const FgVarSet& vars = fg.getVarNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    FgVarNode* varNode = new FgVarNode (vars[i]);
    addVariable (varNode);
  }

  const FgFacSet& facs = fg.getFactorNodes();
  for (unsigned i = 0; i < facs.size(); i++) {
    FgFacNode* facNode = new FgFacNode (facs[i]);
    addFactor (facNode);
    const FgVarSet& neighs = facs[i]->neighbors();
    for (unsigned j = 0; j < neighs.size(); j++) {
      addEdge (facNode, varNodes_[neighs[j]->getIndex()]);
    }
  }
}



FactorGraph::FactorGraph (const BayesNet& bn)
{
  const BnNodeSet& nodes = bn.getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    FgVarNode* varNode = new FgVarNode (nodes[i]);
    addVariable (varNode);
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    const BnNodeSet& parents = nodes[i]->getParents();
    if (!(nodes[i]->hasEvidence() && parents.size() == 0)) {
      VarNodes neighs;
      neighs.push_back (varNodes_[nodes[i]->getIndex()]);
      for (unsigned j = 0; j < parents.size(); j++) {
        neighs.push_back (varNodes_[parents[j]->getIndex()]);
      }
      FgFacNode* fn = new FgFacNode (
          new Factor (neighs, nodes[i]->params(), nodes[i]->distId()));
      if (orderFactorVariables) {
        sort (neighs.begin(), neighs.end(), CompVarId()); 
        fn->factor()->reorderAccordingVarIds();
      }
      addFactor (fn);
      for (unsigned j = 0; j < neighs.size(); j++) {
        addEdge (fn, static_cast<FgVarNode*> (neighs[j]));
      }
    }
  }
  setIndexes();
}



void
FactorGraph::readFromUaiFormat (const char* fileName)
{
   ifstream is (fileName);
  if (!is.is_open()) {
    cerr << "error: cannot read from file " + std::string (fileName) << endl;
    abort();
  }

  string line;
  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  getline (is, line);
  if (line != "MARKOV") {
    cerr << "error: the network must be a MARKOV network " << endl;
    abort();
  }

  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  unsigned nVars;
  is >> nVars;

  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  vector<int> domainSizes (nVars);
  for (unsigned i = 0; i < nVars; i++) {
    unsigned ds;
    is >> ds;
    domainSizes[i] = ds;
  }

  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  for (unsigned i = 0; i < nVars; i++) {
    addVariable (new FgVarNode (i, domainSizes[i]));
  }

  unsigned nFactors;
  is >> nFactors;
  for (unsigned i = 0; i < nFactors; i++) {
    while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
    unsigned nFactorVars;
    is >> nFactorVars;
    VarNodes neighs;
    for (unsigned j = 0; j < nFactorVars; j++) {
      unsigned vid;
      is >> vid;
      FgVarNode* neigh = getFgVarNode (vid);
      if (!neigh) {
        cerr << "error: invalid variable identifier (" << vid << ")" << endl;
        abort();
      }
      neighs.push_back (neigh);
    }
    FgFacNode* fn = new FgFacNode (new Factor (neighs));
    addFactor (fn);
    for (unsigned j = 0; j < neighs.size(); j++) {
      addEdge (fn, static_cast<FgVarNode*> (neighs[j]));
    }
  }

  for (unsigned i = 0; i < nFactors; i++) {
    while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
    unsigned nParams;
    is >> nParams;
    if (facNodes_[i]->params().size() != nParams) {
      cerr << "error: invalid number of parameters for factor " ;
      cerr << facNodes_[i]->getLabel() ;
      cerr << ", expected: " << facNodes_[i]->params().size();
      cerr << ", given: " << nParams << endl;
      abort();
    }
    Params params (nParams);
    for (unsigned j = 0; j < nParams; j++) {
      double param;
      is >> param;
      params[j] = param;
    }
    if (Globals::logDomain) {
      Util::toLog (params);
    }
    facNodes_[i]->factor()->setParams (params);
  }
  is.close();
  setIndexes();
}



void
FactorGraph::readFromLibDaiFormat (const char* fileName)
{
  ifstream is (fileName);
  if (!is.is_open()) {
    cerr << "error: cannot read from file " + std::string (fileName) << endl;
    abort();
  }

  string line;
  unsigned nFactors;

  while ((is.peek()) == '#') getline (is, line);
  is >> nFactors;

  if (is.fail()) {
    cerr << "error: cannot read the number of factors" << endl;
    abort();
  }

  getline (is, line);
  if (is.fail() || line.size() > 0) {
    cerr << "error: cannot read the number of factors" << endl;
    abort();
  }

  for (unsigned i = 0; i < nFactors; i++) {
    unsigned nVars;
    while ((is.peek()) == '#') getline (is, line);

    is >> nVars;
    VarIds vids;
    for (unsigned j = 0; j < nVars; j++) {
      VarId vid;
      while ((is.peek()) == '#') getline (is, line);
      is >> vid;
      vids.push_back (vid);
    }

    VarNodes neighs;
    unsigned nParams = 1;
    for (unsigned j = 0; j < nVars; j++) {
      unsigned dsize;
      while ((is.peek()) == '#') getline (is, line);
      is >> dsize;
      FgVarNode* var = getFgVarNode (vids[j]);
      if (var == 0) {
        var = new FgVarNode (vids[j], dsize);
        addVariable (var);
      } else {
        if (var->range() != dsize) {
          cerr << "error: variable `" << vids[j] << "' appears in two or " ;
          cerr << "more factors with different domain sizes" << endl;
        }
      }
      neighs.push_back (var);
      nParams *= var->range();
    }
    Params params (nParams, 0);
    unsigned nNonzeros;
    while ((is.peek()) == '#')  getline (is, line);
    is >> nNonzeros;

    for (unsigned j = 0; j < nNonzeros; j++) {
      unsigned index;
      double val;
      while ((is.peek()) == '#') getline (is, line);
      is >> index;
      while ((is.peek()) == '#') getline (is, line);
      is >> val;
      params[index] = val;
    }
    reverse (neighs.begin(), neighs.end());
    if (Globals::logDomain) {
      Util::toLog (params);
    }
    FgFacNode* fn = new FgFacNode (new Factor (neighs, params));
    addFactor (fn);
    for (unsigned j = 0; j < neighs.size(); j++) {
      addEdge (fn, static_cast<FgVarNode*> (neighs[j]));
    }
  }
  is.close();
  setIndexes();
}



FactorGraph::~FactorGraph (void)
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    delete varNodes_[i];
  }
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    delete facNodes_[i];
  }
}



void
FactorGraph::addVariable (FgVarNode* vn)
{
  varNodes_.push_back (vn);
  vn->setIndex (varNodes_.size() - 1);
  varMap_.insert (make_pair (vn->varId(), varNodes_.size() - 1));
}



void
FactorGraph::addFactor (FgFacNode* fn)
{
  facNodes_.push_back (fn);
  fn->setIndex (facNodes_.size() - 1);
}



void
FactorGraph::addFactor (const Factor& factor)
{
  FgFacNode* fn = new FgFacNode (factor);
  addFactor (fn);
  const VarIds& vids = factor.arguments();
  for (unsigned i = 0; i < vids.size(); i++) {
    bool found = false;
    for (unsigned j = 0; j < varNodes_.size(); j++) {
      if (varNodes_[j]->varId() == vids[i]) {
        addEdge (varNodes_[j], fn);
        found = true;
      }
    }
    if (found == false) {
      FgVarNode* vn = new FgVarNode (vids[i], factor.range (i));
      addVariable (vn);
      addEdge (vn, fn);
    }
  }
}



void
FactorGraph::addEdge (FgVarNode* vn, FgFacNode* fn)
{
  vn->addNeighbor (fn);
  fn->addNeighbor (vn);
}



void
FactorGraph::addEdge (FgFacNode* fn, FgVarNode* vn)
{
  fn->addNeighbor (vn);
  vn->addNeighbor (fn);
}



VarNode*
FactorGraph::getVariableNode (VarId vid) const
{
  FgVarNode* vn = getFgVarNode (vid);
  assert (vn);
  return vn;
}



VarNodes
FactorGraph::getVariableNodes (void) const
{
  VarNodes vars;
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    vars.push_back (varNodes_[i]);
  }
  return vars;
}



bool
FactorGraph::isTree (void) const
{
  return !containsCycle();
}



DAGraph&
FactorGraph::getStructure (void)
{
  assert (fromBayesNet_);
  if (structure_.empty()) {
    for (unsigned i = 0; i < varNodes_.size(); i++) {
      structure_.addNode (new DAGraphNode (varNodes_[i]));
    }
    for (unsigned i = 0; i < facNodes_.size(); i++) {
      const VarIds& vids = facNodes_[i]->factor()->arguments();
      for (unsigned j = 1; j < vids.size(); j++) {
        structure_.addEdge (vids[j], vids[0]);
      }
    }
  }
  return structure_;
}



void
FactorGraph::setIndexes (void)
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    varNodes_[i]->setIndex (i);
  }
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    facNodes_[i]->setIndex (i);
  }
}



void
FactorGraph::printGraphicalModel (void) const
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    cout << "var id   = " << varNodes_[i]->varId() << endl;
    cout << "label    = " << varNodes_[i]->label() << endl;
    cout << "range    = " << varNodes_[i]->range() << endl;
    cout << "evidence = " << varNodes_[i]->getEvidence() << endl;
    cout << "factors  = " ;
    for (unsigned j = 0; j < varNodes_[i]->neighbors().size(); j++) {
      cout << varNodes_[i]->neighbors()[j]->getLabel() << " " ;
    }
    cout << endl << endl;
  }
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    facNodes_[i]->factor()->print();
  }
}



void
FactorGraph::exportToGraphViz (const char* fileName) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "FactorGraph::exportToDotFile()" << endl;
    abort();
  }
  out << "graph \"" << fileName << "\" {" << endl;
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    if (varNodes_[i]->hasEvidence()) {
      out << '"' << varNodes_[i]->label() << '"' ;
      out << " [style=filled, fillcolor=yellow]" << endl;
    }
  }
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    out << '"' << facNodes_[i]->getLabel() << '"' ;
    out << " [label=\"" << facNodes_[i]->getLabel(); 
    out << "\"" << ", shape=box]" << endl;
  }
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    const FgVarSet& myVars = facNodes_[i]->neighbors();
    for (unsigned j = 0; j < myVars.size(); j++) {
      out << '"' << facNodes_[i]->getLabel() << '"' ;
      out << " -- " ;
      out << '"' << myVars[j]->label() << '"' << endl;
    }
  }
  out << "}" << endl;
  out.close();
}



void
FactorGraph::exportToUaiFormat (const char* fileName) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "FactorGraph::exportToUaiFormat()" << endl;
    abort();
  }

  out << "MARKOV" << endl;
  out << varNodes_.size() << endl;
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    out << varNodes_[i]->range() << " " ;
  }
  out << endl;

  out << facNodes_.size() << endl;
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    const FgVarSet& factorVars = facNodes_[i]->neighbors();
    out << factorVars.size();
    for (unsigned j = 0; j < factorVars.size(); j++) {
      out << " " << factorVars[j]->getIndex();
    }
    out << endl;
  }

  for (unsigned i = 0; i < facNodes_.size(); i++) {
    Params params = facNodes_[i]->params();
    if (Globals::logDomain) {
      Util::fromLog (params);
    }
    out << endl << params.size() << endl << " " ;
    for (unsigned j = 0; j < params.size(); j++) {
      out << params[j] << " " ;
    }
    out << endl;
  }

  out.close();
}



void
FactorGraph::exportToLibDaiFormat (const char* fileName) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "FactorGraph::exportToLibDaiFormat()" << endl;
    abort();
  }
  out << facNodes_.size() << endl << endl;
  for (unsigned i = 0; i < facNodes_.size(); i++) {
    const FgVarSet& factorVars = facNodes_[i]->neighbors();
    out << factorVars.size() << endl;
    for (int j = factorVars.size() - 1; j >= 0; j--) {
      out << factorVars[j]->varId() << " " ;
    }
    out << endl;
    for (unsigned j = 0; j < factorVars.size(); j++) {
      out << factorVars[j]->range() << " " ;
    }
    out << endl;
    Params params = facNodes_[i]->factor()->params();
    if (Globals::logDomain) {
      Util::fromLog (params);
    }
    out << params.size() << endl;
    for (unsigned j = 0; j < params.size(); j++) {
      out << j << " " << params[j] << endl;
    }
    out << endl;
  }
  out.close();
}



bool
FactorGraph::containsCycle (void) const
{
  vector<bool> visitedVars (varNodes_.size(), false);
  vector<bool> visitedFactors (facNodes_.size(), false);
  for (unsigned i = 0; i < varNodes_.size(); i++) {
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
    const FgVarNode* v,
    const FgFacNode* p,
    vector<bool>& visitedVars,
    vector<bool>& visitedFactors) const
{
  visitedVars[v->getIndex()] = true;
  const FgFacSet& adjacencies = v->neighbors();
  for (unsigned i = 0; i < adjacencies.size(); i++) {
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
    const FgFacNode* v,
    const FgVarNode* p,
    vector<bool>& visitedVars,
    vector<bool>& visitedFactors) const
{
  visitedFactors[v->getIndex()] = true;
  const FgVarSet& adjacencies = v->neighbors();
  for (unsigned i = 0; i < adjacencies.size(); i++) {
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

