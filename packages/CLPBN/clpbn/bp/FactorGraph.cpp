#include <cstdlib>
#include <vector>
#include <set>

#include <iostream>
#include <fstream>
#include <sstream>

#include "FactorGraph.h"
#include "FgVarNode.h"
#include "Factor.h"
#include "BayesNet.h"


FactorGraph::FactorGraph (const char* fileName)
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
  int nVars;
  is >> nVars;

  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  vector<int> domainSizes (nVars);
  for (int i = 0; i < nVars; i++) {
    int ds;
    is >> ds;
    domainSizes[i] = ds;
  }

  while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
  for (int i = 0; i < nVars; i++) {
    addVariable (new FgVarNode (i, domainSizes[i]));
  }

  int nFactors;
  is >> nFactors;
  for (int i = 0; i < nFactors; i++) {
    while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
    int nFactorVars;
    is >> nFactorVars;
    FgVarSet factorVars;
    for (int j = 0; j < nFactorVars; j++) {
      int vid;
      is >> vid;
      FgVarNode* var = getFgVarNode (vid);
      if (!var) {
        cerr << "error: invalid variable identifier (" << vid << ")" << endl;
        abort();
      }
      factorVars.push_back (var);
    }
    Factor* f = new Factor (factorVars);
    factors_.push_back (f);
    for (unsigned j = 0; j < factorVars.size(); j++) {
      factorVars[j]->addFactor (f);
    }
  }

  for (int i = 0; i < nFactors; i++) {
    while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
    int nParams;
    is >> nParams;
    ParamSet params (nParams);
    for (int j = 0; j < nParams; j++) {
      double param;
      is >> param;
      params[j] = param;
    }
    factors_[i]->setParameters (params);
  }
  is.close();

  for (unsigned i = 0; i < varNodes_.size(); i++) {
    varNodes_[i]->setIndex (i);
  }
}



FactorGraph::FactorGraph (const BayesNet& bn)
{
  const BnNodeSet& nodes = bn.getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    FgVarNode* varNode = new FgVarNode (nodes[i]);
    varNode->setIndex (i);
    addVariable (varNode);
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    const BnNodeSet& parents = nodes[i]->getParents();
    if (!(nodes[i]->hasEvidence() && parents.size() == 0)) {
      FgVarSet factorVars = { varNodes_[nodes[i]->getIndex()] };
      for (unsigned j = 0; j < parents.size(); j++) {
        factorVars.push_back (varNodes_[parents[j]->getIndex()]);
      }
      Factor* f = new Factor (factorVars, nodes[i]->getDistribution());
      factors_.push_back (f);
      for (unsigned j = 0; j < factorVars.size(); j++) {
        factorVars[j]->addFactor (f);
      }
    }
  }
}



FactorGraph::~FactorGraph (void)
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    delete varNodes_[i];
  }
  for (unsigned i = 0; i < factors_.size(); i++) {
    delete factors_[i];
  }
}



void
FactorGraph::addVariable (FgVarNode* varNode)
{
  varNodes_.push_back (varNode);
  varNode->setIndex (varNodes_.size() - 1);
  indexMap_.insert (make_pair (varNode->getVarId(), varNodes_.size() - 1));
}



void
FactorGraph::removeVariable (const FgVarNode* var)
{
  if (varNodes_[varNodes_.size() - 1] == var) {
    varNodes_.pop_back();
  } else {
    for (unsigned i = 0; i  < varNodes_.size(); i++) {
      if (varNodes_[i] == var) {
        varNodes_.erase (varNodes_.begin() + i);
        return;
      }
    }
    assert (false);
  }
  indexMap_.erase (indexMap_.find (var->getVarId()));
}



void
FactorGraph::addFactor (Factor* f)
{
  factors_.push_back (f);
  const FgVarSet& factorVars = f->getFgVarNodes();
  for (unsigned i = 0; i < factorVars.size(); i++) {
    factorVars[i]->addFactor (f);
  }
}



void
FactorGraph::removeFactor (const Factor* f)
{
  const FgVarSet& factorVars = f->getFgVarNodes();
  for (unsigned i = 0; i < factorVars.size(); i++) {
    if (factorVars[i]) {
      factorVars[i]->removeFactor (f);
    }
  }
  if (factors_[factors_.size() - 1] == f) {
    factors_.pop_back();
  } else {
    for (unsigned i = 0; i  < factors_.size(); i++) {
      if (factors_[i] == f) {
        factors_.erase (factors_.begin() + i);
        return;
      }
    }
    assert (false);
  }
}



VarSet
FactorGraph::getVariables (void) const
{
  VarSet vars;
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    vars.push_back (varNodes_[i]);
  }
  return vars;
}



Variable*
FactorGraph::getVariable (Vid vid) const
{
  return getFgVarNode (vid);
}



void
FactorGraph::setIndexes (void)
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    varNodes_[i]->setIndex (i);
  }
  for (unsigned i = 0; i < factors_.size(); i++) {
    factors_[i]->setIndex (i);
  }
}



void
FactorGraph::freeDistributions (void)
{
  set<Distribution*> dists;
  for (unsigned i = 0; i < factors_.size(); i++) {
    dists.insert (factors_[i]->getDistribution());
  }
  for (set<Distribution*>::iterator it = dists.begin();
      it != dists.end(); it++) {
    delete *it;
  }
}



void
FactorGraph::printGraphicalModel (void) const
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    cout << "variable number " << varNodes_[i]->getIndex() << endl;
    cout << "Id          = "   << varNodes_[i]->getVarId() << endl;
    cout << "Label       = "   << varNodes_[i]->getLabel() << endl;
    cout << "Domain size = "   << varNodes_[i]->getDomainSize() << endl;
    cout << "Evidence    = "   << varNodes_[i]->getEvidence() << endl;
    cout << "Factors     = " ;
    for (unsigned j = 0; j < varNodes_[i]->getFactors().size(); j++) {
      cout << varNodes_[i]->getFactors()[j]->getLabel() << " " ;
    }
    cout << endl << endl;
  }
  for (unsigned i = 0; i < factors_.size(); i++) {
    factors_[i]->printFactor();
    cout << endl;
  }
}



void
FactorGraph::exportToDotFormat (const char* fileName) const
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
      out << '"' << varNodes_[i]->getLabel() << '"' ;
      out << " [style=filled, fillcolor=yellow]" << endl;
    }
  }

  for (unsigned i = 0; i < factors_.size(); i++) {
    out << '"' << factors_[i]->getLabel() << '"' ;
    out << " [label=\"" << factors_[i]->getLabel() << "\\n(";
    out << factors_[i]->getDistribution()->id << ")" << "\"" ;
    out << ", shape=box]" << endl;
  }

  for (unsigned i = 0; i < factors_.size(); i++) {
    CFgVarSet myVars = factors_[i]->getFgVarNodes();
    for (unsigned j = 0; j < myVars.size(); j++) {
      out << '"' << factors_[i]->getLabel() << '"' ;
      out << " -- " ;
      out << '"' << myVars[j]->getLabel() << '"' << endl;
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
    out << varNodes_[i]->getDomainSize() << " " ;
  }
  out << endl;

  out << factors_.size() << endl;
  for (unsigned i = 0; i < factors_.size(); i++) {
    CFgVarSet factorVars = factors_[i]->getFgVarNodes();
    out << factorVars.size();
    for (unsigned j = 0; j < factorVars.size(); j++) {
      out << " " << factorVars[j]->getIndex();
    }
    out << endl;
  }

  for (unsigned i = 0; i < factors_.size(); i++) {
    CParamSet params = factors_[i]->getParameters();
    out << endl << params.size() << endl << " " ;
    for (unsigned j = 0; j < params.size(); j++) {
      out << params[j] << " " ;
    }
    out << endl;
  }

  out.close();
}

