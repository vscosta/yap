#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstdlib>

#include "FactorGraph.h"
#include "FgVarNode.h"
#include "Factor.h"


FactorGraph::FactorGraph (const char* fileName)
{
  string line;
  ifstream is (fileName);
  if (!is.is_open()) {
    cerr << "error: cannot read from file " + std::string (fileName) << endl;
    abort();
  }

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
    varNodes_.push_back (new FgVarNode (i, domainSizes[i]));
  }

  int nFactors;
  is >> nFactors;
  for (int i = 0; i < nFactors; i++) {
    while (is.peek() == '#' || is.peek() == '\n') getline (is, line);
    int nFactorVars;
    is >> nFactorVars;
    FgVarSet factorVars;
    for (int j = 0; j < nFactorVars; j++) {
      int varId;
      is >> varId;
      FgVarNode* var = getVariableById (varId);
      if (var == 0) {
        cerr << "error: invalid variable identifier (" << varId << ")" << endl;
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



FactorGraph::~FactorGraph (void)
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    delete varNodes_[i];
  }
  for (unsigned i = 0; i < factors_.size(); i++) {
    delete factors_[i];
  }
}



FgVarSet
FactorGraph::getFgVarNodes (void) const
{
  return varNodes_;
}



vector<Factor*>
FactorGraph::getFactors (void) const
{
  return factors_;
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



FgVarNode*
FactorGraph::getVariableById (unsigned id) const
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    if (varNodes_[i]->getVarId() == id) {
      return varNodes_[i];
    }
  }
  return 0;
}



FgVarNode*
FactorGraph::getVariableByLabel (string label) const
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    stringstream ss;
    ss << "v" << varNodes_[i]->getVarId();
    if (ss.str() == label) {
      return varNodes_[i];
    }
  }
  return 0;
}



void
FactorGraph::printFactorGraph (void) const
{
  for (unsigned i = 0; i < varNodes_.size(); i++) {
    cout << "variable number " << varNodes_[i]->getIndex() << endl;
    cout << "Id          = "   << varNodes_[i]->getVarId() << endl;
    cout << "Domain size = "   << varNodes_[i]->getDomainSize() << endl;
    cout << "Evidence    = "   << varNodes_[i]->getEvidence() << endl;
    cout << endl;
  }
  cout << endl;
  for (unsigned i = 0; i < factors_.size(); i++) {
    cout << factors_[i]->toString() << endl;
  }
}

