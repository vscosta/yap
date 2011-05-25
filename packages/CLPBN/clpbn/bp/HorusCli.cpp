#include <iostream>
#include <cstdlib>
#include <sstream>

#include "BayesNet.h"
#include "BPSolver.h"

#include "FactorGraph.h"
#include "SPSolver.h"

using namespace std;

void BayesianNetwork (int, const char* []);
void markovNetwork (int, const char* []);

const string USAGE = "usage: \
./hcli  FILE  [VARIABLE | OBSERVED_VARIABLE=EVIDENCE]..." ;


int
main (int argc, const char* argv[])
{
  if (!argv[1]) {
    cerr << "error: no graphical model specified" << endl;
    cerr << USAGE << endl;
    exit (0);
  }
  string fileName  = argv[1];
  string extension = fileName.substr (fileName.find_last_of ('.') + 1);
  if (extension == "xml") {
    BayesianNetwork (argc, argv);
  } else if (extension == "uai") {
    markovNetwork (argc, argv);
  } else {
    cerr << "error: the graphical model must be defined either " ; 
    cerr << "in a xml file or uai file" << endl;
    exit (0);
  }
  return 0;
}



void
BayesianNetwork (int argc, const char* argv[])
{
  BayesNet bn (argv[1]);
  //bn.printNetwork();

  NodeSet queryVars;
  for (int i = 2; i < argc; i++) {
    string arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      BayesNode* queryVar = bn.getNode (arg);
      if (queryVar) {
        queryVars.push_back (queryVar);
      } else {
        cerr << "error: there isn't a variable labeled of " ;
        cerr << "`" << arg << "'" ;
        cerr << endl;
        exit (0);
      }
    } else {
      size_t pos   = arg.find ('=');
      string label = arg.substr (0, pos);
      string state = arg.substr (pos + 1);
      if (label.empty()) {
        cerr << "error: missing left argument" << endl;
        cerr << USAGE << endl;
        exit (0);
      }
      if (state.empty()) {
        cerr << "error: missing right argument" << endl;
        cerr << USAGE << endl;
        exit (0);
      }
      BayesNode* node = bn.getNode (label);
      if (node) {
        if (node->isValidState (state)) {
          node->setEvidence (state);
        } else {
          cerr << "error: `" << state << "' " ;
          cerr << "is not a valid state for " ;
          cerr << "`" << node->getLabel() << "'" ;
          cerr << endl;
          exit (0);
        }
      } else {
        cerr << "error: there isn't a variable labeled of " ;
        cerr << "`" << label << "'" ;
        cerr << endl;
        exit (0);
      }
    }
  }

  BPSolver solver (bn);
  if (queryVars.size() == 0) {
    solver.runSolver();
    solver.printAllPosterioris();
  } else if (queryVars.size() == 1) {
    solver.runSolver();
    solver.printPosterioriOf (queryVars[0]);
  } else {
    Domain domain   = BayesNet::getInstantiations(queryVars);
    ParamSet params = solver.getJointDistribution (queryVars);
    for (unsigned i = 0; i < params.size(); i++) {
      cout << domain[i] << "\t" << params[i] << endl;
    }
  }
  bn.freeDistributions();
}



void
markovNetwork (int argc, const char* argv[])
{
  FactorGraph fg (argv[1]);
  //fg.printFactorGraph();

  VarSet queryVars;
  for (int i = 2; i < argc; i++) {
    string arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (!Util::isInteger (arg)) {
        cerr << "error: `" << arg << "' " ;
        cerr << "is not a valid variable id" ;
        cerr << endl;
        exit (0);
      }
      unsigned varId;
      stringstream ss;
      ss << arg;
      ss >> varId;
      Variable* queryVar = fg.getVariableById (varId);
      if (queryVar) {
        queryVars.push_back (queryVar);
      } else {
        cerr << "error: there isn't a variable with " ;
        cerr << "`" << varId << "' as id" ;
        cerr << endl;
        exit (0);
      }
    } else {
      size_t pos = arg.find ('=');
      if (arg.substr (0, pos).empty()) {
        cerr << "error: missing left argument" << endl;
        cerr << USAGE << endl;
        exit (0);
      }
      if (arg.substr (pos + 1).empty()) {
        cerr << "error: missing right argument" << endl;
        cerr << USAGE << endl;
        exit (0);
      }
      if (!Util::isInteger (arg.substr (0, pos))) {
        cerr << "error: `" << arg.substr (0, pos) << "' " ;
        cerr << "is not a variable id" ;
        cerr << endl;
        exit (0);
      }
      unsigned varId;
      stringstream ss; 
      ss << arg.substr (0, pos);
      ss >> varId;
      Variable* var = fg.getVariableById (varId);
      if (var) {
        if (!Util::isInteger (arg.substr (pos + 1))) {
          cerr << "error: `" << arg.substr (pos + 1) << "' " ;
          cerr << "is not a state index" ;
          cerr << endl;
          exit (0);
        }
        int stateIndex;
        stringstream ss; 
        ss << arg.substr (pos + 1);
        ss >> stateIndex;
        cout << "si: " << stateIndex << endl;
        if (var->isValidStateIndex (stateIndex)) {
          var->setEvidence (stateIndex);
        } else {
          cerr << "error: `" << stateIndex << "' " ;
          cerr << "is not a valid state index for variable " ;
          cerr << "`" << var->getVarId() << "'" ;
          cerr << endl;
          exit (0);
        }
      } else {
        cerr << "error: there isn't a variable with " ;
        cerr << "`" << varId << "' as id" ;
        cerr << endl;
        exit (0);
      }
    }
  }

  SPSolver solver (fg);
  if (queryVars.size() == 0) {
    solver.runSolver();
    solver.printAllPosterioris();
  } else if (queryVars.size() == 1) {
    solver.runSolver();
    solver.printPosterioriOf (queryVars[0]);
  } else {
    assert (false); //FIXME
    //Domain domain   = BayesNet::getInstantiations(queryVars);
    //ParamSet params = solver.getJointDistribution (queryVars);
    //for (unsigned i = 0; i < params.size(); i++) {
    //  cout << domain[i] << "\t" << params[i] << endl;
    //}
  }
}

