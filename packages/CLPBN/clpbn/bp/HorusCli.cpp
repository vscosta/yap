#include <cstdlib>

#include <iostream>
#include <sstream>

#include "BayesNet.h"
#include "FactorGraph.h"
#include "SPSolver.h"
#include "BPSolver.h"
#include "CountingBP.h"

using namespace std;

void BayesianNetwork (int, const char* []);
void markovNetwork (int, const char* []);
void runSolver (Solver*, const VarSet&);

const string USAGE = "usage: \
./hcli  FILE  [VARIABLE | OBSERVED_VARIABLE=EVIDENCE]..." ;


int
main (int argc, const char* argv[])
{ 
  /*
  FactorGraph fg;
  FgVarNode* varNode1 = new FgVarNode (0, 2);
  FgVarNode* varNode2 = new FgVarNode (1, 2);
  FgVarNode* varNode3 = new FgVarNode (2, 2);
  fg.addVariable (varNode1);
  fg.addVariable (varNode2);
  fg.addVariable (varNode3);
  Distribution* dist = new Distribution (ParamSet() = {1.2, 1.4, 2.0, 0.4});
  fg.addFactor (new Factor (FgVarSet() = {varNode1, varNode2}, dist));
  fg.addFactor (new Factor (FgVarSet() = {varNode3, varNode2}, dist));
  //fg.printGraphicalModel();
  //SPSolver sp (fg);
  //sp.runSolver();
  //sp.printAllPosterioris();
  //ParamSet p = sp.getJointDistributionOf (VidSet() = {0, 1, 2});
  //cout << Util::parametersToString (p) << endl;
  CountingBP cbp (fg);
  //cbp.runSolver();
  //cbp.printAllPosterioris();
  ParamSet p2 = cbp.getJointDistributionOf (VidSet() = {0, 1, 2});
  cout << Util::parametersToString (p2) << endl;
  fg.freeDistributions();
  Statistics::printCompressingStats ("compressing.stats");
  return 0;
  */
  if (!argv[1]) {
    cerr << "error: no graphical model specified" << endl;
    cerr << USAGE << endl;
    exit (0);
  }
  const string& fileName  = argv[1];
  const string& extension = fileName.substr (fileName.find_last_of ('.') + 1);
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
  //bn.printGraphicalModel();

  VarSet queryVars;
  for (int i = 2; i < argc; i++) {
    const string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      BayesNode* queryVar = bn.getBayesNode (arg);
      if (queryVar) {
        queryVars.push_back (queryVar);
      } else {
        cerr << "error: there isn't a variable labeled of " ;
        cerr << "`" << arg << "'" ;
        cerr << endl;
        exit (0);
      }
    } else {
      size_t pos = arg.find ('=');
      const string& label = arg.substr (0, pos);
      const string& state = arg.substr (pos + 1);
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
      BayesNode* node = bn.getBayesNode (label);
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

  Solver* solver;
  if (SolverOptions::convertBn2Fg) {
    FactorGraph* fg = new FactorGraph (bn);
    fg->printGraphicalModel();
    solver = new SPSolver (*fg);
    runSolver (solver, queryVars);
    delete fg;
  } else {
    solver = new BPSolver (bn);
    runSolver (solver, queryVars);
  }
  bn.freeDistributions();
}



void
markovNetwork (int argc, const char* argv[])
{
  FactorGraph fg (argv[1]);
  //fg.printGraphicalModel();
  
  VarSet queryVars;
  for (int i = 2; i < argc; i++) {
    const string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (!Util::isInteger (arg)) {
        cerr << "error: `" << arg << "' " ;
        cerr << "is not a valid variable id" ;
        cerr << endl;
        exit (0);
      }
      Vid vid;
      stringstream ss;
      ss << arg;
      ss >> vid;
      Variable* queryVar = fg.getFgVarNode (vid);
      if (queryVar) {
        queryVars.push_back (queryVar);
      } else {
        cerr << "error: there isn't a variable with " ;
        cerr << "`" << vid << "' as id" ;
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
      Vid vid;
      stringstream ss; 
      ss << arg.substr (0, pos);
      ss >> vid;
      Variable* var = fg.getFgVarNode (vid);
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
        cerr << "`" << vid << "' as id" ;
        cerr << endl;
        exit (0);
      }
    }
  }
  Solver* solver = new SPSolver (fg);
  runSolver (solver, queryVars);
  fg.freeDistributions();
}



void
runSolver (Solver* solver, const VarSet& queryVars)
{
  VidSet vids;
  for (unsigned i = 0; i < queryVars.size(); i++) {
    vids.push_back (queryVars[i]->getVarId());
  }
  if (queryVars.size() == 0) {
    solver->runSolver();
    solver->printAllPosterioris();
  } else if (queryVars.size() == 1) {
    solver->runSolver();
    solver->printPosterioriOf (vids[0]);
  } else {
    solver->printJointDistributionOf (vids);
  }
  delete solver;
}

