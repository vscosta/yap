#include <cstdlib>

#include <iostream>
#include <sstream>

#include "FactorGraph.h"
#include "VarElimSolver.h"
#include "BpSolver.h"
#include "CbpSolver.h"

using namespace std;

void processArguments (FactorGraph&, int, const char* []);
void runSolver (const FactorGraph&, const VarIds&);

const string USAGE = "usage: \
./hcli ve|bp|cbp NETWORK_FILE [VARIABLE | OBSERVED_VARIABLE=EVIDENCE]..." ;


int
main (int argc, const char* argv[])
{
  if (argc <= 1) {
    cerr << "error: no solver specified" << endl;
    cerr << "error: no graphical model specified" << endl;
    cerr << USAGE << endl;
    exit (0);
  }
  if (argc <= 2) {
    cerr << "error: no graphical model specified" << endl;
    cerr << USAGE << endl;
    exit (0);
  }
  string solver (argv[1]);
  if (solver == "ve") {
    Globals::infAlgorithm = InfAlgorithms::VE;
  } else if (solver == "bp") {
    Globals::infAlgorithm = InfAlgorithms::BP;
  } else if (solver == "cbp") {
    Globals::infAlgorithm = InfAlgorithms::CBP;
  } else {
    cerr << "error: unknow solver `" << solver << "'" << endl ;
    cerr << USAGE << endl;
    exit(0);
  }
  string fileName (argv[2]);
  string extension = fileName.substr (
      fileName.find_last_of ('.') + 1);
  FactorGraph fg;
  if (extension == "uai") {
    fg.readFromUaiFormat (fileName.c_str());
  } else if (extension == "fg") {
    fg.readFromLibDaiFormat (fileName.c_str());
  } else {
    cerr << "error: the graphical model must be defined either " ; 
    cerr << "in a UAI or libDAI file" << endl;
    exit (0);
  }
  processArguments (fg, argc, argv);
  return 0;
}



void
processArguments (FactorGraph& fg, int argc, const char* argv[])
{
  VarIds queryIds;
  for (int i = 3; i < argc; i++) {
    const string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (!Util::isInteger (arg)) {
        cerr << "error: `" << arg << "' " ;
        cerr << "is not a valid variable id" ;
        cerr << endl;
        exit (0);
      }
      VarId vid;
      stringstream ss;
      ss << arg;
      ss >> vid;
      VarNode* queryVar = fg.getVarNode (vid);
      if (queryVar) {
        queryIds.push_back (vid);
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
      VarId vid;
      stringstream ss; 
      ss << arg.substr (0, pos);
      ss >> vid;
      VarNode* var = fg.getVarNode (vid);
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
        if (var->isValidState (stateIndex)) {
          var->setEvidence (stateIndex);
        } else {
          cerr << "error: `" << stateIndex << "' " ;
          cerr << "is not a valid state index for variable " ;
          cerr << "`" << var->varId() << "'" ;
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
  runSolver (fg, queryIds);
}



void
runSolver (const FactorGraph& fg, const VarIds& queryIds)
{
  Solver* solver = 0;
  switch (Globals::infAlgorithm) {
    case InfAlgorithms::VE:
      solver = new VarElimSolver (fg);
      break;
    case InfAlgorithms::BP:
      solver = new BpSolver (fg);
      break;
    case InfAlgorithms::CBP:
      solver = new CbpSolver (fg);
      break;
    default:
      assert (false);
  }
  if (queryIds.size() == 0) {
    solver->printAllPosterioris();
  } else {
    solver->printAnswer (queryIds);
  }
  delete solver;
}

