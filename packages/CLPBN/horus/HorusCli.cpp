#include <cstdlib>

#include <iostream>
#include <sstream>

#include "FactorGraph.h"
#include "VarElim.h"
#include "BeliefProp.h"
#include "CountingBp.h"

using namespace std;

int readHorusFlags (int, const char* []);
void readFactorGraph (FactorGraph&, const char*);
VarIds readQueryAndEvidence (FactorGraph&, int, const char* [], int);

void runSolver (const FactorGraph&, const VarIds&);

const string USAGE = "usage: ./hcli \
[<KEY>=<VALUE>]... <FILE> [<VAR>|<VAR>=<EVIDENCE>]..." ;


int
main (int argc, const char* argv[])
{
  if (argc <= 1) {
    cerr << "Error: no probabilistic graphical model was given." << endl;
    cerr << USAGE << endl;
    exit (EXIT_FAILURE);
  }
  int idx = readHorusFlags (argc, argv);
  FactorGraph fg;
  readFactorGraph (fg, argv[idx]);
  VarIds queryIds = readQueryAndEvidence (fg, argc, argv, idx + 1);
  runSolver (fg, queryIds);
  return 0;
}



int
readHorusFlags (int argc, const char* argv[])
{
  int i = 1;
  for (; i < argc; i++) {
    const string& arg = argv[i];
    size_t pos = arg.find ('=');
    if (pos == std::string::npos) {
      return i;
    }
    string leftArg  = arg.substr (0, pos);
    string rightArg = arg.substr (pos + 1);
    if (leftArg.empty()) {
      cerr << "Error: missing left argument." << endl;
      cerr << USAGE << endl;
      exit (EXIT_FAILURE);
    }
    if (rightArg.empty()) {
      cerr << "Error: missing right argument." << endl;
      cerr << USAGE << endl;
      exit (EXIT_FAILURE);
    }
    Util::setHorusFlag (leftArg, rightArg);
  }
  return i + 1;
}



void
readFactorGraph (FactorGraph& fg, const char* s)
{
  string fileName (s);
  string extension = fileName.substr (fileName.find_last_of ('.') + 1);
  if (extension == "uai") {
    fg.readFromUaiFormat (fileName.c_str());
  } else if (extension == "fg") {
    fg.readFromLibDaiFormat (fileName.c_str());
  } else {
    cerr << "Error: the probabilistic graphical model must be " ;
    cerr << "defined either in a UAI or libDAI file." << endl;
    exit (EXIT_FAILURE);
  }
}



VarIds
readQueryAndEvidence (
    FactorGraph& fg,
    int argc,
    const char* argv[],
    int start)
{
  VarIds queryIds;
  for (int i = start; i < argc; i++) {
    const string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (Util::isInteger (arg) == false) {
        cerr << "Error: `" << arg << "' " ;
        cerr << "is not a variable id." ;
        cerr << endl;
        exit (EXIT_FAILURE);
      }
      VarId vid = Util::stringToUnsigned (arg);
      VarNode* queryVar = fg.getVarNode (vid);
      if (queryVar == false) {
        cerr << "Error: unknow variable with id " ;
        cerr << "`" << vid << "'."  << endl;
        exit (EXIT_FAILURE);
      }
      queryIds.push_back (vid);
    } else {
      size_t pos = arg.find ('=');
      string leftArg  = arg.substr (0, pos);
      string rightArg = arg.substr (pos + 1);
      if (leftArg.empty()) {
        cerr << "Error: missing left argument." << endl;
        cerr << USAGE << endl;
        exit (EXIT_FAILURE);
      }
      if (Util::isInteger (leftArg) == false) {
        cerr << "Error: `" << leftArg << "' " ;
        cerr << "is not a variable id." << endl ;
        exit (EXIT_FAILURE);
      }
      VarId vid = Util::stringToUnsigned (leftArg);
      VarNode* observedVar = fg.getVarNode (vid);
      if (observedVar == false) {
        cerr << "Error: unknow variable with id " ;
        cerr << "`" << vid << "'."  << endl;
        exit (EXIT_FAILURE);
      }
      if (rightArg.empty()) {
        cerr << "Error: missing right argument." << endl;
        cerr << USAGE << endl;
        exit (EXIT_FAILURE);
      }
      if (Util::isInteger (rightArg) == false) {
        cerr << "Error: `" << rightArg << "' " ;
        cerr << "is not a state index." << endl ;
        exit (EXIT_FAILURE);
      }
      unsigned stateIdx = Util::stringToUnsigned (rightArg);
      if (observedVar->isValidState (stateIdx) == false) {
        cerr << "Error: `" << stateIdx << "' " ;
        cerr << "is not a valid state index for variable with id " ;
        cerr << "`" << vid << "'."  << endl;
        exit (EXIT_FAILURE);
      }
      observedVar->setEvidence (stateIdx);
    }
  }
  return queryIds;
}



void
runSolver (const FactorGraph& fg, const VarIds& queryIds)
{
  GroundSolver* solver = 0;
  switch (Globals::groundSolver) {
    case GroundSolverType::VE:
      solver = new VarElim (fg);
      break;
    case GroundSolverType::BP:
      solver = new BeliefProp (fg);
      break;
    case GroundSolverType::CBP:
      solver = new CountingBp (fg);
      break;
    default:
      assert (false);
  }
  if (Globals::verbosity > 0) {
    solver->printSolverFlags();
    cout << endl;
  }
  if (queryIds.empty()) {
    solver->printAllPosterioris();
  } else {
    solver->printAnswer (queryIds);
  }
  delete solver;
}

