#include <cassert>

#include <string>
#include <iostream>

#include "FactorGraph.h"
#include "VarElim.h"
#include "BeliefProp.h"
#include "CountingBp.h"


int readHorusFlags (int, const char* []);

void readFactorGraph (horus::FactorGraph&, const char*);

horus::VarIds readQueryAndEvidence (
    horus::FactorGraph&, int, const char* [], int);

void runSolver (const horus::FactorGraph&, const horus::VarIds&);

const std::string USAGE = "usage: ./hcli [solver=hve|bp|cbp] \
[<OPTION>=<VALUE>]... <FILE> [<VAR>|<VAR>=<EVIDENCE>]... " ;


int
main (int argc, const char* argv[])
{
  if (argc <= 1) {
    std::cerr << "Error: no probabilistic graphical model was given." ;
    std::cerr << std::endl << USAGE << std::endl;
    exit (EXIT_FAILURE);
  }
  int idx = readHorusFlags (argc, argv);
  horus::FactorGraph fg;
  readFactorGraph (fg, argv[idx]);
  horus::VarIds queryIds
      = readQueryAndEvidence (fg, argc, argv, idx + 1);
  if (horus::FactorGraph::exportToLibDai()) {
    fg.exportToLibDai ("model.fg");
  }
  if (horus::FactorGraph::exportToUai()) {
    fg.exportToUai ("model.uai");
  }
  if (horus::FactorGraph::exportGraphViz()) {
    fg.exportToGraphViz ("model.dot");
  }
  if (horus::FactorGraph::printFactorGraph()) {
    fg.print();
  }
  if (horus::globals::verbosity > 0) {
    std::cout << "factor graph contains " ;
    std::cout << fg.nrVarNodes() << " variables and " ;
    std::cout << fg.nrFacNodes() << " factors " << std::endl;
  }
  runSolver (fg, queryIds);
  return 0;
}



int
readHorusFlags (int argc, const char* argv[])
{
  int i = 1;
  for (; i < argc; i++) {
    const std::string& arg = argv[i];
    size_t pos = arg.find ('=');
    if (pos == std::string::npos) {
      return i;
    }
    std::string leftArg  = arg.substr (0, pos);
    std::string rightArg = arg.substr (pos + 1);
    if (leftArg.empty()) {
      std::cerr << "Error: missing left argument." << std::endl;
      std::cerr << USAGE << std::endl;
      exit (EXIT_FAILURE);
    }
    if (rightArg.empty()) {
      std::cerr << "Error: missing right argument." << std::endl;
      std::cerr << USAGE << std::endl;
      exit (EXIT_FAILURE);
    }
    horus::util::setHorusFlag (leftArg, rightArg);
  }
  return i + 1;
}



void
readFactorGraph (horus::FactorGraph& fg, const char* s)
{
  std::string fileName (s);
  std::string extension = fileName.substr (fileName.find_last_of ('.') + 1);
  if (extension == "uai") {
    fg.readFromUaiFormat (fileName.c_str());
  } else if (extension == "fg") {
    fg.readFromLibDaiFormat (fileName.c_str());
  } else {
    std::cerr << "Error: the probabilistic graphical model must be " ;
    std::cerr << "defined either in a UAI or libDAI file." << std::endl;
    exit (EXIT_FAILURE);
  }
}



horus::VarIds
readQueryAndEvidence (
    horus::FactorGraph& fg,
    int argc,
    const char* argv[],
    int start)
{
  horus::VarIds queryIds;
  for (int i = start; i < argc; i++) {
    const std::string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (horus::util::isInteger (arg) == false) {
        std::cerr << "Error: `" << arg << "' " ;
        std::cerr << "is not a variable id." ;
        std::cerr << std::endl;
        exit (EXIT_FAILURE);
      }
      horus::VarId vid = horus::util::stringToUnsigned (arg);
      horus::VarNode* queryVar = fg.getVarNode (vid);
      if (queryVar == false) {
        std::cerr << "Error: unknow variable with id " ;
        std::cerr << "`" << vid << "'."  << std::endl;
        exit (EXIT_FAILURE);
      }
      queryIds.push_back (vid);
    } else {
      size_t pos = arg.find ('=');
      std::string leftArg  = arg.substr (0, pos);
      std::string rightArg = arg.substr (pos + 1);
      if (leftArg.empty()) {
        std::cerr << "Error: missing left argument." << std::endl;
        std::cerr << USAGE << std::endl;
        exit (EXIT_FAILURE);
      }
      if (horus::util::isInteger (leftArg) == false) {
        std::cerr << "Error: `" << leftArg << "' " ;
        std::cerr << "is not a variable id." << std::endl;
        exit (EXIT_FAILURE);
      }
      horus::VarId vid = horus::util::stringToUnsigned (leftArg);
      horus::VarNode* observedVar = fg.getVarNode (vid);
      if (observedVar == false) {
        std::cerr << "Error: unknow variable with id " ;
        std::cerr << "`" << vid << "'."  << std::endl;
        exit (EXIT_FAILURE);
      }
      if (rightArg.empty()) {
        std::cerr << "Error: missing right argument." << std::endl;
        std::cerr << USAGE << std::endl;
        exit (EXIT_FAILURE);
      }
      if (horus::util::isInteger (rightArg) == false) {
        std::cerr << "Error: `" << rightArg << "' " ;
        std::cerr << "is not a state index." << std::endl;
        exit (EXIT_FAILURE);
      }
      unsigned stateIdx = horus::util::stringToUnsigned (rightArg);
      if (observedVar->isValidState (stateIdx) == false) {
        std::cerr << "Error: `" << stateIdx << "' " ;
        std::cerr << "is not a valid state index for variable with id " ;
        std::cerr << "`" << vid << "'."  << std::endl;
        exit (EXIT_FAILURE);
      }
      observedVar->setEvidence (stateIdx);
    }
  }
  return queryIds;
}



void
runSolver (
    const horus::FactorGraph& fg,
    const horus::VarIds& queryIds)
{
  horus::GroundSolver* solver = 0;
  switch (horus::globals::groundSolver) {
    case horus::GroundSolverType::VE:
      solver = new horus::VarElim (fg);
      break;
    case horus::GroundSolverType::BP:
      solver = new horus::BeliefProp (fg);
      break;
    case horus::GroundSolverType::CBP:
      solver = new horus::CountingBp (fg);
      break;
    default:
      assert (false);
  }
  if (horus::globals::verbosity > 0) {
    solver->printSolverFlags();
    std::cout << std::endl;
  }
  if (queryIds.empty()) {
    solver->printAllPosterioris();
  } else {
    solver->printAnswer (queryIds);
  }
  delete solver;
}

