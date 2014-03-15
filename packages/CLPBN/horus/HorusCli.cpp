#include <cassert>

#include <string>
#include <iostream>

#include "FactorGraph.h"
#include "VarElim.h"
#include "BeliefProp.h"
#include "CountingBp.h"


namespace {

int readHorusFlags (int, const char* []);

void readFactorGraph (Horus::FactorGraph&, const char*);

Horus::VarIds readQueryAndEvidence (
    Horus::FactorGraph&, int, const char* [], int);

void runSolver (const Horus::FactorGraph&, const Horus::VarIds&);

const std::string usage = "usage: ./hcli [solver=hve|bp|cbp] \
[<OPTION>=<VALUE>]... <FILE> [<VAR>|<VAR>=<EVIDENCE>]... " ;

}



int
main (int argc, const char* argv[])
{
  if (argc <= 1) {
    std::cerr << "Error: no probabilistic graphical model was given." ;
    std::cerr << std::endl << usage << std::endl;
    exit (EXIT_FAILURE);
  }
  int idx = readHorusFlags (argc, argv);
  Horus::FactorGraph fg;
  readFactorGraph (fg, argv[idx]);
  Horus::VarIds queryIds
      = readQueryAndEvidence (fg, argc, argv, idx + 1);
  if (Horus::FactorGraph::exportToLibDai()) {
    fg.exportToLibDai ("model.fg");
  }
  if (Horus::FactorGraph::exportToUai()) {
    fg.exportToUai ("model.uai");
  }
  if (Horus::FactorGraph::exportGraphViz()) {
    fg.exportToGraphViz ("model.dot");
  }
  if (Horus::FactorGraph::printFactorGraph()) {
    fg.print();
  }
  if (Horus::Globals::verbosity > 0) {
    std::cout << "factor graph contains " ;
    std::cout << fg.nrVarNodes() << " variables and " ;
    std::cout << fg.nrFacNodes() << " factors " << std::endl;
  }
  runSolver (fg, queryIds);
  return 0;
}



namespace {

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
      std::cerr << usage << std::endl;
      exit (EXIT_FAILURE);
    }
    if (rightArg.empty()) {
      std::cerr << "Error: missing right argument." << std::endl;
      std::cerr << usage << std::endl;
      exit (EXIT_FAILURE);
    }
    Horus::Util::setHorusFlag (leftArg, rightArg);
  }
  return i + 1;
}



void
readFactorGraph (Horus::FactorGraph& fg, const char* s)
{
  std::string fileName (s);
  std::string extension = fileName.substr (fileName.find_last_of ('.') + 1);
  if (extension == "uai") {
    fg = Horus::FactorGraph::readFromUaiFormat (fileName.c_str());
  } else if (extension == "fg") {
    fg = Horus::FactorGraph::readFromLibDaiFormat (fileName.c_str());
  } else {
    std::cerr << "Error: the probabilistic graphical model must be " ;
    std::cerr << "defined either in a UAI or libDAI file." << std::endl;
    exit (EXIT_FAILURE);
  }
}



Horus::VarIds
readQueryAndEvidence (
    Horus::FactorGraph& fg,
    int argc,
    const char* argv[],
    int start)
{
  Horus::VarIds queryIds;
  for (int i = start; i < argc; i++) {
    const std::string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (Horus::Util::isInteger (arg) == false) {
        std::cerr << "Error: `" << arg << "' " ;
        std::cerr << "is not a variable id." ;
        std::cerr << std::endl;
        exit (EXIT_FAILURE);
      }
      Horus::VarId vid = Horus::Util::stringToUnsigned (arg);
      Horus::VarNode* queryVar = fg.getVarNode (vid);
      if (queryVar == nullptr) {
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
        std::cerr << usage << std::endl;
        exit (EXIT_FAILURE);
      }
      if (Horus::Util::isInteger (leftArg) == false) {
        std::cerr << "Error: `" << leftArg << "' " ;
        std::cerr << "is not a variable id." << std::endl;
        exit (EXIT_FAILURE);
      }
      Horus::VarId vid = Horus::Util::stringToUnsigned (leftArg);
      Horus::VarNode* observedVar = fg.getVarNode (vid);
      if (observedVar == nullptr) {
        std::cerr << "Error: unknow variable with id " ;
        std::cerr << "`" << vid << "'."  << std::endl;
        exit (EXIT_FAILURE);
      }
      if (rightArg.empty()) {
        std::cerr << "Error: missing right argument." << std::endl;
        std::cerr << usage << std::endl;
        exit (EXIT_FAILURE);
      }
      if (Horus::Util::isInteger (rightArg) == false) {
        std::cerr << "Error: `" << rightArg << "' " ;
        std::cerr << "is not a state index." << std::endl;
        exit (EXIT_FAILURE);
      }
      unsigned stateIdx = Horus::Util::stringToUnsigned (rightArg);
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
    const Horus::FactorGraph& fg,
    const Horus::VarIds& queryIds)
{
  Horus::GroundSolver* solver = 0;
  switch (Horus::Globals::groundSolver) {
    case Horus::GroundSolverType::veSolver:
      solver = new Horus::VarElim (fg);
      break;
    case Horus::GroundSolverType::bpSolver:
      solver = new Horus::BeliefProp (fg);
      break;
    case Horus::GroundSolverType::CbpSolver:
      solver = new Horus::CountingBp (fg);
      break;
    default:
      assert (false);
  }
  if (Horus::Globals::verbosity > 0) {
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

}

