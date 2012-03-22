#include <cstdlib>

#include <iostream>
#include <sstream>

#include "BayesNet.h"
#include "FactorGraph.h"
#include "VarElimSolver.h"
#include "BnBpSolver.h"
#include "FgBpSolver.h"
#include "CbpSolver.h"

//#include "TinySet.h"
#include "LiftedUtils.h"


using namespace std;

void processArguments (BayesNet&, int, const char* []);
void processArguments (FactorGraph&, int, const char* []);
void runSolver (Solver*, const VarNodes&);

const string USAGE = "usage: \
./hcli  FILE  [VARIABLE | OBSERVED_VARIABLE=EVIDENCE]..." ;


class Cenas
{
  public:
    Cenas (int cc)
    {
      c = cc;
    }
    //operator int (void) const
    //{ 
    //  cout << "return int" << endl;
    //  return c;
    //}
    operator double (void) const
    {
      cout << "return double" << endl;
      return 0.0;
    }
  private:
    int c;
};


int
main (int argc, const char* argv[])
{
  LogVar X = 3;
  LogVarSet Xs = X;
  cout << "set: " << X << endl;
  Cenas c1 (1);
  Cenas c2 (3);
  cout << (c1 < c2) << endl;
  return 0;
  if (!argv[1]) {
    cerr << "error: no graphical model specified" << endl;
    cerr << USAGE << endl;
    exit (0);
  }
  const string& fileName  = argv[1];
  const string& extension = fileName.substr (fileName.find_last_of ('.') + 1);
  if (extension == "xml") {
    BayesNet bn;
    bn.readFromBifFormat (argv[1]);
    processArguments (bn, argc, argv);
  } else if (extension == "uai") {
    FactorGraph fg;
    fg.readFromUaiFormat (argv[1]);
    processArguments (fg, argc, argv);
  } else if (extension == "fg") {
    FactorGraph fg;
    fg.readFromLibDaiFormat (argv[1]);
    processArguments (fg, argc, argv);
  } else {
    cerr << "error: the graphical model must be defined either " ; 
    cerr << "in a xml, uai or libDAI file" << endl;
    exit (0);
  }
  return 0;
}



void
processArguments (BayesNet& bn, int argc, const char* argv[])
{
  VarNodes queryVars;
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
        bn.freeDistributions();
        exit (0);
      }
    } else {
      size_t pos = arg.find ('=');
      const string& label = arg.substr (0, pos);
      const string& state = arg.substr (pos + 1);
      if (label.empty()) {
        cerr << "error: missing left argument" << endl;
        cerr << USAGE << endl;
        bn.freeDistributions();
        exit (0);
      }
      if (state.empty()) {
        cerr << "error: missing right argument" << endl;
        cerr << USAGE << endl;
        bn.freeDistributions();
        exit (0);
      }
      BayesNode* node = bn.getBayesNode (label);
      if (node) {
        if (node->isValidState (state)) {
          node->setEvidence (state);
        } else {
          cerr << "error: `" << state << "' " ;
          cerr << "is not a valid state for " ;
          cerr << "`" << node->label() << "'" ;
          cerr << endl;
          bn.freeDistributions();
          exit (0);
        }
      } else {
        cerr << "error: there isn't a variable labeled of " ;
        cerr << "`" << label << "'" ;
        cerr << endl;
        bn.freeDistributions();
        exit (0);
      }
    }
  }

  Solver* solver = 0;
  FactorGraph* fg = 0;
  switch (InfAlgorithms::infAlgorithm) {
    case InfAlgorithms::VE:
      fg = new FactorGraph (bn);
      solver = new VarElimSolver (*fg);
      break;
    case InfAlgorithms::BN_BP:
      solver = new BnBpSolver (bn);
      break;
    case InfAlgorithms::FG_BP:
      fg = new FactorGraph (bn);
      solver = new FgBpSolver (*fg);
      break;
    case InfAlgorithms::CBP:
      fg = new FactorGraph (bn);
      solver = new CbpSolver (*fg);
      break;
    default:
      assert (false);
  }
  runSolver (solver, queryVars);
  delete fg;
  bn.freeDistributions();
}



void
processArguments (FactorGraph& fg, int argc, const char* argv[])
{
  VarNodes queryVars;
  for (int i = 2; i < argc; i++) {
    const string& arg = argv[i];
    if (arg.find ('=') == std::string::npos) {
      if (!Util::isInteger (arg)) {
        cerr << "error: `" << arg << "' " ;
        cerr << "is not a valid variable id" ;
        cerr << endl;
        fg.freeDistributions();
        exit (0);
      }
      VarId vid;
      stringstream ss;
      ss << arg;
      ss >> vid;
      VarNode* queryVar = fg.getFgVarNode (vid);
      if (queryVar) {
        queryVars.push_back (queryVar);
      } else {
        cerr << "error: there isn't a variable with " ;
        cerr << "`" << vid << "' as id" ;
        cerr << endl;
        fg.freeDistributions();
        exit (0);
      }
    } else {
      size_t pos = arg.find ('=');
      if (arg.substr (0, pos).empty()) {
        cerr << "error: missing left argument" << endl;
        cerr << USAGE << endl;
        fg.freeDistributions();
        exit (0);
      }
      if (arg.substr (pos + 1).empty()) {
        cerr << "error: missing right argument" << endl;
        cerr << USAGE << endl;
        fg.freeDistributions();
        exit (0);
      }
      if (!Util::isInteger (arg.substr (0, pos))) {
        cerr << "error: `" << arg.substr (0, pos) << "' " ;
        cerr << "is not a variable id" ;
        cerr << endl;
        fg.freeDistributions();
        exit (0);
      }
      VarId vid;
      stringstream ss; 
      ss << arg.substr (0, pos);
      ss >> vid;
      VarNode* var = fg.getFgVarNode (vid);
      if (var) {
        if (!Util::isInteger (arg.substr (pos + 1))) {
          cerr << "error: `" << arg.substr (pos + 1) << "' " ;
          cerr << "is not a state index" ;
          cerr << endl;
          fg.freeDistributions();
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
          fg.freeDistributions();
          exit (0);
        }
      } else {
        cerr << "error: there isn't a variable with " ;
        cerr << "`" << vid << "' as id" ;
        cerr << endl;
        fg.freeDistributions();
        exit (0);
      }
    }
  }
  Solver* solver = 0;
  switch (InfAlgorithms::infAlgorithm) {
    case InfAlgorithms::VE:
      solver = new VarElimSolver (fg);
      break;
    case InfAlgorithms::BN_BP:
    case InfAlgorithms::FG_BP:
      //cout << "here!" << endl;
      //fg.printGraphicalModel();
      //fg.exportToLibDaiFormat ("net.fg");
      solver = new FgBpSolver (fg);
      break;
    case InfAlgorithms::CBP:
      solver = new CbpSolver (fg);
      break;
    default:
      assert (false);
  }
  runSolver (solver, queryVars);
  fg.freeDistributions();
}



void
runSolver (Solver* solver, const VarNodes& queryVars)
{
  VarIds vids;
  for (unsigned i = 0; i < queryVars.size(); i++) {
    vids.push_back (queryVars[i]->varId());
  }
  if (queryVars.size() == 0) {
    solver->runSolver();
    solver->printAllPosterioris();
  } else if (queryVars.size() == 1) {
    solver->runSolver();
    solver->printPosterioriOf (vids[0]);
  } else {
    solver->runSolver();
    solver->printJointDistributionOf (vids);
  }
  delete solver;
}

