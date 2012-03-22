#include <cstdlib>

#include <vector>

#include <iostream>
#include <sstream>

#include <YapInterface.h>

#include "BayesNet.h"
#include "FactorGraph.h"
#include "VarElimSolver.h"
#include "BnBpSolver.h"
#include "FgBpSolver.h"
#include "CbpSolver.h"
#include "ElimGraph.h"
#include "FoveSolver.h"
#include "ParfactorList.h"


using namespace std;



Params readParams (YAP_Term);


int createLiftedNetwork (void)
{
  Parfactors parfactors;
  YAP_Term parfactorList = YAP_ARG1;
  while (parfactorList != YAP_TermNil()) {
    YAP_Term parfactor = YAP_HeadOfTerm (parfactorList);

    // read dist id
    unsigned distId = YAP_IntOfTerm (YAP_ArgOfTerm (1, parfactor));

    // read the ranges
    Ranges ranges;
    YAP_Term rangeList = YAP_ArgOfTerm (3, parfactor);
    while (rangeList != YAP_TermNil()) {
      unsigned range = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (rangeList));
      ranges.push_back (range);
      rangeList = YAP_TailOfTerm (rangeList);
    }

    // read parametric random vars
    ProbFormulas formulas;
    unsigned count = 0;
    unordered_map<YAP_Term, LogVar> lvMap;
    YAP_Term pvList = YAP_ArgOfTerm (2, parfactor);
    while (pvList != YAP_TermNil()) {
      YAP_Term formulaTerm = YAP_HeadOfTerm (pvList);
      if (YAP_IsAtomTerm (formulaTerm)) {
        string name ((char*) YAP_AtomName (YAP_AtomOfTerm (formulaTerm)));
        Symbol functor = LiftedUtils::getSymbol (name);
        formulas.push_back (ProbFormula (functor, ranges[count]));
      } else {
        LogVars logVars;
        YAP_Functor yapFunctor = YAP_FunctorOfTerm (formulaTerm);
        string name ((char*) YAP_AtomName (YAP_NameOfFunctor (yapFunctor)));
        Symbol functor = LiftedUtils::getSymbol (name);
        unsigned arity = (unsigned) YAP_ArityOfFunctor (yapFunctor);
        for (unsigned i = 1; i <= arity; i++) {
          YAP_Term ti = YAP_ArgOfTerm (i, formulaTerm);
          unordered_map<YAP_Term, LogVar>::iterator it = lvMap.find (ti);
          if (it != lvMap.end()) {
            logVars.push_back (it->second);
          } else {
            unsigned newLv = lvMap.size();
            lvMap[ti] = newLv;
            logVars.push_back (newLv);
          }
        }
        formulas.push_back (ProbFormula (functor, logVars, ranges[count]));
      }
      count ++;
      pvList = YAP_TailOfTerm (pvList);
    }

    // read the parameters
    const Params& params = readParams (YAP_ArgOfTerm (4, parfactor)); 

    // read the constraint
    Tuples tuples;
    if (lvMap.size() >= 1) {
      YAP_Term tupleList = YAP_ArgOfTerm (5, parfactor);
      while (tupleList != YAP_TermNil()) {
        YAP_Term term = YAP_HeadOfTerm (tupleList);
        assert (YAP_IsApplTerm (term));
        YAP_Functor yapFunctor = YAP_FunctorOfTerm (term);
        unsigned arity = (unsigned) YAP_ArityOfFunctor (yapFunctor);
        assert (lvMap.size() == arity);
        Tuple tuple (arity);
        for (unsigned i = 1; i <= arity; i++) {
          YAP_Term ti = YAP_ArgOfTerm (i, term);
          if (YAP_IsAtomTerm (ti) == false) {
            cerr << "error: bad formed constraint" << endl;
            abort();
          }
          string name ((char*) YAP_AtomName (YAP_AtomOfTerm (ti)));
          tuple[i - 1] = LiftedUtils::getSymbol (name);
        }
        tuples.push_back (tuple);
        tupleList = YAP_TailOfTerm (tupleList);
      }
    }
    parfactors.push_back (new Parfactor (formulas, params, tuples, distId));
	  parfactorList = YAP_TailOfTerm (parfactorList);
  }

  // LiftedUtils::printSymbolDictionary();
  cout << "*******************************************************" << endl;
  cout << "INITIAL PARFACTORS" << endl;
  cout << "*******************************************************" << endl;
  for (unsigned i = 0; i < parfactors.size(); i++) {
    parfactors[i]->print();
    cout << endl;
  }
  ParfactorList* pfList = new ParfactorList();
  for (unsigned i = 0; i < parfactors.size(); i++) {
    pfList->add (parfactors[i]);
  }
  cout << endl;
  cout << "*******************************************************" << endl;
  cout << "SHATTERED PARFACTORS" << endl;
  cout << "*******************************************************" << endl;
  pfList->shatter();
  pfList->print();

  // insert the evidence
  ObservedFormulas obsFormulas;
  YAP_Term observedList = YAP_ARG2;
  while (observedList != YAP_TermNil()) {
    YAP_Term pair = YAP_HeadOfTerm (observedList);
    YAP_Term ground = YAP_ArgOfTerm (1, pair);
    Symbol functor;
    Symbols args;
    if (YAP_IsAtomTerm (ground)) {
      string name ((char*) YAP_AtomName (YAP_AtomOfTerm (ground)));
      functor = LiftedUtils::getSymbol (name);
    } else {
      assert (YAP_IsApplTerm (ground));
      YAP_Functor yapFunctor = YAP_FunctorOfTerm (ground);
      string name ((char*) (YAP_AtomName (YAP_NameOfFunctor (yapFunctor))));
      functor = LiftedUtils::getSymbol (name);
      unsigned arity = (unsigned) YAP_ArityOfFunctor (yapFunctor);
      for (unsigned i = 1; i <= arity; i++) {
        YAP_Term ti = YAP_ArgOfTerm (i, ground);
        assert (YAP_IsAtomTerm (ti));
        string arg ((char *) YAP_AtomName (YAP_AtomOfTerm (ti)));
        args.push_back (LiftedUtils::getSymbol (arg));
      }
    }
    unsigned evidence = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (2, pair));
    bool found = false;
    for (unsigned i = 0; i < obsFormulas.size(); i++) {
      if (obsFormulas[i]->functor()  == functor &&
          obsFormulas[i]->arity()    == args.size() &&
          obsFormulas[i]->evidence() == evidence) {
        obsFormulas[i]->addTuple (args);
        found = true;
      }
    }
    if (found == false) {
      obsFormulas.push_back (new ObservedFormula (functor, evidence, args));
    }
    observedList = YAP_TailOfTerm (observedList);
  }
  FoveSolver::absorveEvidence (*pfList, obsFormulas);

  YAP_Int p = (YAP_Int) (pfList);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG3);
}



int
createGroundNetwork (void)
{
  Statistics::incrementPrimaryNetworksCounting();
  // cout << "creating network number " ;
  // cout << Statistics::getPrimaryNetworksCounting() << endl;
  // if (Statistics::getPrimaryNetworksCounting() > 98) {
  //   Statistics::writeStatisticsToFile ("../../compressing.stats");
  // }
  BayesNet* bn = new BayesNet();
  YAP_Term varList = YAP_ARG1;
  BnNodeSet nodes;
  vector<VarIds> parents;
  while (varList != YAP_TermNil()) {
    YAP_Term var     =   YAP_HeadOfTerm (varList);
    VarId vid        =   (VarId) YAP_IntOfTerm    (YAP_ArgOfTerm (1, var));
    unsigned dsize   =   (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (2, var));
    int evidence     =   (int) YAP_IntOfTerm      (YAP_ArgOfTerm (3, var));
    YAP_Term parentL =                             YAP_ArgOfTerm (4, var);
    unsigned distId  =   (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (5, var));
    parents.push_back (VarIds());
    while (parentL != YAP_TermNil()) {
      unsigned parentId = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (parentL));
      parents.back().push_back (parentId);
      parentL = YAP_TailOfTerm (parentL);
    }
    Distribution* dist = bn->getDistribution (distId);
    if (!dist) {
      dist = new Distribution (distId);
      bn->addDistribution (dist);
    }
    assert (bn->getBayesNode (vid) == 0);
    nodes.push_back (bn->addNode (vid, dsize, evidence, dist));
    varList = YAP_TailOfTerm (varList);
  }
  for (unsigned i = 0; i < nodes.size(); i++) {
    BnNodeSet ps;
    for (unsigned j = 0; j < parents[i].size(); j++) {
      assert (bn->getBayesNode (parents[i][j]) != 0);
      ps.push_back (bn->getBayesNode (parents[i][j]));
    }
    nodes[i]->setParents (ps);
  }
  bn->setIndexes();
  YAP_Int p = (YAP_Int) (bn);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG2);
}



int
setBayesNetParams (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term distList = YAP_ARG2;
  while (distList != YAP_TermNil()) {
    YAP_Term dist    = YAP_HeadOfTerm (distList);
    unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    const Params params = readParams (YAP_ArgOfTerm (2, dist));
    bn->getDistribution(distId)->updateParameters (params);
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



int
setParfactorGraphParams (void)
{
  // FIXME
  // ParfactorGraph* pfg = (ParfactorGraph*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term distList = YAP_ARG2;
  while (distList != YAP_TermNil()) {
    // YAP_Term dist    = YAP_HeadOfTerm (distList);
    // unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    // const Params params = readParams (YAP_ArgOfTerm (2, dist));
    // pfg->getDistribution(distId)->setData (params);
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



Params
readParams (YAP_Term paramL)
{
  Params params;
  while (paramL!= YAP_TermNil()) {
    params.push_back ((double) YAP_FloatOfTerm (YAP_HeadOfTerm (paramL)));
    paramL = YAP_TailOfTerm (paramL);
  }
  if (Globals::logDomain) {
    Util::toLog (params);
  }
  return params;
}



int
runLiftedSolver (void)
{
  ParfactorList* pfList = (ParfactorList*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<Params> results;

  while (taskList != YAP_TermNil()) {
    YAP_Term jointList = YAP_HeadOfTerm (taskList);      
    Grounds queryVars;
    assert (YAP_IsPairTerm (taskList));
    assert (YAP_IsPairTerm (jointList));
    while (jointList != YAP_TermNil()) {
      YAP_Term ground = YAP_HeadOfTerm (jointList);
      if (YAP_IsAtomTerm (ground)) {
        string name ((char*) YAP_AtomName (YAP_AtomOfTerm (ground)));
        queryVars.push_back (Ground (LiftedUtils::getSymbol (name)));
      } else {
        assert (YAP_IsApplTerm (ground));
        YAP_Functor yapFunctor = YAP_FunctorOfTerm (ground);
        string name ((char*) (YAP_AtomName (YAP_NameOfFunctor (yapFunctor))));
        unsigned arity = (unsigned) YAP_ArityOfFunctor (yapFunctor);
        Symbol functor = LiftedUtils::getSymbol (name);
        Symbols args;
        for (unsigned i = 1; i <= arity; i++) {
          YAP_Term ti = YAP_ArgOfTerm (i, ground);
          assert (YAP_IsAtomTerm (ti));
          string arg ((char *) YAP_AtomName (YAP_AtomOfTerm (ti)));
          args.push_back (LiftedUtils::getSymbol (arg));
        }
        queryVars.push_back (Ground (functor, args));
      }
      jointList = YAP_TailOfTerm (jointList);
    }
    FoveSolver solver (pfList);
    if (queryVars.size() == 1) {
      results.push_back (solver.getPosterioriOf (queryVars[0]));
    } else {
      assert (false); // TODO joint dist
    }
    taskList = YAP_TailOfTerm (taskList);
  }

  YAP_Term list = YAP_TermNil();
  for (int i = results.size() - 1; i >= 0; i--) {
    const Params& beliefs = results[i];
    YAP_Term queryBeliefsL = YAP_TermNil();
    for (int j = beliefs.size() - 1; j >= 0; j--) {
      YAP_Int sl1      = YAP_InitSlot (list);
      YAP_Term belief  = YAP_MkFloatTerm (beliefs[j]);
      queryBeliefsL    = YAP_MkPairTerm (belief, queryBeliefsL);
      list             = YAP_GetFromSlot (sl1);
      YAP_RecoverSlots (1);
    }
    list = YAP_MkPairTerm (queryBeliefsL, list);
  }
 
  return YAP_Unify (list, YAP_ARG3);
}



int
runOtherSolvers (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<VarIds> tasks;
  std::set<VarId> vids;
  while (taskList != YAP_TermNil()) {
    if (YAP_IsPairTerm (YAP_HeadOfTerm (taskList))) {
      tasks.push_back (VarIds());
      YAP_Term jointList = YAP_HeadOfTerm (taskList);      
      while (jointList != YAP_TermNil()) {
        VarId vid = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (jointList));
        assert (bn->getBayesNode (vid));
        tasks.back().push_back (vid);
        vids.insert (vid);
        jointList = YAP_TailOfTerm (jointList);
      }
    } else {
      VarId vid = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (taskList));
      assert (bn->getBayesNode (vid));
      tasks.push_back (VarIds() = {vid});
      vids.insert (vid);
    }
    taskList = YAP_TailOfTerm (taskList);
  }
  
  Solver* bpSolver = 0;
  GraphicalModel* graphicalModel = 0;
  CFactorGraph::checkForIdenticalFactors = false;
  if (InfAlgorithms::infAlgorithm != InfAlgorithms::VE) {
    BayesNet* mrn = bn->getMinimalRequesiteNetwork (
        VarIds (vids.begin(), vids.end()));
    if (InfAlgorithms::infAlgorithm == InfAlgorithms::BN_BP) {
      graphicalModel = mrn;
      bpSolver = new BnBpSolver (*static_cast<BayesNet*> (graphicalModel));
    } else if (InfAlgorithms::infAlgorithm == InfAlgorithms::FG_BP) {
      graphicalModel = new FactorGraph (*mrn);
      bpSolver = new FgBpSolver (*static_cast<FactorGraph*> (graphicalModel));
      delete mrn;
    } else if (InfAlgorithms::infAlgorithm == InfAlgorithms::CBP) {
      graphicalModel = new FactorGraph (*mrn);
      bpSolver = new CbpSolver (*static_cast<FactorGraph*> (graphicalModel));
      delete mrn;
    }
    bpSolver->runSolver();
  }

  vector<Params> results;
  results.reserve (tasks.size());
  for (unsigned i = 0; i < tasks.size(); i++) {
    //if (i == 1) exit (0);
    if (InfAlgorithms::infAlgorithm == InfAlgorithms::VE) {
      BayesNet* mrn = bn->getMinimalRequesiteNetwork (tasks[i]);
      VarElimSolver* veSolver = new VarElimSolver (*mrn);
      if (tasks[i].size() == 1) {
        results.push_back (veSolver->getPosterioriOf (tasks[i][0]));
      } else {
        results.push_back (veSolver->getJointDistributionOf (tasks[i]));
      }
      delete mrn;
      delete veSolver;
    } else {
      if (tasks[i].size() == 1) {
        results.push_back (bpSolver->getPosterioriOf (tasks[i][0]));
      } else {
        results.push_back (bpSolver->getJointDistributionOf (tasks[i]));
      }
    }
  }
  delete bpSolver;
  delete graphicalModel;

  YAP_Term list = YAP_TermNil();
  for (int i = results.size() - 1; i >= 0; i--) {
    const Params& beliefs = results[i];
    YAP_Term queryBeliefsL = YAP_TermNil();
    for (int j = beliefs.size() - 1; j >= 0; j--) {
      YAP_Int sl1      = YAP_InitSlot (list);
      YAP_Term belief  = YAP_MkFloatTerm (beliefs[j]);
      queryBeliefsL    = YAP_MkPairTerm (belief, queryBeliefsL);
      list             = YAP_GetFromSlot (sl1);
      YAP_RecoverSlots (1);
    }
    list = YAP_MkPairTerm (queryBeliefsL, list);
  }
 
  return YAP_Unify (list, YAP_ARG3);
}



int
setExtraVarsInfo (void)
{
  // BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  GraphicalModel::clearVariablesInformation();
  YAP_Term varsInfoL =  YAP_ARG2;
  while (varsInfoL != YAP_TermNil()) {
    YAP_Term head    = YAP_HeadOfTerm (varsInfoL);
    VarId vid          = YAP_IntOfTerm  (YAP_ArgOfTerm (1, head));
    YAP_Atom label   = YAP_AtomOfTerm (YAP_ArgOfTerm (2, head));
    YAP_Term statesL = YAP_ArgOfTerm (3, head);
    States states;
    while (statesL != YAP_TermNil()) {
      YAP_Atom atom = YAP_AtomOfTerm (YAP_HeadOfTerm (statesL));
      states.push_back ((char*) YAP_AtomName (atom));
      statesL = YAP_TailOfTerm (statesL);
    }
    GraphicalModel::addVariableInformation (vid,
        (char*) YAP_AtomName (label), states);
    varsInfoL = YAP_TailOfTerm (varsInfoL);
  }
  return TRUE;
}



int
setHorusFlag (void)
{
  string key ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG1)));
  if (key == "inf_alg") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (       value == "ve") {
      InfAlgorithms::infAlgorithm = InfAlgorithms::VE;
    } else if (value == "bn_bp") {
      InfAlgorithms::infAlgorithm = InfAlgorithms::BN_BP;
    } else if (value == "fg_bp") {
      InfAlgorithms::infAlgorithm = InfAlgorithms::FG_BP;
    } else if (value == "cbp") {
      InfAlgorithms::infAlgorithm = InfAlgorithms::CBP;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      return FALSE;
    }
  } else if (key == "elim_heuristic") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (       value == "min_neighbors") {
      ElimGraph::setEliminationHeuristic (ElimHeuristic::MIN_NEIGHBORS);
    } else if (value == "min_weight") {
      ElimGraph::setEliminationHeuristic (ElimHeuristic::MIN_WEIGHT);
    } else if (value == "min_fill") {
      ElimGraph::setEliminationHeuristic (ElimHeuristic::MIN_FILL);
    } else if (value == "weighted_min_fill") {
      ElimGraph::setEliminationHeuristic (ElimHeuristic::WEIGHTED_MIN_FILL);
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      return FALSE;
    }
  } else if (key == "schedule") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (       value == "seq_fixed") {
      BpOptions::schedule = BpOptions::Schedule::SEQ_FIXED;
    } else if (value == "seq_random") {
      BpOptions::schedule = BpOptions::Schedule::SEQ_RANDOM;
    } else if (value == "parallel") {
      BpOptions::schedule = BpOptions::Schedule::PARALLEL;
    } else if (value == "max_residual") {
      BpOptions::schedule = BpOptions::Schedule::MAX_RESIDUAL;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      return FALSE;
    }
  } else if (key == "accuracy") {
    BpOptions::accuracy = (double) YAP_FloatOfTerm (YAP_ARG2);
  } else if (key == "max_iter") {
    BpOptions::maxIter = (int) YAP_IntOfTerm (YAP_ARG2);
  } else if (key == "use_logarithms") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (       value == "true") {
      Globals::logDomain = true;
    } else if (value == "false") {
      Globals::logDomain = false;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      return FALSE;
    }
  } else if (key == "order_factor_variables") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (       value == "true") {
      FactorGraph::orderFactorVariables = true;
    } else if (value == "false") {
      FactorGraph::orderFactorVariables = false;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      return FALSE;
    }
  } else {
    cerr << "warning: invalid key `" << key << "'" << endl;
    return FALSE;
  }
  return TRUE;
}



int
freeBayesNetwork (void)
{
  //Statistics::writeStatisticsToFile ("stats.txt");
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  bn->freeDistributions();
  delete bn;
  return TRUE;
}



int
freeParfactorGraph (void)
{
  delete (ParfactorList*) YAP_IntOfTerm (YAP_ARG1);
  return TRUE;
}



extern "C" void
init_predicates (void)
{
  YAP_UserCPredicate ("create_lifted_network",      createLiftedNetwork,     3);
  YAP_UserCPredicate ("create_ground_network",      createGroundNetwork,     2);
  YAP_UserCPredicate ("set_parfactor_graph_params", setParfactorGraphParams, 2);
  YAP_UserCPredicate ("set_bayes_net_params",       setBayesNetParams,       2);
  YAP_UserCPredicate ("run_lifted_solver",          runLiftedSolver,         3);
  YAP_UserCPredicate ("run_other_solvers",          runOtherSolvers,         3);
  YAP_UserCPredicate ("set_extra_vars_info",        setExtraVarsInfo,        2);
  YAP_UserCPredicate ("set_horus_flag",             setHorusFlag,            2);
  YAP_UserCPredicate ("free_bayesian_network",      freeBayesNetwork,        1);
  YAP_UserCPredicate ("free_parfactor_graph",       freeParfactorGraph,      1);
}

