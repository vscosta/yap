#include <cstdlib>

#include <vector>

#include <iostream>
#include <sstream>

#include <YapInterface.h>

#include "ParfactorList.h"
#include "BayesNet.h"
#include "FactorGraph.h"
#include "FoveSolver.h"
#include "VarElimSolver.h"
#include "BnBpSolver.h"
#include "FgBpSolver.h"
#include "CbpSolver.h"
#include "ElimGraph.h"


using namespace std;


typedef std::pair<ParfactorList*, ObservedFormulas*> LiftedNetwork;


Params readParameters (YAP_Term);

vector<unsigned> readUnsignedList (YAP_Term);

void readLiftedEvidence (YAP_Term, ObservedFormulas&);

Parfactor* readParfactor (YAP_Term);



vector<unsigned>
readUnsignedList (YAP_Term list)
{
  vector<unsigned> vec;
  while (list != YAP_TermNil()) {
    vec.push_back ((unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (list)));
    list = YAP_TailOfTerm (list);
  }
  return vec;
}


int createLiftedNetwork (void)
{
  Parfactors parfactors;
  YAP_Term parfactorList = YAP_ARG1;
  while (parfactorList != YAP_TermNil()) {
    YAP_Term pfTerm = YAP_HeadOfTerm (parfactorList);
    parfactors.push_back (readParfactor (pfTerm));
	  parfactorList = YAP_TailOfTerm (parfactorList);
  }

  // LiftedUtils::printSymbolDictionary();
  if (Constants::DEBUG > 1) {
    // Util::printHeader ("INITIAL PARFACTORS");
    // for (unsigned i = 0; i < parfactors.size(); i++) {
    //  parfactors[i]->print();
    //  cout << endl;
    // }
    // parfactors[0]->countConvert (LogVar (0));
    //parfactors[1]->fullExpand (LogVar (1));
    Util::printHeader ("SHATTERED PARFACTORS");
  }

  ParfactorList* pfList = new ParfactorList (parfactors);

  if (Constants::DEBUG > 1) {
    pfList->print();
  }

  // read evidence
  ObservedFormulas* obsFormulas = new ObservedFormulas();
  readLiftedEvidence (YAP_ARG2, *(obsFormulas));

  LiftedNetwork* net = new LiftedNetwork (pfList, obsFormulas);
  YAP_Int p = (YAP_Int) (net);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG3);
}



Parfactor* readParfactor (YAP_Term pfTerm)
{
  // read dist id
  unsigned distId = YAP_IntOfTerm (YAP_ArgOfTerm (1, pfTerm));

  // read the ranges
  Ranges ranges;
  YAP_Term rangeList = YAP_ArgOfTerm (3, pfTerm);
  while (rangeList != YAP_TermNil()) {
    unsigned range = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (rangeList));
    ranges.push_back (range);
    rangeList = YAP_TailOfTerm (rangeList);
  }

  // read parametric random vars
  ProbFormulas formulas;
  unsigned count = 0;
  unordered_map<YAP_Term, LogVar> lvMap;
  YAP_Term pvList = YAP_ArgOfTerm (2, pfTerm);
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
  const Params& params = readParameters (YAP_ArgOfTerm (4, pfTerm)); 

  // read the constraint
  Tuples tuples;
  if (lvMap.size() >= 1) {
    YAP_Term tupleList = YAP_ArgOfTerm (5, pfTerm);
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
          cerr << "error: constraint has free variables" << endl;
          abort();
        }
        string name ((char*) YAP_AtomName (YAP_AtomOfTerm (ti)));
        tuple[i - 1] = LiftedUtils::getSymbol (name);
      }
      tuples.push_back (tuple);
      tupleList = YAP_TailOfTerm (tupleList);
    }
  }
  return new Parfactor (formulas, params, tuples, distId);
}



void readLiftedEvidence (
    YAP_Term observedList,
    ObservedFormulas& obsFormulas)
{
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
      if (obsFormulas[i].functor()  == functor &&
          obsFormulas[i].arity()    == args.size() &&
          obsFormulas[i].evidence() == evidence) {
        obsFormulas[i].addTuple (args);
        found = true;
      }
    }
    if (found == false) {
      obsFormulas.push_back (ObservedFormula (functor, evidence, args));
    }
    observedList = YAP_TailOfTerm (observedList);
  } 
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
  
  string factorsType ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG1)));
  cout << "factors type: '" << factorsType << "'" << endl;

  YAP_Term factorList = YAP_ARG2;
  while (factorList != YAP_TermNil()) {
    YAP_Term factor =  YAP_HeadOfTerm (factorList);
    // read the var ids
    VarIds varIds = readUnsignedList (YAP_ArgOfTerm (1, factor));
    // read the ranges
    Ranges ranges = readUnsignedList (YAP_ArgOfTerm (2, factor));
    // read the parameters
    Params params = readParameters (YAP_ArgOfTerm (3, factor)); 
    // read dist id
    unsigned distId = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (4, factor));
    factorList = YAP_TailOfTerm (factorList);
    Factor f (varIds, ranges, params, distId);
    f.print();
  }
  assert (false);
  /*
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
    assert (bn->getBayesNode (vid) == 0);
    BayesNode* newNode = new BayesNode (
        vid, dsize, evidence, Params(), distId);
    bn->addNode (newNode);
    varList = YAP_TailOfTerm (varList);
  }
  const BnNodeSet& nodes = bn->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    BnNodeSet ps;
    for (unsigned j = 0; j < parents[i].size(); j++) {
      assert (bn->getBayesNode (parents[i][j]) != 0);
      ps.push_back (bn->getBayesNode (parents[i][j]));
    }
    nodes[i]->setParents (ps);
  }
  bn->setIndexes();
  */
  YAP_Int p = (YAP_Int) (bn);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG2);
}



Params
readParameters (YAP_Term paramL)
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
  LiftedNetwork* network = (LiftedNetwork*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<Params> results;
  ParfactorList pfListCopy (*network->first);
  FoveSolver::absorveEvidence (pfListCopy, *network->second);
  while (taskList != YAP_TermNil()) {
    Grounds queryVars;
    YAP_Term jointList = YAP_HeadOfTerm (taskList);
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
    FoveSolver solver (pfListCopy);
    if (queryVars.size() == 1) {
      results.push_back (solver.getPosterioriOf (queryVars[0]));
    } else {
      results.push_back (solver.getJointDistributionOf (queryVars));
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
runGroundSolver (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<VarIds> tasks;
  std::set<VarId> vids;
  while (taskList != YAP_TermNil()) {
    VarIds queryVars;
    YAP_Term jointList = YAP_HeadOfTerm (taskList);      
    while (jointList != YAP_TermNil()) {
      VarId vid = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (jointList));
      assert (bn->getBayesNode (vid));
      queryVars.push_back (vid);
      vids.insert (vid);
      jointList = YAP_TailOfTerm (jointList);
    }
    tasks.push_back (queryVars);
    taskList = YAP_TailOfTerm (taskList);
  }
  
  Solver* bpSolver = 0;
  GraphicalModel* graphicalModel = 0;
  CFactorGraph::checkForIdenticalFactors = false;
  if (Globals::infAlgorithm != InfAlgorithms::VE) {
    BayesNet* mrn = bn->getMinimalRequesiteNetwork (
        VarIds (vids.begin(), vids.end()));
    if (Globals::infAlgorithm == InfAlgorithms::BN_BP) {
      graphicalModel = mrn;
      bpSolver = new BnBpSolver (*static_cast<BayesNet*> (graphicalModel));
    } else if (Globals::infAlgorithm == InfAlgorithms::FG_BP) {
      graphicalModel = new FactorGraph (*mrn);
      bpSolver = new FgBpSolver (*static_cast<FactorGraph*> (graphicalModel));
      delete mrn;
    } else if (Globals::infAlgorithm == InfAlgorithms::CBP) {
      graphicalModel = new FactorGraph (*mrn);
      bpSolver = new CbpSolver (*static_cast<FactorGraph*> (graphicalModel));
      delete mrn;
    }
    bpSolver->runSolver();
  }

  vector<Params> results;
  results.reserve (tasks.size());
  for (unsigned i = 0; i < tasks.size(); i++) {
    if (Globals::infAlgorithm == InfAlgorithms::VE) {
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
setParfactorsParams (void)
{
  LiftedNetwork* network = (LiftedNetwork*) YAP_IntOfTerm (YAP_ARG1);
  ParfactorList* pfList = network->first;
  YAP_Term distList = YAP_ARG2;
  unordered_map<unsigned, Params> paramsMap;
  while (distList != YAP_TermNil()) {
    YAP_Term dist     = YAP_HeadOfTerm (distList);
    unsigned distId   = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    assert (Util::contains (paramsMap, distId) == false);
    paramsMap[distId] = readParameters (YAP_ArgOfTerm (2, dist));
    distList = YAP_TailOfTerm (distList);
  }
  ParfactorList::iterator it = pfList->begin();
  while (it != pfList->end()) {
    assert (Util::contains (paramsMap, (*it)->distId()));
    // (*it)->setParams (paramsMap[(*it)->distId()]); 
    ++ it;
  }
  return TRUE;
}



int
setBayesNetParams (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term distList = YAP_ARG2;
  unordered_map<unsigned, Params> paramsMap;
  while (distList != YAP_TermNil()) {
    YAP_Term dist     = YAP_HeadOfTerm (distList);
    unsigned distId   = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    assert (Util::contains (paramsMap, distId) == false);
    paramsMap[distId] = readParameters (YAP_ArgOfTerm (2, dist));
    distList = YAP_TailOfTerm (distList);
  }
  const BnNodeSet& nodes = bn->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    assert (Util::contains (paramsMap, nodes[i]->distId()));
    nodes[i]->setParams (paramsMap[nodes[i]->distId()]);
  }
  return TRUE;
}



int
setExtraVarsInfo (void)
{
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
      Globals::infAlgorithm = InfAlgorithms::VE;
    } else if (value == "bn_bp") {
      Globals::infAlgorithm = InfAlgorithms::BN_BP;
    } else if (value == "fg_bp") {
      Globals::infAlgorithm = InfAlgorithms::FG_BP;
    } else if (value == "cbp") {
      Globals::infAlgorithm = InfAlgorithms::CBP;
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
freeGroundNetwork (void)
{
  delete (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  return TRUE;
}



int
freeParfactors (void)
{
  LiftedNetwork* network = (LiftedNetwork*) YAP_IntOfTerm (YAP_ARG1);
  delete network->first;
  delete network->second;
  delete network;
  return TRUE;
}



extern "C" void
init_predicates (void)
{
  YAP_UserCPredicate ("create_lifted_network", createLiftedNetwork, 3);
  YAP_UserCPredicate ("create_ground_network", createGroundNetwork, 3);
  YAP_UserCPredicate ("run_lifted_solver",     runLiftedSolver,     3);
  YAP_UserCPredicate ("run_ground_solver",     runGroundSolver,     3);
  YAP_UserCPredicate ("set_parfactors_params", setParfactorsParams, 2);
  YAP_UserCPredicate ("set_bayes_net_params",  setBayesNetParams,   2);
  YAP_UserCPredicate ("set_extra_vars_info",   setExtraVarsInfo,    2);
  YAP_UserCPredicate ("set_horus_flag",        setHorusFlag,        2);
  YAP_UserCPredicate ("free_parfactors",       freeParfactors,      1);
  YAP_UserCPredicate ("free_ground_network",   freeGroundNetwork,   1);
}

