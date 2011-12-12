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

using namespace std;


int
createNetwork (void)
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
  vector<VarIdSet> parents;
  while (varList != YAP_TermNil()) {
    YAP_Term var     =   YAP_HeadOfTerm (varList);
    VarId vid          =   (VarId) YAP_IntOfTerm        (YAP_ArgOfTerm (1, var));
    unsigned dsize   =   (unsigned) YAP_IntOfTerm   (YAP_ArgOfTerm (2, var));
    int evidence     =   (int) YAP_IntOfTerm        (YAP_ArgOfTerm (3, var));
    YAP_Term parentL =                               YAP_ArgOfTerm (4, var);
    unsigned distId  =   (unsigned) YAP_IntOfTerm   (YAP_ArgOfTerm (5, var));
    parents.push_back (VarIdSet());
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
setParameters (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term distList = YAP_ARG2;
  while (distList != YAP_TermNil()) {
    YAP_Term dist    = YAP_HeadOfTerm (distList);
    unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    YAP_Term paramL  =                           YAP_ArgOfTerm (2, dist);
    ParamSet params;
    while (paramL!= YAP_TermNil()) {
      params.push_back ((double) YAP_FloatOfTerm (YAP_HeadOfTerm (paramL)));
      paramL = YAP_TailOfTerm (paramL);
    }
    if (NSPACE == NumberSpace::LOGARITHM) {
       Util::toLog (params);
    }
    bn->getDistribution(distId)->updateParameters (params);
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



int
runSolver (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<VarIdSet> tasks;
  std::set<VarId> vids;
  while (taskList != YAP_TermNil()) {
    if (YAP_IsPairTerm (YAP_HeadOfTerm (taskList))) {
      tasks.push_back (VarIdSet());
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
      tasks.push_back (VarIdSet() = {vid});
      vids.insert (vid);
    }
    taskList = YAP_TailOfTerm (taskList);
  }
  
  Solver* bpSolver = 0;
  GraphicalModel* graphicalModel = 0;
  CFactorGraph::disableCheckForIdenticalFactors();
  if (InfAlgorithms::infAlgorithm != InfAlgorithms::VE) {
    BayesNet* mrn = bn->getMinimalRequesiteNetwork (
        VarIdSet (vids.begin(), vids.end()));
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

  vector<ParamSet> results;
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
    const ParamSet& beliefs = results[i];
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
setSolverParameter (void)
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
  } else if (key == "always_loopy_solver") {
    string value ((char*) YAP_AtomName (YAP_AtomOfTerm (YAP_ARG2)));
    if (value == "true") {
      BpOptions::useAlwaysLoopySolver = true;
    } else if (value == "false") {
      BpOptions::useAlwaysLoopySolver = false;
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



int useLogSpace (void)
{
  NSPACE = NumberSpace::LOGARITHM;
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



extern "C" void
init_predicates (void)
{
  YAP_UserCPredicate ("create_network",        createNetwork,      2);
  YAP_UserCPredicate ("set_extra_vars_info",   setExtraVarsInfo,   2);
  YAP_UserCPredicate ("set_parameters",        setParameters,      2);
  YAP_UserCPredicate ("run_solver",            runSolver,          3);
  YAP_UserCPredicate ("set_solver_parameter",  setSolverParameter, 2);
  YAP_UserCPredicate ("use_log_space",         useLogSpace,        0);
  YAP_UserCPredicate ("free_bayesian_network", freeBayesNetwork,   1);
}

