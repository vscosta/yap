#include <cstdlib>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>

#include <YapInterface.h>

#include "callgrind.h"

#include "BayesNet.h"
#include "BayesNode.h"
#include "BPSolver.h"

using namespace std;

int
createNetwork (void)
{
  Statistics::numCreatedNets ++;
  cout << "creating network number " << Statistics::numCreatedNets << endl;
  if (Statistics::numCreatedNets == 1) {
    //CALLGRIND_START_INSTRUMENTATION;
  }
  BayesNet* bn = new BayesNet();

  YAP_Term varList = YAP_ARG1;
  while (varList != YAP_TermNil()) {
    YAP_Term var     = YAP_HeadOfTerm (varList);
    unsigned varId   = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, var));
    unsigned dsize   = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (2, var));
    int evidence     = (int)      YAP_IntOfTerm (YAP_ArgOfTerm (3, var));
    YAP_Term parentL =                           YAP_ArgOfTerm (4, var);
    unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (5, var));
    NodeSet parents;
    while (parentL != YAP_TermNil()) {
      unsigned parentId = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (parentL));
      BayesNode* parent = bn->getNode (parentId);
      if (!parent) {
        parent = bn->addNode (parentId);
      }
      parents.push_back (parent);
      parentL = YAP_TailOfTerm (parentL);
    }
    Distribution* dist = bn->getDistribution (distId);
    if (!dist) {
      dist = new Distribution (distId);
      bn->addDistribution (dist);
    }
    BayesNode* node = bn->getNode (varId);
    if (node) {
      node->setData (dsize, evidence, parents, dist);
    } else {
      bn->addNode (varId, dsize, evidence, parents, dist);
    }
    varList = YAP_TailOfTerm (varList);
  }
  bn->setIndexes();

  if (Statistics::numCreatedNets == 1688) {
    Statistics::writeStats();
    //Statistics::writeStats();
    //CALLGRIND_STOP_INSTRUMENTATION;
    //CALLGRIND_DUMP_STATS;
    //exit (0);
  }
  YAP_Int p = (YAP_Int) (bn);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG2);
}



int
setExtraVarsInfo (void)
{
  BayesNet* bn     = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term varsInfoL =                            YAP_ARG2;
  while (varsInfoL != YAP_TermNil()) {
    YAP_Term head    = YAP_HeadOfTerm (varsInfoL);
    unsigned varId   = YAP_IntOfTerm  (YAP_ArgOfTerm (1, head));
    YAP_Atom label   = YAP_AtomOfTerm (YAP_ArgOfTerm (2, head));
    YAP_Term domainL =                 YAP_ArgOfTerm (3, head);
    Domain domain;
    while (domainL != YAP_TermNil()) {
      YAP_Atom atom = YAP_AtomOfTerm (YAP_HeadOfTerm (domainL));
      domain.push_back ((char*) YAP_AtomName (atom));
      domainL = YAP_TailOfTerm (domainL);
    }
    BayesNode* node = bn->getNode (varId);
    assert (node);
    node->setLabel ((char*) YAP_AtomName (label));
    node->setDomain (domain);
    varsInfoL = YAP_TailOfTerm (varsInfoL);
  }
  return TRUE;
}



int
setParameters (void)
{
  BayesNet* bn    = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term distList =                            YAP_ARG2;
  while (distList != YAP_TermNil()) {
    YAP_Term dist    = YAP_HeadOfTerm (distList);
    unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    YAP_Term paramL  =                           YAP_ArgOfTerm (2, dist);
    ParamSet params;
    while (paramL!= YAP_TermNil()) {
      params.push_back ((double) YAP_FloatOfTerm (YAP_HeadOfTerm (paramL)));
      paramL = YAP_TailOfTerm (paramL);
    }
    bn->getDistribution(distId)->updateParameters(params);
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



int
runSolver (void)
{
  BayesNet* bn      = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList =                            YAP_ARG2;

  vector<NodeSet> tasks;
  NodeSet marginalVars;

  while (taskList != YAP_TermNil()) {
    if (YAP_IsPairTerm (YAP_HeadOfTerm (taskList))) {
      NodeSet jointVars;
      YAP_Term jointList = YAP_HeadOfTerm (taskList);      
      while (jointList != YAP_TermNil()) {
        unsigned varId = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (jointList));
        assert (bn->getNode (varId));
        jointVars.push_back (bn->getNode (varId));
        jointList = YAP_TailOfTerm (jointList);
      }
      tasks.push_back (jointVars);
    } else {
      unsigned varId = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (taskList));
      BayesNode* node = bn->getNode (varId);
      assert (node);
      tasks.push_back (NodeSet() = {node});
      marginalVars.push_back (node);
    }
    taskList = YAP_TailOfTerm (taskList);
  }
  /*
  cout << "tasks to resolve:" << endl;
  for (unsigned i = 0; i < tasks.size(); i++) {
    cout << "i" << ": " ;
    if (tasks[i].size() == 1) {
      cout << tasks[i][0]->getVarId() << endl;
    } else {
      for (unsigned j = 0; j < tasks[i].size(); j++) {
        cout << tasks[i][j]->getVarId() << " " ;
      }
      cout << endl;
    }
  }
  */
  
  cerr << "prunning now..." << endl;
  BayesNet* prunedNet = bn->pruneNetwork (marginalVars);
  bn->printNetworkToFile ("net.txt");
  BPSolver solver (*prunedNet);
  cerr << "solving marginals now..." << endl;
  solver.runSolver();
  cerr << "calculating joints now ..." << endl;

  vector<ParamSet> results;
  results.reserve (tasks.size());  
  for (unsigned i = 0; i < tasks.size(); i++) {
    if (tasks[i].size() == 1) {
      BayesNode* node = prunedNet->getNode (tasks[i][0]->getVarId());
      results.push_back (solver.getPosterioriOf (node));
    } else {
      BPSolver solver2 (*bn);
      cout << "calculating an join dist on: " ;
      for (unsigned j = 0; j < tasks[i].size(); j++) {
        cout << tasks[i][j]->getVarId() << " " ;
      }
      cout << "..." << endl;
      results.push_back (solver2.getJointDistribution (tasks[i]));
    }
  }

  delete prunedNet;

  YAP_Term list = YAP_TermNil();
  for (int i = results.size() - 1; i >= 0; i--) {
    const ParamSet& beliefs = results[i];
    YAP_Term queryBeliefsL = YAP_TermNil();
    for (int j = beliefs.size() - 1; j >= 0; j--) {
      YAP_Int sl1 = YAP_InitSlot(list);
      YAP_Term belief = YAP_MkFloatTerm (beliefs[j]);
      queryBeliefsL = YAP_MkPairTerm (belief, queryBeliefsL);
      list =  YAP_GetFromSlot(sl1);
      YAP_RecoverSlots(1);
    }
    list = YAP_MkPairTerm (queryBeliefsL, list);
  }
 
  return YAP_Unify (list, YAP_ARG3);
}



int
deleteBayesNet (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  bn->freeDistributions();
  delete bn;
  return TRUE;
}



extern "C" void
init_predicates (void)
{
  YAP_UserCPredicate ("create_network",      createNetwork,    2);
  YAP_UserCPredicate ("set_extra_vars_info", setExtraVarsInfo, 2);
  YAP_UserCPredicate ("set_parameters",      setParameters,    2);
  YAP_UserCPredicate ("run_solver",          runSolver,        3);
  YAP_UserCPredicate ("delete_bayes_net",    deleteBayesNet,   1);
}

