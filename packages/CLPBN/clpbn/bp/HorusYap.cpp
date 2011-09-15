#include <cstdlib>
#include <vector>

#include <iostream>
#include <sstream>

#include <YapInterface.h>

#include "BayesNet.h"
#include "FactorGraph.h"
#include "BPSolver.h"
#include "SPSolver.h"
#include "CountingBP.h"

using namespace std;


int
createNetwork (void)
{
  //Statistics::numCreatedNets ++;
  //cout << "creating network number " << Statistics::numCreatedNets << endl;

  BayesNet* bn = new BayesNet();
  YAP_Term varList = YAP_ARG1;
  while (varList != YAP_TermNil()) {
    YAP_Term var     = YAP_HeadOfTerm (varList);
    Vid vid          = (Vid) YAP_IntOfTerm (YAP_ArgOfTerm (1, var));
    unsigned dsize   = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (2, var));
    int evidence     = (int) YAP_IntOfTerm (YAP_ArgOfTerm (3, var));
    YAP_Term parentL =  YAP_ArgOfTerm (4, var);
    unsigned distId  = (unsigned) YAP_IntOfTerm (YAP_ArgOfTerm (5, var));
    BnNodeSet parents;
    while (parentL != YAP_TermNil()) {
      unsigned parentId = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (parentL));
      BayesNode* parent = bn->getBayesNode (parentId);
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
    BayesNode* node = bn->getBayesNode (vid);
    if (node) {
      node->setData (dsize, evidence, parents, dist);
    } else {
      bn->addNode (vid, dsize, evidence, parents, dist);
    }
    varList = YAP_TailOfTerm (varList);
  }
  bn->setIndexes();

  // if (Statistics::numCreatedNets == 1688) {
  //   Statistics::writeStats();
  //   exit (0);
  // }
  YAP_Int p = (YAP_Int) (bn);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG2);
}



int
setExtraVarsInfo (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term varsInfoL =  YAP_ARG2;
  while (varsInfoL != YAP_TermNil()) {
    YAP_Term head    = YAP_HeadOfTerm (varsInfoL);
    Vid vid     = YAP_IntOfTerm  (YAP_ArgOfTerm (1, head));
    YAP_Atom label   = YAP_AtomOfTerm (YAP_ArgOfTerm (2, head));
    YAP_Term domainL = YAP_ArgOfTerm (3, head);
    Domain domain;
    while (domainL != YAP_TermNil()) {
      YAP_Atom atom = YAP_AtomOfTerm (YAP_HeadOfTerm (domainL));
      domain.push_back ((char*) YAP_AtomName (atom));
      domainL = YAP_TailOfTerm (domainL);
    }
    BayesNode* node = bn->getBayesNode (vid);
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
    bn->getDistribution(distId)->updateParameters(params);
    if (Statistics::numCreatedNets == 4) {
      cout << "dist " << distId << " parameters:" ;
      cout << Util::parametersToString (params);
      cout << endl;
    }
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



int
runSolver (void)
{
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term taskList = YAP_ARG2;
  vector<VidSet> tasks;
  VidSet marginalVids;

  while (taskList != YAP_TermNil()) {
    if (YAP_IsPairTerm (YAP_HeadOfTerm (taskList))) {
      VidSet jointVids;
      YAP_Term jointList = YAP_HeadOfTerm (taskList);      
      while (jointList != YAP_TermNil()) {
        Vid vid = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (jointList));
        assert (bn->getBayesNode (vid));
        jointVids.push_back (vid);
        jointList = YAP_TailOfTerm (jointList);
      }
      tasks.push_back (jointVids);
    } else {
      Vid vid = (unsigned) YAP_IntOfTerm (YAP_HeadOfTerm (taskList));
      assert (bn->getBayesNode (vid));
      tasks.push_back (VidSet() = {vid});
      marginalVids.push_back (vid);
    }
    taskList = YAP_TailOfTerm (taskList);
  }
  
  // cout << "inference tasks:" << endl;
  // for (unsigned i = 0; i < tasks.size(); i++) {
  //   cout << "i" << ": " ;
  //   if (tasks[i].size() == 1) {
  //     cout << tasks[i][0] << endl;
  //   } else {
  //     for (unsigned j = 0; j < tasks[i].size(); j++) {
  //       cout << tasks[i][j] << " " ;
  //     }
  //     cout << endl;
  //   }
  // }
  
  Solver* solver     = 0;
  GraphicalModel* gm = 0;
  VidSet vids;
  const BnNodeSet& nodes = bn->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    vids.push_back (nodes[i]->getVarId());
  }
  if (marginalVids.size() != 0) {
    bn->exportToDotFormat ("bn unbayes.dot");
    BayesNet* mrn = bn->getMinimalRequesiteNetwork (marginalVids);
    mrn->exportToDotFormat ("bn bayes.dot");
    //BayesNet* mrn = bn->getMinimalRequesiteNetwork (vids);
    if (SolverOptions::convertBn2Fg) {
      gm = new FactorGraph (*mrn);
      if (SolverOptions::compressFactorGraph) {
        solver = new CountingBP (*static_cast<FactorGraph*> (gm));
      } else {
        solver = new SPSolver (*static_cast<FactorGraph*> (gm));
      }
      if (SolverOptions::runBayesBall) {
        delete mrn;
      }
    } else {
      gm     = mrn;
      solver = new BPSolver (*static_cast<BayesNet*> (gm));
    }
    solver->runSolver();
  }

  vector<ParamSet> results;
  results.reserve (tasks.size());  
  for (unsigned i = 0; i < tasks.size(); i++) {
    if (tasks[i].size() == 1) {
      results.push_back (solver->getPosterioriOf (tasks[i][0]));
    } else {
      static int count = 0;
      cout << "calculating joint... " << count ++ << endl;
      //if (count == 5225) {
      //  Statistics::printCompressingStats ("compressing.stats");
      //}
      Solver*      solver2  = 0;
      GraphicalModel* gm2   = 0;
      bn->exportToDotFormat ("joint.dot");
      BayesNet* mrn2;
      if (SolverOptions::runBayesBall) {
        mrn2 = bn->getMinimalRequesiteNetwork (tasks[i]);
      } else {
        mrn2 = bn;
      }
      if (SolverOptions::convertBn2Fg) {
        gm2 = new FactorGraph (*mrn2);
        if (SolverOptions::compressFactorGraph) {
          solver2 = new CountingBP (*static_cast<FactorGraph*> (gm2));
        } else {
          solver2 = new SPSolver (*static_cast<FactorGraph*> (gm2));
        }
        if (SolverOptions::runBayesBall) {
          delete mrn2;
        }
      } else {
        gm2     = mrn2;
        solver2 = new BPSolver (*static_cast<BayesNet*> (gm2));
      }
      results.push_back (solver2->getJointDistributionOf (tasks[i]));
      delete solver2;
      delete gm2;
    }
  }

  delete solver;
  delete gm;

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
freeBayesNetwork (void)
{
  //Statistics::printCompressingStats ("../../compressing.stats");
  BayesNet* bn = (BayesNet*) YAP_IntOfTerm (YAP_ARG1);
  bn->freeDistributions();
  delete bn;
  return TRUE;
}



extern "C" void
init_predicates (void)
{
  YAP_UserCPredicate ("create_network",        createNetwork,    2);
  YAP_UserCPredicate ("set_extra_vars_info",   setExtraVarsInfo, 2);
  YAP_UserCPredicate ("set_parameters",        setParameters,    2);
  YAP_UserCPredicate ("run_solver",            runSolver,        3);
  YAP_UserCPredicate ("free_bayesian_network", freeBayesNetwork, 1);
}

