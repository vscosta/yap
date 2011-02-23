#include <iostream>
#include <sstream>
#include <vector>
#include <string>

#include <Yap/YapInterface.h>

#include <BayesianNetwork.h>
#include <BayesianNode.h>
#include <BpNetwork.h>
#include <BpNode.h>


using namespace std;

int addVariables (BayesianNetwork&, YAP_Term, int);
int addDistributions (BayesianNetwork&, YAP_Term, int);

int createNetwork (void)
{
  BayesianNetwork* bn = new BpNetwork();
  addDistributions (*bn, YAP_ARG3, (int) YAP_IntOfTerm (YAP_ARG4));
  addVariables (*bn, YAP_ARG1, (int) YAP_IntOfTerm (YAP_ARG2));
  YAP_Int p = (YAP_Int) (bn);
  return YAP_Unify (YAP_MkIntTerm (p), YAP_ARG5);
}



int addVariables (BayesianNetwork& bn, YAP_Term varList, int nVars)
{
  for (int i = 0; i < nVars; i++) {
    YAP_Term var = YAP_HeadOfTerm (varList);
    int varId  = (int) YAP_IntOfTerm (YAP_ArgOfTerm (1, var));
    int distId = (int) YAP_IntOfTerm (YAP_ArgOfTerm (2, var));
    YAP_Term parentsList = YAP_ArgOfTerm (3, var);
    int nParents = (int) YAP_IntOfTerm (YAP_ArgOfTerm (4, var));
    vector<BayesianNode*> parents;
    for (int j = 0; j < nParents; j++) {
      int parentId = (int) YAP_IntOfTerm (YAP_HeadOfTerm (parentsList));
      stringstream parentName;
      parentName << parentId;
      parents.push_back (bn.getNode (parentName.str()));
      parentsList = YAP_TailOfTerm (parentsList);
    }
    stringstream nodeName;
    nodeName << varId;
    int evidence = (int) YAP_IntOfTerm (YAP_ArgOfTerm (5, var));
    bn.addNode (nodeName.str(), parents, evidence, distId);
    varList = YAP_TailOfTerm (varList);
  }
  return TRUE;
}



int addDistributions (BayesianNetwork& bn, YAP_Term distList, int nDists)
{
  for (int i = 0; i < nDists; i++) {
    YAP_Term dist = YAP_HeadOfTerm (distList);
    int distId = (int) YAP_IntOfTerm (YAP_ArgOfTerm (1, dist));
    YAP_Term domainList = YAP_ArgOfTerm (2, dist);
    int domainSize = (int) YAP_IntOfTerm (YAP_ArgOfTerm (3, dist));
    vector<string> domain (domainSize);
    for (int j = 0; j < domainSize; j++) {
      YAP_Atom atom = YAP_AtomOfTerm (YAP_HeadOfTerm (domainList));
      domain[j] = (char*) YAP_AtomName (atom);;
      domainList = YAP_TailOfTerm (domainList);
    }
    YAP_Term paramsList = YAP_ArgOfTerm (4, dist);
    int nParams = (int) YAP_IntOfTerm (YAP_ArgOfTerm (5, dist));
    double* params = new double [nParams];
    for (int j = 0; j < nParams; j++) {
      params[j] = (double) YAP_FloatOfTerm (YAP_HeadOfTerm (paramsList));
      paramsList = YAP_TailOfTerm (paramsList);
    }
    bn.addDistribution (distId, params, nParams, domain);
    distList = YAP_TailOfTerm (distList);
  }
  return TRUE;
}



int runSolver (void)
{
  BpNetwork* bn = (BpNetwork*) YAP_IntOfTerm (YAP_ARG1);
  YAP_Term queryVarsList = YAP_ARG2;
  int nQueryVars = (int) YAP_IntOfTerm (YAP_ARG3);
  vector<BayesianNode*> queryVars;
  for (int i = 0; i < nQueryVars; i++) {
    int queryVarId = (int) YAP_IntOfTerm (YAP_HeadOfTerm (queryVarsList));
    stringstream queryVarName;
    queryVarName << queryVarId;
    queryVars.push_back (bn->getNode (queryVarName.str()));
    queryVarsList = YAP_TailOfTerm (queryVarsList);
  }
  bn->runSolver (queryVars);
  vector<double> beliefs = bn->getBeliefs();
  YAP_Term beliefsList = YAP_TermNil();
  for (int i = beliefs.size() - 1; i >= 0; i--) {
    YAP_Term belief = YAP_MkFloatTerm (beliefs[i]);
    beliefsList = YAP_MkPairTerm (belief, beliefsList);
  }
  return YAP_Unify (beliefsList, YAP_ARG4);
}



int freeMemory (void)
{
  BpNetwork* bn = (BpNetwork*) YAP_IntOfTerm (YAP_ARG1);
  delete bn;
  return TRUE;
}



extern "C" void init_predicates (void)
{
  YAP_UserCPredicate ("create_network", createNetwork, 5);
  YAP_UserCPredicate ("run_solver", runSolver, 4);
  YAP_UserCPredicate ("free_memory", freeMemory, 1);
}

