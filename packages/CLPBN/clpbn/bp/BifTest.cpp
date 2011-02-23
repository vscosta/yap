#include <iostream> 

#include "BayesianNetwork.h"
#include "BayesianNode.h"
#include "BpNetwork.h"
#include "BpNode.h"
#include "BifInterface.h"

using namespace std;

int main (int argc, char* argv[])
{
  BpNetwork bn;
  // BayesianNetwork bn;
  BifInterface::createNetworkFromXML (&bn, argv[1]);
  bn.printNetwork();

  // bn.getNode("FreightTruck")->setEvidence (0);
  // bn.getNode("Alarm")->setEvidence (0);

  // bn.setSolverParameters (SEQUENTIAL_SCHEDULE, 500, 0.001);
  // bn.setSolverParameters (PARALLEL_SCHEDULE, 500, 0.00000000000001);
  // bn.setSolverParameters (PARALLEL_SCHEDULE, 500, 0.0000000000000000000001);
  
  //bn.getNode ("F")->setEvidence (0);
  vector<BayesianNode*> queryVars;
  //queryVars.push_back (bn.getNode ("D"));
  //queryVars.push_back (bn.getNode ("Burglar"));
  queryVars.push_back (bn.getNode ("FreightTruck"));
  queryVars.push_back (bn.getNode ("Alarm"));
  bn.runSolver (queryVars);
  
  // bn.printCurrentStatus();
  // bn.printBeliefs();
  return 0;
}
