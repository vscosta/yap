
#include "LiftedBpSolver.h"


Params
LiftedBpSolver::getPosterioriOf (const Ground&)
{
  return Params();
}



Params
LiftedBpSolver::getJointDistributionOf (const Grounds&)
{
  return Params();
}



void
LiftedBpSolver::printSolverFlags (void) const
{
  stringstream ss;
  ss << "lifted bp [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}

