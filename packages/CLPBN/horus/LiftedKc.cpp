#include "LiftedKc.h"
#include "LiftedWCNF.h"
#include "LiftedCircuit.h"


LiftedKc::LiftedKc (const ParfactorList& pfList)
    : pfList_(pfList)
{
  lwcnf_ = new LiftedWCNF (pfList);
  circuit_ = new LiftedCircuit (lwcnf_);
}



LiftedKc::~LiftedKc (void)
{
}



Params
LiftedKc::solveQuery (const Grounds&)
{
  return Params();
}



void
LiftedKc::printSolverFlags (void) const
{
  stringstream ss;
  ss << "lifted kc [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}

