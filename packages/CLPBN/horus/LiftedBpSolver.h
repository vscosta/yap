#ifndef HORUS_LIFTEDBPSOLVER_H
#define HORUS_LIFTEDBPSOLVER_H

#include "ParfactorList.h"

class LiftedBpSolver
{
  public:
   LiftedBpSolver (const ParfactorList& pfList) : pfList_(pfList) { }

   Params getPosterioriOf (const Ground&);

   Params getJointDistributionOf (const Grounds&);

   void printSolverFlags (void) const;

  private:
    ParfactorList pfList_;

};

#endif // HORUS_LIFTEDBPSOLVER_H
