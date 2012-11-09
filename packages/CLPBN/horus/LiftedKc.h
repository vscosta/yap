#ifndef HORUS_LIFTEDKC_H
#define HORUS_LIFTEDKC_H

#include "ParfactorList.h"

class LiftedWCNF;
class LiftedCircuit;

class LiftedKc
{
  public:
   LiftedKc (const ParfactorList& pfList);

  ~LiftedKc (void);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    LiftedWCNF*    lwcnf_;
    LiftedCircuit* circuit_;

    const ParfactorList& pfList_;
};

#endif // HORUS_LIFTEDKC_H

