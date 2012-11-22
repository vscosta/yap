#ifndef HORUS_LIFTEDKC_H
#define HORUS_LIFTEDKC_H

#include "LiftedSolver.h"
#include "ParfactorList.h"

class LiftedWCNF;
class LiftedCircuit;


class LiftedKc : public LiftedSolver
{
  public:
   LiftedKc (const ParfactorList& pfList)
       : LiftedSolver(pfList) { }

  ~LiftedKc (void);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    LiftedWCNF*     lwcnf_;
    LiftedCircuit*  circuit_;
    ParfactorList   pfList_;
};

#endif // HORUS_LIFTEDKC_H

