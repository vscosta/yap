#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_

#include "LiftedSolver.h"
#include "ParfactorList.h"


namespace Horus {

class LiftedKc : public LiftedSolver {
  public:
   LiftedKc (const ParfactorList& pfList)
       : LiftedSolver(pfList) { }

   Params solveQuery (const Grounds&);

   void printSolverFlags() const;

  private:
    DISALLOW_COPY_AND_ASSIGN (LiftedKc);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_
