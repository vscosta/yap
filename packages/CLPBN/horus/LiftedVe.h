#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_

#include "LiftedSolver.h"
#include "ParfactorList.h"


namespace Horus {

class LiftedOperator;


class LiftedVe : public LiftedSolver {
  public:
   LiftedVe (const ParfactorList& pfList)
       : LiftedSolver(pfList) { }

   Params solveQuery (const Grounds&);

   void printSolverFlags() const;

  private:
    void runSolver (const Grounds&);

    LiftedOperator* getBestOperation (const Grounds&);

    ParfactorList  pfList_;
    double         largestCost_;

    DISALLOW_COPY_AND_ASSIGN (LiftedVe);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_

