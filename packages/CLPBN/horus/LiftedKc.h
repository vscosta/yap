#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_

#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>

#include "LiftedSolver.h"
#include "LiftedWCNF.h"
#include "ParfactorList.h"


namespace Horus {

class LiftedCircuit;

class LiftedKc : public LiftedSolver {
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

    DISALLOW_COPY_AND_ASSIGN (LiftedKc);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDKC_H_

