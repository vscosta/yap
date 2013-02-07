#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H_

#include "ParfactorList.h"


namespace horus {

class LiftedSolver
{
  public:
    LiftedSolver (const ParfactorList& pfList)
        : parfactorList(pfList) { }

    virtual ~LiftedSolver() { } // ensure that subclass destructor is called

    virtual Params solveQuery (const Grounds& query) = 0;

    virtual void printSolverFlags (void) const = 0;

  protected:
    const ParfactorList& parfactorList;

  private:
    DISALLOW_COPY_AND_ASSIGN (LiftedSolver);
};

}  // namespace horus

#endif // YAP_PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H_

