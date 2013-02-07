#ifndef PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H
#define PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H

#include "ParfactorList.h"
#include "Horus.h"

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

#endif // PACKAGES_CLPBN_HORUS_LIFTEDSOLVER_H

