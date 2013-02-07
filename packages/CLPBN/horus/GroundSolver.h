#ifndef YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_
#define YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_

#include "FactorGraph.h"
#include "Horus.h"


class GroundSolver
{
  public:
    GroundSolver (const FactorGraph& factorGraph) : fg(factorGraph) { }

    virtual ~GroundSolver() { } // ensure that subclass destructor is called

    virtual Params solveQuery (VarIds queryVids) = 0;

    virtual void printSolverFlags (void) const = 0;

    void printAnswer (const VarIds& vids);

    void printAllPosterioris (void);

    static Params getJointByConditioning (GroundSolverType,
        FactorGraph, const VarIds& jointVarIds);

  protected:
    const FactorGraph& fg;

  private:
    DISALLOW_COPY_AND_ASSIGN (GroundSolver);
};

#endif // YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_

