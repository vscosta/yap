#ifndef YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_
#define YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_

#include "FactorGraph.h"
#include "Horus.h"


namespace Horus {

class GroundSolver {
  public:
    GroundSolver (const FactorGraph& factorGraph) : fg(factorGraph) { }

    virtual ~GroundSolver() { } // ensure that subclass destructor is called

    virtual Params solveQuery (VarIds queryVids) = 0;

    virtual void printSolverFlags() const = 0;

    void printAnswer (const VarIds& vids);

    void printAllPosterioris();

    static Params getJointByConditioning (GroundSolverType,
        FactorGraph, const VarIds& jointVarIds);

  protected:
    const FactorGraph& fg;

  private:
    DISALLOW_COPY_AND_ASSIGN (GroundSolver);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_GROUNDSOLVER_H_

