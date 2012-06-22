#ifndef HORUS_SOLVER_H
#define HORUS_SOLVER_H

#include <iomanip>

#include "FactorGraph.h"
#include "Var.h"
#include "Horus.h"


using namespace std;

class Solver
{
  public:
    Solver (const FactorGraph& factorGraph) : fg(factorGraph) { }

    virtual ~Solver() { } // ensure that subclass destructor is called

    virtual Params solveQuery (VarIds queryVids) = 0;

    virtual void printSolverFlags (void) const = 0;

    void printAnswer (const VarIds& vids);

    void printAllPosterioris (void);

    Params getJointByConditioning (GroundSolver,
        FactorGraph, const VarIds& jointVarIds) const;
   
  protected:
    const FactorGraph& fg;
};

#endif // HORUS_SOLVER_H

