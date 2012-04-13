#ifndef HORUS_SOLVER_H
#define HORUS_SOLVER_H

#include <iomanip>

#include "Var.h"
#include "FactorGraph.h"


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
   
  protected:
    const FactorGraph& fg;
};

#endif // HORUS_SOLVER_H

