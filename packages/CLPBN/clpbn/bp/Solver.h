#ifndef HORUS_SOLVER_H
#define HORUS_SOLVER_H

#include <iomanip>

#include "Var.h"
#include "FactorGraph.h"


using namespace std;

class Solver
{
  public:
    Solver (const FactorGraph& fg) : fg_(fg) { }

    virtual ~Solver() { } // ensure that subclass destructor is called

    virtual Params solveQuery (VarIds queryVids) = 0;

    void printAnswer (const VarIds& vids);

    void printAllPosterioris (void);
   
  protected:
    const FactorGraph& fg_;
};

#endif // HORUS_SOLVER_H

