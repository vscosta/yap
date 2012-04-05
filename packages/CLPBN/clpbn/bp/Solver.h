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

    virtual void runSolver (void) = 0;

    virtual Params getPosterioriOf (VarId) = 0;

    virtual Params getJointDistributionOf (const VarIds&) = 0;

    void printAllPosterioris (void);

    void printPosterioriOf (VarId vid);

    void printJointDistributionOf (const VarIds& vids);
   
  private:
    const FactorGraph& fg_;
};

#endif // HORUS_SOLVER_H

