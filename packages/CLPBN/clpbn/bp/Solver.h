#ifndef HORUS_SOLVER_H
#define HORUS_SOLVER_H

#include <iomanip>

#include "GraphicalModel.h"
#include "VarNode.h"

using namespace std;

class Solver
{
  public:
    Solver (const GraphicalModel* gm)
    {
      gm_ = gm;
    }
    virtual ~Solver() {} // to ensure that subclass destructor is called
    virtual void      runSolver (void) = 0;
    virtual Params  getPosterioriOf (VarId) = 0;
    virtual Params  getJointDistributionOf (const VarIds&) = 0;

    void printAllPosterioris (void);
    void printPosterioriOf (VarId vid);
    void printJointDistributionOf (const VarIds& vids);
   
  private:
    const GraphicalModel* gm_;
};

#endif // HORUS_SOLVER_H

