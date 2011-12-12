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
    virtual ParamSet  getPosterioriOf (VarId) = 0;
    virtual ParamSet  getJointDistributionOf (const VarIdSet&) = 0;

    void printAllPosterioris (void);
    void printPosterioriOf (VarId vid);
    void printJointDistributionOf (const VarIdSet& vids);
   
  private:
    const GraphicalModel* gm_;
};

#endif // HORUS_SOLVER_H

