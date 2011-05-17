#ifndef BP_SOLVER_H
#define BP_SOLVER_H

#include <iomanip>

#include "GraphicalModel.h"
#include "Variable.h"

using namespace std;

class Solver
{
  public:
    Solver (const GraphicalModel* gm)
    {
      gm_ = gm;
    }
    virtual void     runSolver (void) = 0;
    virtual ParamSet getPosterioriOf  (const Variable*) const = 0;

    void printPosterioriOf (const Variable* var) const
    {
      cout << endl;
      cout << setw (20) << left << var->getLabel() << "posteriori" ;
      cout << endl;
      cout << "------------------------------" ;
      cout << endl;
      const Domain& domain = var->getDomain();
      ParamSet results = getPosterioriOf (var);
      for (int xi = 0; xi < var->getDomainSize(); xi++) {
        cout << setw (20) << domain[xi];
        cout << setprecision (PRECISION) << results[xi];
        cout << endl;
      }
      cout << endl;
    }

    void printAllPosterioris (void) const
    {
      VarSet vars = gm_->getVariables();
      for (unsigned i = 0; i < vars.size(); i++) {
        printPosterioriOf (vars[i]);
      }
    }

  private:
     const GraphicalModel* gm_;
};

#endif
