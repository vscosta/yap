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
    virtual ~Solver() {} // to call subclass destructor
    virtual void     runSolver (void) = 0;
    virtual ParamSet getPosterioriOf  (Vid) const = 0;
    virtual ParamSet getJointDistributionOf (const VidSet&) = 0;

    void printAllPosterioris (void) const
    {
      VarSet vars = gm_->getVariables();
      for (unsigned i = 0; i < vars.size(); i++) {
        printPosterioriOf (vars[i]->getVarId());
      }
    }

    void printPosterioriOf (Vid vid) const
    {
      Variable* var = gm_->getVariable (vid);
      cout << endl;
      cout << setw (20) << left << var->getLabel() << "posteriori" ;
      cout << endl;
      cout << "------------------------------" ;
      cout << endl;
      const Domain& domain = var->getDomain();
      ParamSet results = getPosterioriOf (vid);
      for (unsigned xi = 0; xi < var->getDomainSize(); xi++) {
        cout << setw (20) << domain[xi];
        cout << setprecision (PRECISION) << results[xi];
        cout << endl;
      }
      cout << endl;
    }

    void printJointDistributionOf (const VidSet& vids)
    {
      const ParamSet& jointDist = getJointDistributionOf (vids);
      cout << endl;
      cout << "joint distribution of " ;
      VarSet vars;
      for (unsigned i = 0; i < vids.size() - 1; i++) {
        Variable* var = gm_->getVariable (vids[i]);
        cout << var->getLabel() << ", " ;
        vars.push_back (var);
      }
      Variable* var = gm_->getVariable (vids[vids.size() - 1]);
      cout << var->getLabel() ;
      vars.push_back (var);
      cout << endl;
      cout << "------------------------------" ;
      cout << endl;
      const vector<string>& domainConfs = Util::getInstantiations (vars);
      for (unsigned i = 0; i < jointDist.size(); i++) {
        cout << left << setw (20) << domainConfs[i];
        cout << setprecision (PRECISION) << jointDist[i];
        cout << endl;
      }
      cout << endl;
    }

  private:
    const GraphicalModel* gm_;
};

#endif //BP_SOLVER_H

