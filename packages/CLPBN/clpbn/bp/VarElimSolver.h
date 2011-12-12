#ifndef HORUS_VARELIMSOLVER_H
#define HORUS_VARELIMSOLVER_H

#include "unordered_map"

#include "Solver.h"
#include "FactorGraph.h"
#include "BayesNet.h"
#include "Shared.h"


using namespace std;


class VarElimSolver : public Solver
{
  public:
    VarElimSolver (const BayesNet&);
    VarElimSolver (const FactorGraph&);
   ~VarElimSolver (void);
    void      runSolver (void) { }
    ParamSet  getPosterioriOf (VarId);
    ParamSet  getJointDistributionOf (const VarIdSet&);

  private:
    void createFactorList (void);
    void introduceEvidence (void);
    void chooseEliminationOrder (const VarIdSet&);
    void processFactorList (const VarIdSet&);
    void eliminate (VarId);
    void printActiveFactors (void);

    const BayesNet*                  bayesNet_;
    const FactorGraph*               factorGraph_;
    vector<Factor*>                  factorList_;
    VarIdSet                         elimOrder_;
    unordered_map<VarId, vector<unsigned>> varFactors_;
};

#endif // HORUS_VARELIMSOLVER_H

