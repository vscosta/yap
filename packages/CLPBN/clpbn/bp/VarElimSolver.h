#ifndef HORUS_VARELIMSOLVER_H
#define HORUS_VARELIMSOLVER_H

#include "unordered_map"

#include "Solver.h"
#include "FactorGraph.h"
#include "Horus.h"


using namespace std;


class VarElimSolver : public Solver
{
  public:
    VarElimSolver (const FactorGraph&);

   ~VarElimSolver (void);

    void runSolver (void) { }

    Params getPosterioriOf (VarId);

    Params getJointDistributionOf (const VarIds&);

  private:
    void createFactorList (void);

    void absorveEvidence (void);

    void findEliminationOrder (const VarIds&);

    void processFactorList (const VarIds&);

    void eliminate (VarId);

    void printActiveFactors (void);

    const FactorGraph*  factorGraph_;
    vector<Factor*>     factorList_;
    VarIds              elimOrder_;
    unordered_map<VarId, vector<unsigned>> varFactors_;
};

#endif // HORUS_VARELIMSOLVER_H

