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
    VarElimSolver (const FactorGraph& fg) : Solver (fg) { }

   ~VarElimSolver (void);

    Params solveQuery (VarIds);

    void printSolverFlags (void) const;

  private:
    void createFactorList (void);

    void absorveEvidence (void);

    void findEliminationOrder (const VarIds&);

    void processFactorList (const VarIds&);

    void eliminate (VarId);

    void printActiveFactors (void);

    Factors  factorList_;
    VarIds   elimOrder_;
    unsigned largestFactorSize_;
    unsigned totalFactorSize_;
    unordered_map<VarId, vector<size_t>> varFactors_;
};

#endif // HORUS_VARELIMSOLVER_H
