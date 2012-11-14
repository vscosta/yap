#ifndef HORUS_VARELIM_H
#define HORUS_VARELIM_H

#include "unordered_map"

#include "GroundSolver.h"
#include "FactorGraph.h"
#include "Horus.h"


using namespace std;


class VarElim : public GroundSolver
{
  public:
    VarElim (const FactorGraph& fg) : GroundSolver (fg) { }

   ~VarElim (void);

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

#endif // HORUS_VARELIM_H

