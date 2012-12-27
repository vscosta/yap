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

   ~VarElim (void) { }

    Params solveQuery (VarIds);

    void printSolverFlags (void) const;

  private:
    void createFactorList (void);

    void absorveEvidence (void);

    Params processFactorList (const VarIds&);

    void eliminate (VarId);

    void printActiveFactors (void);

    Factors   factorList_;
    unsigned  largestFactorSize_;
    unsigned  totalFactorSize_;
    unordered_map<VarId, vector<size_t>> varMap_;

    DISALLOW_COPY_AND_ASSIGN (VarElim);
};

#endif // HORUS_VARELIM_H

