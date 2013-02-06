#ifndef PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H
#define PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H

#include "ParfactorList.h"

class LiftedOperations
{
  public:
    static void shatterAgainstQuery (
        ParfactorList& pfList, const Grounds& query);

    static void runWeakBayesBall (
        ParfactorList& pfList, const Grounds&);

    static void absorveEvidence (
        ParfactorList& pfList, ObservedFormulas& obsFormulas);

    static Parfactors countNormalize (Parfactor*, const LogVarSet&);

    static Parfactor calcGroundMultiplication (Parfactor pf);

  private:
    static Parfactors absorve (ObservedFormula&, Parfactor*);

    DISALLOW_COPY_AND_ASSIGN (LiftedOperations);
};

#endif // PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H

