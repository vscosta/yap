#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_

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

#endif // YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_

