#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_

#include "ParfactorList.h"


namespace Horus {

namespace LiftedOperations {

void shatterAgainstQuery (ParfactorList& pfList, const Grounds& query);

void runWeakBayesBall (ParfactorList& pfList, const Grounds& query);

void absorveEvidence (ParfactorList& pfList, ObservedFormulas&);

Parfactors countNormalize (Parfactor*, const LogVarSet&);

Parfactor calcGroundMultiplication (Parfactor pf);

}  // namespace LiftedOperations

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDOPERATIONS_H_

