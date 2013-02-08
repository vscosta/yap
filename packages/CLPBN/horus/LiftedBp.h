#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_

#include <vector>

#include "LiftedSolver.h"
#include "ParfactorList.h"


namespace horus {

class FactorGraph;
class WeightedBp;

class LiftedBp : public LiftedSolver
{
  public:
   LiftedBp (const ParfactorList& pfList);

  ~LiftedBp (void);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    void refineParfactors (void);

    bool iterate (void);

    std::vector<PrvGroup> getQueryGroups (const Grounds&);

    void createFactorGraph (void);

    std::vector<std::vector<unsigned>> getWeights (void) const;

    unsigned rangeOfGround (const Ground&);

    Params getJointByConditioning (const ParfactorList&, const Grounds&);

    ParfactorList  pfList_;
    WeightedBp*    solver_;
    FactorGraph*   fg_;

    DISALLOW_COPY_AND_ASSIGN (LiftedBp);
};

}  // namespace horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_

