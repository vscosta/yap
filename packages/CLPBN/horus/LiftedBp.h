#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_

#include <vector>

#include "LiftedSolver.h"
#include "ParfactorList.h"
#include "Indexer.h"


namespace Horus {

class FactorGraph;
class WeightedBp;

class LiftedBp : public LiftedSolver{
  public:
    LiftedBp (const ParfactorList& pfList);

   ~LiftedBp();

    Params solveQuery (const Grounds&);

    void printSolverFlags() const;

  private:
    void refineParfactors();

    bool iterate();

    std::vector<PrvGroup> getQueryGroups (const Grounds&);

    void createFactorGraph();

    std::vector<std::vector<unsigned>> getWeights() const;

    unsigned rangeOfGround (const Ground&);

    Params getJointByConditioning (const ParfactorList&, const Grounds&);

    ParfactorList  pfList_;
    WeightedBp*    solver_;
    FactorGraph*   fg_;

    DISALLOW_COPY_AND_ASSIGN (LiftedBp);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDBP_H_

