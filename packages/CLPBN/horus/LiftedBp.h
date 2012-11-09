#ifndef HORUS_LIFTEDBP_H
#define HORUS_LIFTEDBP_H

#include "ParfactorList.h"

class FactorGraph;
class WeightedBp;

class LiftedBp
{
  public:
   LiftedBp (const ParfactorList& pfList);

  ~LiftedBp (void);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    void refineParfactors (void);

    bool iterate (void);

    vector<PrvGroup> getQueryGroups (const Grounds&);

    void createFactorGraph (void);

    vector<vector<unsigned>> getWeights (void) const;
 
    unsigned rangeOfGround (const Ground&);

    Params getJointByConditioning (const ParfactorList&, const Grounds&);

    ParfactorList  pfList_;
    WeightedBp*    solver_;
    FactorGraph*   fg_;

};

#endif // HORUS_LIFTEDBP_H

