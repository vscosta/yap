#ifndef HORUS_LIFTEDBPSOLVER_H
#define HORUS_LIFTEDBPSOLVER_H

#include "ParfactorList.h"

class SpLink;
class FactorGraph;
class WeightedBpSolver;

class LiftedBpSolver
{
  public:
   LiftedBpSolver (const ParfactorList& pfList);

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    void refineParfactors (void);

    bool iterate (void);

    vector<PrvGroup> getQueryGroups (const Grounds&);

    FactorGraph* getFactorGraph (void);

    vector<vector<unsigned>> getWeights (void) const;

    ParfactorList      pfList_;
    WeightedBpSolver*  solver_;

};

#endif // HORUS_LIFTEDBPSOLVER_H
