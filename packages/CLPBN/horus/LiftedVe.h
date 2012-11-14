#ifndef HORUS_LIFTEDVE_H
#define HORUS_LIFTEDVE_H

#include "LiftedSolver.h"
#include "ParfactorList.h"


class LiftedOperator
{
  public:
    virtual ~LiftedOperator (void) { }
  
    virtual double getLogCost (void) = 0;

    virtual void apply (void) = 0;

    virtual string toString (void) = 0;

    static vector<LiftedOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    static void printValidOps (ParfactorList&, const Grounds&);

    static vector<ParfactorList::iterator> getParfactorsWithGroup (
        ParfactorList&, PrvGroup group);
};



class ProductOperator : public LiftedOperator
{
  public:
    ProductOperator (
        ParfactorList::iterator g1, ParfactorList::iterator g2,
        ParfactorList& pfList) : g1_(g1), g2_(g2), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<ProductOperator*> getValidOps (ParfactorList&);

    string toString (void);

  private:
    static bool validOp (Parfactor*, Parfactor*);

    ParfactorList::iterator  g1_;
    ParfactorList::iterator  g2_;
    ParfactorList&           pfList_;
};



class SumOutOperator : public LiftedOperator
{
  public:
    SumOutOperator (PrvGroup group, ParfactorList& pfList) 
        : group_(group), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<SumOutOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    string toString (void);

  private:
    static bool validOp (PrvGroup, ParfactorList&, const Grounds&);

    static bool isToEliminate (Parfactor*, PrvGroup, const Grounds&);

    PrvGroup        group_;
    ParfactorList&  pfList_;
};



class CountingOperator : public LiftedOperator
{
  public:
    CountingOperator (
        ParfactorList::iterator pfIter,
        LogVar X,
        ParfactorList& pfList)
        : pfIter_(pfIter), X_(X), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<CountingOperator*> getValidOps (ParfactorList&);

    string toString (void);

  private:
    static bool validOp (Parfactor*, LogVar);

    ParfactorList::iterator  pfIter_;
    LogVar                   X_;
    ParfactorList&           pfList_;
};



class GroundOperator : public LiftedOperator
{
  public:
    GroundOperator (
        PrvGroup group,
        unsigned lvIndex,
        ParfactorList& pfList)
        : group_(group), lvIndex_(lvIndex), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<GroundOperator*> getValidOps (ParfactorList&);

    string toString (void);

  private:
     vector<pair<PrvGroup, unsigned>> getAffectedFormulas (void);

    PrvGroup        group_;
    unsigned        lvIndex_;
    ParfactorList&  pfList_;
};



class LiftedVe : public LiftedSolver
{
  public:
   LiftedVe (const ParfactorList& pfList)
       : LiftedSolver(pfList), pfList_(pfList) { }

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    void runSolver (const Grounds&);

    LiftedOperator* getBestOperation (const Grounds&);

    ParfactorList  pfList_;
    double         largestCost_;
};

#endif // HORUS_LIFTEDVE_H

