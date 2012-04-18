#ifndef HORUS_FOVESOLVER_H
#define HORUS_FOVESOLVER_H


#include "ParfactorList.h"


class LiftedOperator
{
  public:
    virtual double getLogCost (void) = 0;

    virtual void apply (void) = 0;

    virtual string toString (void) = 0;

    static vector<LiftedOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    static void printValidOps (ParfactorList&, const Grounds&);
};



class SumOutOperator : public LiftedOperator
{
  public:
    SumOutOperator (unsigned group, ParfactorList& pfList) 
        : group_(group), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<SumOutOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    string toString (void);

  private:
    static bool validOp (unsigned, ParfactorList&, const Grounds&);

    static vector<ParfactorList::iterator> parfactorsWithGroup (
        ParfactorList& pfList, unsigned group);

    static bool isToEliminate (Parfactor*, unsigned, const Grounds&);

    unsigned        group_;
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
        ParfactorList::iterator pfIter,
        LogVar X,
        ParfactorList& pfList)
        : pfIter_(pfIter), X_(X), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static vector<GroundOperator*> getValidOps (ParfactorList&);

    string toString (void);

  private:
    ParfactorList::iterator  pfIter_;
    LogVar                   X_;
    ParfactorList&           pfList_;
};



class FoveSolver
{
  public:
   FoveSolver (const ParfactorList& pfList) : pfList_(pfList) { }

   Params getPosterioriOf (const Ground&);

   Params getJointDistributionOf (const Grounds&);

   static void absorveEvidence (
       ParfactorList& pfList, ObservedFormulas& obsFormulas);

   static Parfactors countNormalize (Parfactor*, const LogVarSet&);

  private:
    void runSolver (const Grounds&);

    LiftedOperator* getBestOperation (const Grounds&);

    void runWeakBayesBall (const Grounds&); 

    void shatterAgainstQuery (const Grounds&);

    static Parfactors absorve (ObservedFormula&, Parfactor*);

    ParfactorList pfList_;
};

#endif // HORUS_FOVESOLVER_H

