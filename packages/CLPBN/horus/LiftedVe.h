#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_

#include <vector>
#include <string>

#include "LiftedSolver.h"
#include "ParfactorList.h"


namespace horus {

class LiftedOperator
{
  public:
    virtual ~LiftedOperator (void) { }

    virtual double getLogCost (void) = 0;

    virtual void apply (void) = 0;

    virtual std::string toString (void) = 0;

    static std::vector<LiftedOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    static void printValidOps (ParfactorList&, const Grounds&);

    static std::vector<ParfactorList::iterator> getParfactorsWithGroup (
        ParfactorList&, PrvGroup group);

  private:
    DISALLOW_ASSIGN (LiftedOperator);
};



class ProductOperator : public LiftedOperator
{
  public:
    ProductOperator (
        ParfactorList::iterator g1, ParfactorList::iterator g2,
        ParfactorList& pfList) : g1_(g1), g2_(g2), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static std::vector<ProductOperator*> getValidOps (ParfactorList&);

    std::string toString (void);

  private:
    static bool validOp (Parfactor*, Parfactor*);

    ParfactorList::iterator  g1_;
    ParfactorList::iterator  g2_;
    ParfactorList&           pfList_;

    DISALLOW_COPY_AND_ASSIGN (ProductOperator);
};



class SumOutOperator : public LiftedOperator
{
  public:
    SumOutOperator (PrvGroup group, ParfactorList& pfList)
        : group_(group), pfList_(pfList) { }

    double getLogCost (void);

    void apply (void);

    static std::vector<SumOutOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    std::string toString (void);

  private:
    static bool validOp (PrvGroup, ParfactorList&, const Grounds&);

    static bool isToEliminate (Parfactor*, PrvGroup, const Grounds&);

    PrvGroup        group_;
    ParfactorList&  pfList_;

    DISALLOW_COPY_AND_ASSIGN (SumOutOperator);
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

    static std::vector<CountingOperator*> getValidOps (ParfactorList&);

    std::string toString (void);

  private:
    static bool validOp (Parfactor*, LogVar);

    ParfactorList::iterator  pfIter_;
    LogVar                   X_;
    ParfactorList&           pfList_;

    DISALLOW_COPY_AND_ASSIGN (CountingOperator);
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

    static std::vector<GroundOperator*> getValidOps (ParfactorList&);

    std::string toString (void);

  private:
    std::vector<std::pair<PrvGroup, unsigned>> getAffectedFormulas (void);

    PrvGroup        group_;
    unsigned        lvIndex_;
    ParfactorList&  pfList_;

    DISALLOW_COPY_AND_ASSIGN (GroundOperator);
};



class LiftedVe : public LiftedSolver
{
  public:
   LiftedVe (const ParfactorList& pfList)
       : LiftedSolver(pfList) { }

   Params solveQuery (const Grounds&);

   void printSolverFlags (void) const;

  private:
    void runSolver (const Grounds&);

    LiftedOperator* getBestOperation (const Grounds&);

    ParfactorList  pfList_;
    double         largestCost_;

    DISALLOW_COPY_AND_ASSIGN (LiftedVe);
};

}  // namespace horus

#endif // YAP_PACKAGES_CLPBN_HORUS_LIFTEDVE_H_

