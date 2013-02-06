#ifndef PACKAGES_CLPBN_HORUS_BELIEFPROP_H
#define PACKAGES_CLPBN_HORUS_BELIEFPROP_H

#include <set>
#include <vector>

#include <sstream>

#include "GroundSolver.h"
#include "FactorGraph.h"


using namespace std;


enum MsgSchedule {
  SEQ_FIXED,
  SEQ_RANDOM,
  PARALLEL,
  MAX_RESIDUAL
};


class BpLink
{
  public:
    BpLink (FacNode* fn, VarNode* vn);

    virtual ~BpLink (void) { };

    FacNode* facNode (void) const { return fac_; }

    VarNode* varNode (void) const { return var_; }

    const Params& message (void) const { return *currMsg_; }

    Params& nextMessage (void) { return *nextMsg_; }

    double residual (void) const { return residual_; }

    void clearResidual (void);

    void updateResidual (void);

    virtual void updateMessage (void);

    string toString (void) const;

  protected:
    FacNode*  fac_;
    VarNode*  var_;
    Params    v1_;
    Params    v2_;
    Params*   currMsg_;
    Params*   nextMsg_;
    double    residual_;

  private:
    DISALLOW_COPY_AND_ASSIGN (BpLink);
};

typedef vector<BpLink*> BpLinks;


class SPNodeInfo
{
  public:
    SPNodeInfo (void) { }
    void addBpLink (BpLink* link) { links_.push_back (link); }
    const BpLinks& getLinks (void) { return links_; }
  private:
    BpLinks links_;
    DISALLOW_COPY_AND_ASSIGN (SPNodeInfo);
};


class BeliefProp : public GroundSolver
{
  public:
    BeliefProp (const FactorGraph&);

    virtual ~BeliefProp (void);

    Params solveQuery (VarIds);

    virtual void printSolverFlags (void) const;

    virtual Params getPosterioriOf (VarId);

    virtual Params getJointDistributionOf (const VarIds&);

    Params getFactorJoint (FacNode* fn, const VarIds&);

    static double accuracy (void) { return accuracy_; }

    static void setAccuracy (double acc) { accuracy_ = acc; }

    static unsigned maxIterations (void) { return maxIter_; }

    static void setMaxIterations (unsigned mi) { maxIter_ = mi; }

    static MsgSchedule msgSchedule (void) { return schedule_; }

    static void setMsgSchedule (MsgSchedule sch) { schedule_ = sch; }

  protected:
    SPNodeInfo* ninf (const VarNode* var) const;

    SPNodeInfo* ninf (const FacNode* fac) const;

    void calculateAndUpdateMessage (BpLink* link, bool calcResidual = true);

    void calculateMessage (BpLink* link, bool calcResidual = true);

    void updateMessage (BpLink* link);

    struct CompareResidual
    {
      inline bool operator() (const BpLink* link1, const BpLink* link2)
      {
        return link1->residual() > link2->residual();
      }
    };

    void runSolver (void);

    virtual void createLinks (void);

    virtual void maxResidualSchedule (void);

    virtual void calcFactorToVarMsg (BpLink*);

    virtual Params getVarToFactorMsg (const BpLink*) const;

    virtual Params getJointByConditioning (const VarIds&) const;

    BpLinks              links_;
    unsigned             nIters_;
    vector<SPNodeInfo*>  varsI_;
    vector<SPNodeInfo*>  facsI_;
    bool                 runned_;

    typedef multiset<BpLink*, CompareResidual> SortedOrder;
    SortedOrder sortedOrder_;

    typedef unordered_map<BpLink*, SortedOrder::iterator> BpLinkMap;
    BpLinkMap linkMap_;

    static double       accuracy_;
    static unsigned     maxIter_;
    static MsgSchedule  schedule_;

  private:
    void initializeSolver (void);

    bool converged (void);

    virtual void printLinkInformation (void) const;

    DISALLOW_COPY_AND_ASSIGN (BeliefProp);
};



inline SPNodeInfo*
BeliefProp::ninf (const VarNode* var) const
{
  return varsI_[var->getIndex()];
}



inline SPNodeInfo*
BeliefProp::ninf (const FacNode* fac) const
{
  return facsI_[fac->getIndex()];
}

#endif // PACKAGES_CLPBN_HORUS_BELIEFPROP_H

