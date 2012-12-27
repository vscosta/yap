#ifndef HORUS_BELIEFPROP_H
#define HORUS_BELIEFPROP_H

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
    BpLink (FacNode* fn, VarNode* vn)
    {
      fac_ = fn;
      var_ = vn;
      v1_.resize (vn->range(), LogAware::log (1.0 / vn->range()));
      v2_.resize (vn->range(), LogAware::log (1.0 / vn->range()));
      currMsg_   = &v1_;
      nextMsg_   = &v2_;
      residual_  = 0.0;
    }

    virtual ~BpLink (void) { };

    FacNode* facNode (void) const { return fac_; }

    VarNode* varNode (void) const { return var_; }

    const Params& message (void) const { return *currMsg_; }

    Params& nextMessage (void) { return *nextMsg_; }

    double residual (void) const { return residual_; }

    void clearResidual (void) { residual_ = 0.0; }

    void updateResidual (void)
    {
      residual_ = LogAware::getMaxNorm (v1_,v2_);
    }

    virtual void updateMessage (void)
    {
      swap (currMsg_, nextMsg_);
    }

    string toString (void) const
    {
      stringstream ss;
      ss << fac_->getLabel();
      ss << " -- " ;
      ss << var_->label();
      return ss.str();
    }

  protected:
    FacNode*  fac_;
    VarNode*  var_;
    Params    v1_;
    Params    v2_;
    Params*   currMsg_;
    Params*   nextMsg_;
    double    residual_;
};

typedef vector<BpLink*> BpLinks;


class SPNodeInfo
{
  public:
    void addBpLink (BpLink* link) { links_.push_back (link); }
    const BpLinks& getLinks (void) { return links_; }
  private:
    BpLinks links_;
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

    static MsgSchedule  schedule;
    static double       accuracy;
    static unsigned     maxIter;

  protected:
    SPNodeInfo* ninf (const VarNode* var) const
    {
      return varsI_[var->getIndex()];
    }

    SPNodeInfo* ninf (const FacNode* fac) const
    {
      return facsI_[fac->getIndex()];
    }

    void calculateAndUpdateMessage (BpLink* link, bool calcResidual = true)
    {
      if (Globals::verbosity > 2) {
        cout << "calculating & updating " << link->toString() << endl;
      }
      calcFactorToVarMsg (link);
      if (calcResidual) {
        link->updateResidual();
      }
      link->updateMessage();
    }

    void calculateMessage (BpLink* link, bool calcResidual = true)
    {
      if (Globals::verbosity > 2) {
        cout << "calculating " << link->toString() << endl;
      }
      calcFactorToVarMsg (link);
      if (calcResidual) {
        link->updateResidual();
      }
    }

    void updateMessage (BpLink* link)
    {
      link->updateMessage();
      if (Globals::verbosity > 2) {
        cout << "updating " << link->toString() << endl;
      }
    }

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

  private:
    void initializeSolver (void);

    bool converged (void);

    virtual void printLinkInformation (void) const;
};

#endif // HORUS_BELIEFPROP_H

