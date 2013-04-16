#ifndef YAP_PACKAGES_CLPBN_HORUS_BELIEFPROP_H_
#define YAP_PACKAGES_CLPBN_HORUS_BELIEFPROP_H_

#include <vector>
#include <set>
#include <string>

#include "GroundSolver.h"
#include "FactorGraph.h"


namespace Horus {

class BeliefProp : public GroundSolver {
  private:
    class SPNodeInfo;

  public:
    enum class MsgSchedule {
      seqFixedSch,
      seqRandomSch,
      parallelSch,
      maxResidualSch
    };

    BeliefProp (const FactorGraph&);

    virtual ~BeliefProp();

    Params solveQuery (VarIds);

    virtual void printSolverFlags() const;

    virtual Params getPosterioriOf (VarId);

    virtual Params getJointDistributionOf (const VarIds&);

    Params getFactorJoint (FacNode* fn, const VarIds&);

    static double accuracy() { return accuracy_; }

    static void setAccuracy (double acc) { accuracy_ = acc; }

    static unsigned maxIterations() { return maxIter_; }

    static void setMaxIterations (unsigned mi) { maxIter_ = mi; }

    static MsgSchedule msgSchedule() { return schedule_; }

    static void setMsgSchedule (MsgSchedule sch) { schedule_ = sch; }

  protected:
    class BpLink {
      public:
        BpLink (FacNode* fn, VarNode* vn);

        virtual ~BpLink() { };

        FacNode* facNode() const { return fac_; }

        VarNode* varNode() const { return var_; }

        const Params& message() const { return *currMsg_; }

        Params& nextMessage() { return *nextMsg_; }

        double residual() const { return residual_; }

        void clearResidual();

        void updateResidual();

        virtual void updateMessage();

        std::string toString() const;

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

    struct CmpResidual {
      bool operator() (const BpLink* l1, const BpLink* l2) {
        return l1->residual() > l2->residual();
    }};

    typedef std::vector<BeliefProp::BpLink*>                    BpLinks;
    typedef std::multiset<BpLink*, CmpResidual>                 SortedOrder;
    typedef std::unordered_map<BpLink*, SortedOrder::iterator>  BpLinkMap;

    BpLinks& getLinks (const VarNode* var);

    BpLinks& getLinks (const FacNode* fac);

    void calculateAndUpdateMessage (BpLink* link, bool calcResidual = true);

    void calculateMessage (BpLink* link, bool calcResidual = true);

    void updateMessage (BpLink* link);

    void runSolver();

    virtual void createLinks();

    virtual void maxResidualSchedule();

    virtual void calcFactorToVarMsg (BpLink*);

    virtual Params getVarToFactorMsg (const BpLink*);

    virtual Params getJointByConditioning (const VarIds&) const;

    BpLinks                   links_;
    unsigned                  nIters_;
    bool                      runned_;
    SortedOrder               sortedOrder_;
    BpLinkMap                 linkMap_;

    static double             accuracy_;

  private:
    void initializeSolver();

    bool converged();

    virtual void printLinkInformation() const;

    std::vector<BpLinks>      varsLinks_;
    std::vector<BpLinks>      facsLinks_;

    static unsigned           maxIter_;
    static MsgSchedule        schedule_;

    DISALLOW_COPY_AND_ASSIGN (BeliefProp);
};



inline BeliefProp::BpLinks&
BeliefProp::getLinks (const VarNode* var)
{
  return varsLinks_[var->getIndex()];
}



inline BeliefProp::BpLinks&
BeliefProp::getLinks (const FacNode* fac)
{
  return facsLinks_[fac->getIndex()];
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_BELIEFPROP_H_

