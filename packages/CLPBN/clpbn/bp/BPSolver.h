#ifndef BP_BP_SOLVER_H
#define BP_BP_SOLVER_H

#include <vector>
#include <set>

#include "Solver.h"
#include "BayesNet.h"
#include "BPNodeInfo.h"
#include "Shared.h"

using namespace std;

class BPNodeInfo;

static const string PI = "pi" ;
static const string LD = "ld" ;


enum MessageType {PI_MSG, LAMBDA_MSG};
enum JointCalcType {CHAIN_RULE, JUNCTION_NODE};

class Edge
{
  public:
    Edge (BayesNode* s, BayesNode* d, MessageType t)
    { 
      source_ = s;
      destin_ = d;
      type_   = t;
      if (type_ == PI_MSG) {
        currMsg_.resize (s->getDomainSize(), 1);
        nextMsg_.resize (s->getDomainSize(), 1);
      } else {
        currMsg_.resize (d->getDomainSize(), 1);
        nextMsg_.resize (d->getDomainSize(), 1);
      }
      msgSended_ = false;
      residual_ = 0.0;
    }
  
    //void setMessage (ParamSet msg)
    //{
    //  Util::normalize (msg);
    //  residual_ = Util::getMaxNorm (currMsg_, msg);
    //  currMsg_ = msg;
    //}

    void setNextMessage (CParamSet msg)
    {
      nextMsg_ = msg;
      Util::normalize (nextMsg_);
      residual_ = Util::getMaxNorm (currMsg_, nextMsg_);
    }

    void updateMessage (void)
    {
      currMsg_ = nextMsg_;
      if (DL >= 3) {
        cout << "updating " << toString() << endl;
      }
      msgSended_ = true;
    }
   
    void updateResidual (void)
    {
      residual_ = Util::getMaxNorm (currMsg_, nextMsg_);
    }

    string toString (void) const
    {
      stringstream ss;
      if (type_ == PI_MSG) {
        ss << PI;
      } else if (type_ == LAMBDA_MSG) {
        ss << LD;
      } else {
        abort();
      }
      ss << "(" << source_->getLabel();
      ss << " --> " << destin_->getLabel() << ")" ;
      return ss.str();
    }

    BayesNode*   getSource (void) const        { return source_; }
    BayesNode*   getDestination (void) const   { return destin_; }
    MessageType  getMessageType (void) const   { return type_; }
    CParamSet    getMessage (void) const       { return currMsg_; }
    bool         messageWasSended (void) const { return msgSended_; }
    double       getResidual (void) const      { return residual_; }
    void         clearResidual (void)          { residual_ = 0.0; }
 
  private:
    BayesNode*   source_;
    BayesNode*   destin_;
    MessageType  type_;
    ParamSet     currMsg_;
    ParamSet     nextMsg_;
    bool         msgSended_;
    double       residual_;
};


class BPSolver : public Solver
{
  public:
    BPSolver (const BayesNet&);
   ~BPSolver (void);

    void                runSolver (void);
    ParamSet            getPosterioriOf (Vid) const;
    ParamSet            getJointDistributionOf (const VidSet&);
  

  private:
    DISALLOW_COPY_AND_ASSIGN (BPSolver);

    void                initializeSolver (void);
    void                runPolyTreeSolver (void);
    void                runLoopySolver (void);
    void                maxResidualSchedule (void);
    bool                converged (void) const;
    void                updatePiValues (BayesNode*);
    void                updateLambdaValues (BayesNode*);
    ParamSet            calculateNextLambdaMessage (Edge* edge);
    ParamSet            calculateNextPiMessage (Edge* edge);
    ParamSet            getJointByJunctionNode (const VidSet&) const;
    ParamSet            getJointByChainRule (const VidSet&) const;
    void                printMessageStatusOf (const BayesNode*) const;
    void                printAllMessageStatus (void) const;

    ParamSet getMessage (Edge* edge)
    {
      if (DL >= 3) {
        cout << "    calculating " << edge->toString() << endl;
      }
      if (edge->getMessageType() == PI_MSG) {
        return calculateNextPiMessage (edge);
      } else if (edge->getMessageType() == LAMBDA_MSG) {
        return calculateNextLambdaMessage (edge);
      } else {
        abort();
      }
      return ParamSet();
    }

    void updateValues (Edge* edge)
    {
      if (!edge->getDestination()->hasEvidence()) {
        if (edge->getMessageType() == PI_MSG) {
          updatePiValues (edge->getDestination());
        } else if (edge->getMessageType() == LAMBDA_MSG) {
          updateLambdaValues (edge->getDestination());
        } else {
          abort();
        }
      }
    }

    BPNodeInfo* M (const BayesNode* node) const
    {
      assert (node);
      assert (node == bn_->getBayesNode (node->getVarId()));
      assert (node->getIndex() < nodesI_.size());
      return nodesI_[node->getIndex()];
    }

    const BayesNet*     bn_;
    vector<BPNodeInfo*> nodesI_;
    unsigned            nIter_;
    vector<Edge*>       links_;
    bool                useAlwaysLoopySolver_;
    JointCalcType       jointCalcType_;

    struct compare
    {
      inline bool operator() (const Edge* e1, const Edge* e2)
      {
        return e1->getResidual() > e2->getResidual();
      }
    };

    typedef multiset<Edge*, compare> SortedOrder;
    SortedOrder sortedOrder_;

    typedef map<Edge*, SortedOrder::iterator> EdgeMap;
    EdgeMap edgeMap_;

};

#endif //BP_BP_SOLVER_H

