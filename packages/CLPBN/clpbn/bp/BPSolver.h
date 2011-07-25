#ifndef BP_BPSOLVER_H
#define BP_BPSOLVER_H

#include <vector>
#include <string>
#include <set>

#include "Solver.h"
#include "BayesNet.h"
#include "BpNode.h"
#include "Shared.h"

using namespace std;

class BPSolver;

static const string PI = "pi" ;
static const string LD = "ld" ;

enum MessageType {PI_MSG, LAMBDA_MSG};

class BPSolver;
struct Edge
{
  Edge (BayesNode* s, BayesNode* d, MessageType t)
  { 
    source       = s;
    destination  = d;
    type         = t;
  }
  string getId (void) const 
  {
    stringstream ss;
    type == PI_MSG ? ss << PI : ss << LD;
    ss << source->getVarId() << "." << destination->getVarId();
    return ss.str();
  }
  string toString (void) const
  {
    stringstream ss;
    type == PI_MSG ? ss << PI << "(" : ss << LD << "(" ;
    ss << source->getLabel() << " --> " ;
    ss << destination->getLabel();
    ss << ")" ;
    return ss.str();
  }
  BayesNode*        source;
  BayesNode*        destination;
  MessageType       type;
  static BPSolver*  klass;
};



/*
class BPMessage
{
  BPMessage (BayesNode* parent, BayesNode* child) 
  {
    parent_ = parent;
    child_  = child;
    currPiMsg_.resize (child->getDomainSize(),  1);
    currLdMsg_.resize (parent->getDomainSize(), 1);
    nextLdMsg_.resize (parent->getDomainSize(), 1);
    nextPiMsg_.resize (child->getDomainSize(),  1);
    piResidual_ = 1.0;
    ldResidual_ = 1.0;
  }

  Param getPiMessageValue (int idx) const
  {
    assert (idx >=0 && idx < child->getDomainSize());
    return currPiMsg_[idx];
  }

  Param getLambdaMessageValue (int idx) const
  {
    assert (idx >=0 && idx < parent->getDomainSize());
    return currLdMsg_[idx];
  }

  const ParamSet& getPiMessage (void) const
  {
    return currPiMsg_;
  }

  const ParamSet& getLambdaMessage (void) const
  {
    return currLdMsg_;
  }

  ParamSet& piNextMessageReference (void)
  {
    return nextPiMsg_;
  }

  ParamSet& lambdaNextMessageReference (const BayesNode* source)
  {
    return nextLdMsg_;
  }

  void updatePiMessage (void)
  {
    currPiMsg_ = nextPiMsg_;
    Util::normalize (currPiMsg_);
  }

  void updateLambdaMessage (void)
  {
    currLdMsg_ = nextLdMsg_;
    Util::normalize (currLdMsg_);
  }

  double getPiResidual (void)
  {
    return piResidual_;
  }

  double getLambdaResidual (void)
  {
    return ldResidual_;
  }

  void updatePiResidual (void)
  {
    piResidual_ = Util::getL1dist (currPiMsg_, nextPiMsg_);
  }

  void updateLambdaResidual (void)
  {
    ldResidual_ = Util::getL1dist (currLdMsg_, nextLdMsg_);
  }

  void clearPiResidual (void)
  {
    piResidual_ = 0.0;
  }

  void clearLambdaResidual (void)
  {
    ldResidual_ = 0.0;
  }

  BayesNode*        parent_;
  BayesNode*        child_;
  ParamSet          currPiMsg_; // current pi messages
  ParamSet          currLdMsg_; // current lambda messages
  ParamSet          nextPiMsg_;
  ParamSet          nextLdMsg_;
  Param             piResidual_;
  Param             ldResidual_;
};



class NodeInfo
{
  NodeInfo (BayesNode* node)
  {
    node_ = node;
    piVals_.resize (node->getDomainSize(), 1);
    ldVals_.resize (node->getDomainSize(), 1);
  }

  ParamSet getBeliefs (void) const
  {
    double sum = 0.0;
    ParamSet beliefs (node_->getDomainSize());
    for (int xi = 0; xi < node_->getDomainSize(); xi++) {
      double prod = piVals_[xi] * ldVals_[xi];
      beliefs[xi] = prod;
      sum += prod;
    }
    assert (sum);
    //normalize the beliefs
    for (int xi = 0; xi < node_->getDomainSize(); xi++) {
      beliefs[xi] /= sum;
    }
    return beliefs;
  }

  double getPiValue (int idx) const
  {
    assert (idx >=0 && idx < node_->getDomainSize());
    return piVals_[idx];
  }

  void setPiValue (int idx, double value)
  {
    assert (idx >=0 && idx < node_->getDomainSize());
    piVals_[idx] = value;
  }

  double getLambdaValue (int idx) const
  {
    assert (idx >=0 && idx < node_->getDomainSize());
    return ldVals_[idx];
  }

  void setLambdaValue (int idx, double value)
  {
    assert (idx >=0 && idx < node_->getDomainSize());
    ldVals_[idx] = value;
  }

  ParamSet& getPiValues (void)
  {
    return piVals_;
  }

  ParamSet& getLambdaValues (void)
  {
    return ldVals_;
  }

  double getBeliefChange (void)
  {
    double change = 0.0;
    if (oldBeliefs_.size() == 0) {
      oldBeliefs_ = getBeliefs();
      change = MAX_CHANGE_;
    } else {
      ParamSet currentBeliefs = getBeliefs();
      for (int xi = 0; xi < node_->getDomainSize(); xi++) {
        change += abs (currentBeliefs[xi] - oldBeliefs_[xi]);
      }
      oldBeliefs_ = currentBeliefs;
    }
    return change;
  }

  bool hasReceivedChildInfluence (void) const
  {
    // if all lambda values are equal, then neither
    // this node neither its descendents have evidence,
    // we can use this to don't send lambda messages his parents
    bool childInfluenced = false;
    for (int xi = 1; xi < node_->getDomainSize(); xi++) {
      if (ldVals_[xi] != ldVals_[0]) {
        childInfluenced = true;
        break;
      }
    }
    return childInfluenced;
  }

  BayesNode*             node_;
  ParamSet               piVals_;     // pi values
  ParamSet               ldVals_;     // lambda values
  ParamSet               oldBeliefs_;
};
*/


bool compareResidual (const Edge&, const Edge&);

class BPSolver : public Solver
{
  public:
    BPSolver (const BayesNet&);
   ~BPSolver (void);

    void                runSolver (void);
    ParamSet            getPosterioriOf (const Variable* var) const;
    ParamSet            getJointDistribution (const NodeSet&) const;

  private:
    DISALLOW_COPY_AND_ASSIGN (BPSolver);

    void                initializeSolver (void);
    void                incorporateEvidence (BayesNode*);
    void                runPolyTreeSolver (void);
    void                polyTreePiMessage (BayesNode*, BayesNode*);
    void                polyTreeLambdaMessage (BayesNode*, BayesNode*);
    void                runGenericSolver (void);
    void                maxResidualSchedule (void);
    bool                converged (void) const;
    void                updatePiValues (BayesNode*);
    void                updateLambdaValues (BayesNode*);
    void                calculateNextPiMessage (BayesNode*, BayesNode*);
    void                calculateNextLambdaMessage (BayesNode*, BayesNode*);
    void                printMessageStatusOf (const BayesNode*) const;
    void                printAllMessageStatus (void) const;
    // inlines
    void                updatePiMessage (BayesNode*, BayesNode*);
    void                updateLambdaMessage (BayesNode*, BayesNode*);
    void                calculateNextMessage (const Edge&);
    void                updateMessage (const Edge&);
    void                updateValues (const Edge&);
    double              getResidual (const Edge&) const;
    void                updateResidual (const Edge&);
    void                clearResidual (const Edge&);
    BpNode*             M (const BayesNode*) const;
    friend bool         compareResidual (const Edge&, const Edge&);

    const BayesNet*     bn_;
    vector<BpNode*>     msgs_;
    Schedule            schedule_;
    int                 nIter_;
    int                 maxIter_;
    double              accuracy_;
    vector<Edge>        updateOrder_;
    bool                forceGenericSolver_;

    struct compare
    {
      inline bool operator() (const Edge& e1, const Edge& e2)
      {
        return compareResidual (e1, e2);
      }
    };

    typedef multiset<Edge, compare> SortedOrder;
    SortedOrder sortedOrder_;

    typedef unordered_map<string, SortedOrder::iterator> EdgeMap;
    EdgeMap edgeMap_;

};



inline void
BPSolver::updatePiMessage (BayesNode* source, BayesNode* destination)
{
  M(source)->updatePiMessage(destination);
}



inline void
BPSolver::updateLambdaMessage (BayesNode* source, BayesNode* destination)
{
  M(destination)->updateLambdaMessage(source);
}



inline void
BPSolver::calculateNextMessage (const Edge& e)
{
  if (DL >= 1) {
    cout << "calculating " << e.toString() << endl;
  }
  if (e.type == PI_MSG) {
    calculateNextPiMessage (e.source, e.destination);
  } else {
    calculateNextLambdaMessage (e.source, e.destination);
  }
}



inline void
BPSolver::updateMessage (const Edge& e)
{
  if (DL >= 1) {
      cout << "updating " << e.toString() << endl;
    }
  if (e.type == PI_MSG) {
    M(e.source)->updatePiMessage(e.destination);
  } else {
    M(e.destination)->updateLambdaMessage(e.source);
  }
}



inline void
BPSolver::updateValues (const Edge& e)
{
  if (!e.destination->hasEvidence()) {
    if (e.type == PI_MSG) {
      updatePiValues (e.destination);
    } else {
      updateLambdaValues (e.destination);
    }
  }
}



inline double
BPSolver::getResidual (const Edge& e) const
{
  if (e.type == PI_MSG) {
    return M(e.source)->getPiResidual(e.destination);
  } else {
    return M(e.destination)->getLambdaResidual(e.source);
  }
}



inline void
BPSolver::updateResidual (const Edge& e)
{
  if (e.type == PI_MSG) {
    M(e.source)->updatePiResidual(e.destination);
  } else {
    M(e.destination)->updateLambdaResidual(e.source);
  }
}



inline void
BPSolver::clearResidual (const Edge& e)
{
  if (e.type == PI_MSG) {
    M(e.source)->clearPiResidual(e.destination);
  } else {
    M(e.destination)->clearLambdaResidual(e.source);
  }
}



inline bool
compareResidual (const Edge& e1, const Edge& e2)
{
  double residual1;
  double residual2;
  if (e1.type == PI_MSG) {
    residual1 = Edge::klass->M(e1.source)->getPiResidual(e1.destination);
  } else {
    residual1 = Edge::klass->M(e1.destination)->getLambdaResidual(e1.source);
  }
  if (e2.type == PI_MSG) {
    residual2 = Edge::klass->M(e2.source)->getPiResidual(e2.destination);
  } else {
    residual2 = Edge::klass->M(e2.destination)->getLambdaResidual(e2.source);
  }
  return residual1 > residual2;
}



inline BpNode*
BPSolver::M (const BayesNode* node) const
{
  assert (node);
  assert (node == bn_->getNode (node->getVarId()));
  assert (node->getIndex() < msgs_.size());
  return msgs_[node->getIndex()];
}


#endif

