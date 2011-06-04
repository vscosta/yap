#ifndef BP_BPNODE_H
#define BP_BPNODE_H

#include <vector>
#include <map>
#include <string>
#include <unordered_map>

#include "BayesNode.h"
#include "Shared.h"

using namespace std;

class BpNode
{
  public:
    BpNode (int);
    BpNode (BayesNode*);

    ParamSet               getBeliefs (void) const;
    double                 getPiValue (int) const;
    void                   setPiValue (int, double);
    double                 getLambdaValue (int) const;
    void                   setLambdaValue (int, double);
    ParamSet&              getPiValues (void);
    ParamSet&              getLambdaValues (void);
    double                 getPiMessageValue (const BayesNode*, int) const;
    double                 getLambdaMessageValue (const BayesNode*, int) const;
    const ParamSet&        getPiMessage (const BayesNode*) const;
    const ParamSet&        getLambdaMessage (const BayesNode*) const;
    ParamSet&              piNextMessageReference (const BayesNode*);
    ParamSet&              lambdaNextMessageReference (const BayesNode*);
    void                   updatePiMessage (const BayesNode*);
    void                   updateLambdaMessage (const BayesNode*);
    double                 getBeliefChange (void);
    void                   updatePiResidual (const BayesNode*);
    void                   updateLambdaResidual (const BayesNode*);
    void                   clearPiResidual (const BayesNode*);
    void                   clearLambdaResidual (const BayesNode*);
    bool                   hasReceivedChildInfluence (void) const;
    // inlines
    double                 getPiResidual (const BayesNode*);
    double                 getLambdaResidual (const BayesNode*);
    int                    getIndex (const BayesNode*) const;
 
  private:
    DISALLOW_COPY_AND_ASSIGN (BpNode);

    IndexMap               indexMap_;
    ParamSet               piVals_;     // pi values
    ParamSet               ldVals_;     // lambda values
    vector<ParamSet>       currPiMsgs_; // current pi messages
    vector<ParamSet>       currLdMsgs_; // current lambda messages
    vector<ParamSet>       nextPiMsgs_;
    vector<ParamSet>       nextLdMsgs_;
    ParamSet               oldBeliefs_;
    ParamSet               piResiduals_;
    ParamSet               ldResiduals_;
    int                    ds_;
    const NodeSet*         childs_;
    static bool            calculateMessageResidual_;
//    static const double    MAX_CHANGE_ = 10000000.0;
};



inline double
BpNode::getPiResidual (const BayesNode* destination)
{
  return piResiduals_[getIndex(destination)];
}


inline double
BpNode::getLambdaResidual (const BayesNode* source)
{
  return ldResiduals_[getIndex(source)];
}



inline int
BpNode::getIndex (const BayesNode* node) const
{
  assert (node);
  //assert (indexMap_.find(node->getVarId()) != indexMap_.end());
  //return indexMap_.find(node->getVarId())->second;
  for (unsigned i = 0; childs_->size(); i++) {
    if ((*childs_)[i]->getVarId() == node->getVarId()) {
      return i;
    }
  }
  assert (false);
  return -1;
}


#endif

