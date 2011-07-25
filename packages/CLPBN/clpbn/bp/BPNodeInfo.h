#ifndef BP_BP_NODE_H
#define BP_BP_NODE_H

#include <vector>
#include <map>

#include "BPSolver.h"
#include "BayesNode.h"
#include "Shared.h"

//class Edge;

using namespace std;

class BPNodeInfo
{
  public:
    BPNodeInfo (int);
    BPNodeInfo (BayesNode*);

    ParamSet               getBeliefs (void) const;
    double                 getPiValue (unsigned) const;
    void                   setPiValue (unsigned, Param);
    double                 getLambdaValue (unsigned) const;
    void                   setLambdaValue (unsigned, Param);
    double                 getBeliefChange (void);
    bool                   receivedBottomInfluence (void) const;

    ParamSet& getPiValues (void)              { return piVals_; }
    ParamSet& getLambdaValues (void)          { return ldVals_; }
    bool arePiValuesCalculated (void)         { return piValsCalc_; }
    bool areLambdaValuesCalculated (void)     { return ldValsCalc_; }
    void markPiValuesAsCalculated (void)      { piValsCalc_ = true; }
    void markLambdaValuesAsCalculated (void)  { ldValsCalc_ = true; }
    void incNumPiMsgsRcv (void)               { nPiMsgsRcv_ ++;     }
    void incNumLambdaMsgsRcv (void)           { nLdMsgsRcv_ ++;     }

    bool receivedAllPiMessages (void)
    {
      return node_->getParents().size() == nPiMsgsRcv_;
    }

    bool receivedAllLambdaMessages (void)
    {
      return node_->getChilds().size() == nLdMsgsRcv_;
    }

    bool readyToSendPiMsgTo (const BayesNode*) const ;
    bool readyToSendLambdaMsgTo (const BayesNode*) const;

    CEdgeSet getIncomingParentLinks (void)  { return inParentLinks_;  }
    CEdgeSet getIncomingChildLinks (void)   { return inChildLinks_;   }
    CEdgeSet getOutcomingParentLinks (void) { return outParentLinks_; }
    CEdgeSet getOutcomingChildLinks (void)  { return outChildLinks_;  }
   
    void addIncomingParentLink  (Edge* l)   { inParentLinks_.push_back (l);  }
    void addIncomingChildLink   (Edge* l)   { inChildLinks_.push_back (l);   }
    void addOutcomingParentLink (Edge* l)   { outParentLinks_.push_back (l); }
    void addOutcomingChildLink  (Edge* l)   { outChildLinks_.push_back (l);  }
   
  private:
    DISALLOW_COPY_AND_ASSIGN (BPNodeInfo);

    ParamSet               piVals_;     // pi values
    ParamSet               ldVals_;     // lambda values
    ParamSet               oldBeliefs_;
    unsigned               nPiMsgsRcv_;
    unsigned               nLdMsgsRcv_;
    bool                   piValsCalc_;
    bool                   ldValsCalc_;
    EdgeSet                inParentLinks_;
    EdgeSet                inChildLinks_;
    EdgeSet                outParentLinks_;
    EdgeSet                outChildLinks_;
    unsigned               ds_;
    const BayesNode*       node_;
    map<const BayesNode*, bool> pmsgs_;
    map<const BayesNode*, bool> cmsgs_;
};

#endif //BP_BP_NODE_H

