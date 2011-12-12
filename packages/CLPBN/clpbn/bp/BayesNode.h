#ifndef HORUS_BAYESNODE_H
#define HORUS_BAYESNODE_H

#include <vector>

#include "VarNode.h"
#include "CptEntry.h"
#include "Distribution.h"
#include "Shared.h"

using namespace std;


class BayesNode : public VarNode
{
  public:
    BayesNode (const VarNode& v) : VarNode (v) {}
    BayesNode (VarId, unsigned, int, Distribution*);
    BayesNode (VarId, unsigned, int, const BnNodeSet&, Distribution*);

    void                     setParents (const BnNodeSet&);
    void                     addChild (BayesNode*);
    void                     setDistribution (Distribution*);
    Distribution*            getDistribution (void);
    const ParamSet&          getParameters (void);
    ParamSet                 getRow (int) const;
    void                     setProbability (int, const CptEntry&, double);
    bool                     isRoot (void);
    bool                     isLeaf (void);
    bool                     hasNeighbors (void) const;
    int                      getCptSize (void);
    const vector<CptEntry>&  getCptEntries (void);
    int                      getIndexOfParent (const BayesNode*) const;
    string                   cptEntryToString (const CptEntry&) const;
    string                   cptEntryToString (int, const CptEntry&) const;

    const BnNodeSet& getParents (void) const  { return parents_; }
    const BnNodeSet& getChilds  (void) const  { return childs_;  }

    unsigned getRowSize (void) const
    { 
      return dist_->params.size() / nrStates();
    }

    double getProbability (int row, const CptEntry& entry)
    {
      int col = entry.getParameterIndex();
      int idx = (row * getRowSize()) + col;
      return dist_->params[idx];
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNode);

    States                   getDomainHeaders (void) const;
    friend ostream&          operator << (ostream&, const BayesNode&);

    BnNodeSet                parents_;
    BnNodeSet                childs_;
    Distribution*            dist_;
};

ostream& operator << (ostream&, const BayesNode&);

#endif // HORUS_BAYESNODE_H

