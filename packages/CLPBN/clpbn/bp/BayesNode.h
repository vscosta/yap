#ifndef BP_BAYESNODE_H
#define BP_BAYESNODE_H

#include <vector>
#include <string>
#include <sstream>

#include "Variable.h"
#include "CptEntry.h"
#include "Distribution.h"
#include "Shared.h"

using namespace std;


class BayesNode : public Variable
{
  public:
    BayesNode (unsigned);
    BayesNode (unsigned, unsigned, int, const NodeSet&, Distribution*);
    BayesNode (unsigned, string, const Domain&, const NodeSet&, Distribution*);

    void                     setData (unsigned, int, const NodeSet&, Distribution*);
    void                     addChild (BayesNode*);
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
    // inlines
    const NodeSet&           getParents (void) const;
    const NodeSet&           getChilds (void) const;
    double                   getProbability (int, const CptEntry& entry);
    unsigned                 getRowSize (void) const;

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNode);

    Domain                   getDomainHeaders (void) const;
    friend ostream&          operator << (ostream&, const BayesNode&);

    NodeSet                  parents_;
    NodeSet                  childs_;
    Distribution*            dist_;
};

ostream& operator << (ostream&, const BayesNode&);



inline const NodeSet&
BayesNode::getParents (void) const
{
  return parents_;
}



inline const NodeSet&
BayesNode::getChilds (void) const
{
  return childs_;
}



inline double
BayesNode::getProbability (int row, const CptEntry& entry)
{
  int col = entry.getParameterIndex();
  int idx = (row * getRowSize()) + col;
  return dist_->params[idx];
}



inline unsigned 
BayesNode::getRowSize (void) const
{ 
  return dist_->params.size() / getDomainSize();
}

#endif

