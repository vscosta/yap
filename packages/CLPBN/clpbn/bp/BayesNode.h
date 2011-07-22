#ifndef BP_BAYES_NODE_H
#define BP_BAYES_NODE_H

#include <vector>

#include "Variable.h"
#include "CptEntry.h"
#include "Distribution.h"
#include "Shared.h"

using namespace std;


class BayesNode : public Variable
{
  public:
    BayesNode (Vid vid) : Variable (vid) {}
    BayesNode (Vid, unsigned, int, const BnNodeSet&, Distribution*);
    BayesNode (Vid, string, const Domain&, const BnNodeSet&, Distribution*);

    void                     setData (unsigned, int, const BnNodeSet&,
                                      Distribution*);
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

    const BnNodeSet& getParents (void) const { return parents_; }
    const BnNodeSet& getChilds (void) const  { return childs_; }

    unsigned getRowSize (void) const
    { 
      return dist_->params.size() / getDomainSize();
    }

    double getProbability (int row, const CptEntry& entry)
    {
      int col = entry.getParameterIndex();
      int idx = (row * getRowSize()) + col;
      return dist_->params[idx];
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNode);

    Domain                   getDomainHeaders (void) const;
    friend ostream&          operator << (ostream&, const BayesNode&);

    BnNodeSet                parents_;
    BnNodeSet                childs_;
    Distribution*            dist_;
};

ostream& operator << (ostream&, const BayesNode&);

#endif //BP_BAYES_NODE_H

