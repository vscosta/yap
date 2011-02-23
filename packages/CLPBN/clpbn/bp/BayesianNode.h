#ifndef BAYESIAN_NODE_H
#define BAYESIAN_NODE_H

#include <vector>
#include <string>

#include "Distribution.h"
#include "CptEntry.h"

using namespace std;

class BayesianNode 
{
  public:
    // constructs
    BayesianNode (string, vector<BayesianNode*>, Distribution*, int = -1);
    // destruct
    ~BayesianNode (void);
    // methods
    string                     getVariableName (void) const;
    vector<BayesianNode*>      getParents (void) const;
    vector<BayesianNode*>      getChilds (void) const;
    void                       addChild (BayesianNode*);
    double*                    getParameters (void);
    double*                    getRow (int) const;
    double                     getProbability (CptEntry&);
    void                       setProbability (CptEntry&, double);
    bool                       isRoot (void);
    bool                       isLeaf (void);
    int                        getRowSize (void) const;
    int                        getCptSize (void);
    vector<string>             getDomain (void) const;
    int                        getDomainSize (void) const;
    vector<CptEntry>           getCptEntries (const vector<pair<int, int> >&);
    vector<CptEntry>           getCptEntriesOfRow (int);
    int                        getIndexOfParent (const BayesianNode*) const;
    bool                       hasEvidence (void);
    int                        getEvidence (void);
    void                       setEvidence (int);
    string                     entryToString (const CptEntry& entry) const;

  private:
    BayesianNode (const BayesianNode&);    // disallow copy
    void operator= (const BayesianNode&);  // disallow assign
    // methods
    vector<string>             getDomainHeaders (void) const;
    friend ostream&            operator << (ostream&, const BayesianNode&);
    // members
    string                     varName_;  // variable name
    vector<BayesianNode*>      parents_;  // parents of this node
    vector<BayesianNode*>      childs_;   // children of this node
    Distribution*              dist_;
    int                        evidence_;
};

ostream& operator << (ostream&, const BayesianNode&);

#endif // BAYESIAN_NODE_H

