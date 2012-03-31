#ifndef HORUS_BAYESNODE_H
#define HORUS_BAYESNODE_H

#include <vector>

#include "VarNode.h"
#include "Horus.h"

using namespace std;


class BayesNode : public VarNode
{
  public:

    BayesNode (const VarNode& v) : VarNode (v) { }

    BayesNode (const BayesNode* n) : 
        VarNode (n->varId(), n->nrStates(), n->getEvidence()),
        params_(n->params()), distId_(n->distId()) { }

    BayesNode (VarId vid, unsigned nrStates, int ev,
               const Params& ps, unsigned id)
      : VarNode (vid, nrStates, ev) , params_(ps), distId_(id) { }
      
    const BnNodeSet& getParents (void) const { return parents_; }

    const BnNodeSet& getChilds  (void) const { return childs_;  }
   
    const Params& params (void) const { return params_; }

    unsigned distId (void) const { return distId_; }

    unsigned getRowSize (void) const
    { 
      return params_.size() / nrStates();
    }

    double getProbability (int row, unsigned col)
    {
      int idx = (row * getRowSize()) + col;
      return params_[idx];
    }

    void setParams (const Params& params);

    void setParents (const BnNodeSet&);

    void addChild (BayesNode*);

    const Params& getParameters (void);

    Params getRow (int) const;

    bool isRoot (void);

    bool isLeaf (void);

    bool hasNeighbors (void) const;

    int getCptSize (void);

    int indexOfParent (const BayesNode*) const;

    string cptEntryToString (int, const vector<unsigned>&) const;

    friend ostream& operator << (ostream&, const BayesNode&);

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNode);

    States getDomainHeaders (void) const;

    BnNodeSet  parents_;
    BnNodeSet  childs_;
    Params     params_;
    unsigned   distId_;
};

#endif // HORUS_BAYESNODE_H

