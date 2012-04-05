#ifndef HORUS_FACTORGRAPH_H
#define HORUS_FACTORGRAPH_H

#include <vector>

#include "Factor.h"
#include "BayesNet.h"
#include "Horus.h"

using namespace std;


class FactorNode;


class VarNode : public Var
{
  public:
    VarNode (VarId varId, unsigned nrStates) : Var (varId, nrStates) { }

    VarNode (const Var* v) : Var (v) { }

    void addNeighbor (FactorNode* fn) { neighs_.push_back (fn); }

    const FactorNodes& neighbors (void) const { return neighs_; }

  private:
    DISALLOW_COPY_AND_ASSIGN (VarNode);

    FactorNodes neighs_;
};


class FactorNode
{
  public:
    FactorNode (const FactorNode* fn)
    {
      factor_ = new Factor (*fn->factor());
      index_  = -1;
    }

    FactorNode (Factor* f) : factor_(new Factor(*f)), index_(-1) { }

    FactorNode (const Factor& f) : factor_(new Factor (f)), index_(-1) { }

    Factor* factor() const { return factor_; }

    void addNeighbor (VarNode* vn) { neighs_.push_back (vn); }

    const VarNodes&  neighbors (void) const  { return neighs_; }

    int getIndex (void) const
    {
      assert (index_ != -1);
      return index_;
    }

    void setIndex (int index)
    {
      index_ = index;
    }

    const Params& params (void) const
    {
      return factor_->params();
    }

    string getLabel (void)
    {
      return factor_->getLabel();
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (FactorNode);

    Factor*   factor_;
    VarNodes  neighs_;
    int       index_;
};


struct CompVarId
{
  bool operator() (const Var* v1, const Var* v2) const
  {
    return v1->varId() < v2->varId();
  }
};


class FactorGraph
{
  public:
    FactorGraph (void) { }

    FactorGraph (const FactorGraph&);

   ~FactorGraph (void);

    const VarNodes& varNodes (void) const { return varNodes_; }

    const FactorNodes& factorNodes (void) const { return facNodes_; }

    void setFromBayesNetwork (void) { fromBayesNet_ = true; }
 
    bool isFromBayesNetwork (void) const { return fromBayesNet_ ; }

    VarNode* getVarNode (VarId vid) const
    {
      IndexMap::const_iterator it = varMap_.find (vid);
      return (it != varMap_.end()) ? varNodes_[it->second] : 0;
    }

    void readFromUaiFormat (const char*);

    void readFromLibDaiFormat (const char*);

    void addVariable (VarNode*);

    void addFactor (FactorNode*);

    void addFactor (const Factor& factor);

    void addEdge (VarNode*, FactorNode*);

    void addEdge (FactorNode*, VarNode*);

    bool isTree (void) const;

    DAGraph& getStructure (void);

    void setIndexes (void);

    void print (void) const;

    void exportToGraphViz (const char*) const;

    void exportToUaiFormat (const char*) const;

    void exportToLibDaiFormat (const char*) const;
                      
    static bool orderFactorVariables;

  private:
    // DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    bool containsCycle (void) const;

    bool containsCycle (const VarNode*, const FactorNode*,
        vector<bool>&, vector<bool>&) const;

    bool containsCycle (const FactorNode*, const VarNode*,
        vector<bool>&, vector<bool>&) const;

    VarNodes  varNodes_;
    FactorNodes  facNodes_;

    bool      fromBayesNet_;
    DAGraph   structure_;

    typedef unordered_map<unsigned, unsigned> IndexMap;
    IndexMap  varMap_;
};

#endif // HORUS_FACTORGRAPH_H

