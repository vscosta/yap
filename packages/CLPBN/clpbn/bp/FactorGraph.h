#ifndef HORUS_FACTORGRAPH_H
#define HORUS_FACTORGRAPH_H

#include <vector>

#include "Factor.h"
#include "GraphicalModel.h"
#include "BayesNet.h"
#include "Horus.h"

using namespace std;


class FgFacNode;


class FgVarNode : public VarNode
{
  public:
    FgVarNode (VarId varId, unsigned nrStates) : VarNode (varId, nrStates) { }

    FgVarNode (const VarNode* v) : VarNode (v) { }

    void addNeighbor (FgFacNode* fn) { neighs_.push_back (fn); }

    const FgFacSet& neighbors (void) const { return neighs_; }

  private:
    DISALLOW_COPY_AND_ASSIGN (FgVarNode);

    FgFacSet neighs_;
};


class FgFacNode
{
  public:
    FgFacNode (const FgFacNode* fn)
    {
      factor_ = new Factor (*fn->factor());
      index_  = -1;
    }

    FgFacNode (Factor* f) : factor_(new Factor(*f)), index_(-1) { }

    FgFacNode (const Factor& f) : factor_(new Factor (f)), index_(-1) { }

    Factor* factor() const { return factor_; }

    void addNeighbor (FgVarNode* vn) { neighs_.push_back (vn); }

    const FgVarSet&  neighbors (void) const  { return neighs_; }

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
    DISALLOW_COPY_AND_ASSIGN (FgFacNode);

    Factor*   factor_;
    FgVarSet  neighs_;
    int       index_;
};


struct CompVarId
{
  bool operator() (const VarNode* vn1, const VarNode* vn2) const
  {
    return vn1->varId() < vn2->varId();
  }
};


class FactorGraph : public GraphicalModel
{
  public:
    FactorGraph (void) { }

    FactorGraph (const FactorGraph&);

    FactorGraph (const BayesNet&);

   ~FactorGraph (void);

    const FgVarSet& getVarNodes (void) const { return varNodes_; }

    const FgFacSet& getFactorNodes (void) const { return facNodes_; }

    void setFromBayesNetwork (void) { fromBayesNet_ = true; }
 
    bool isFromBayesNetwork (void) const { return fromBayesNet_ ; }

    FgVarNode* getFgVarNode (VarId vid) const
    {
      IndexMap::const_iterator it = varMap_.find (vid);
      return (it != varMap_.end()) ? varNodes_[it->second] : 0;
    }

    void readFromUaiFormat (const char*);

    void readFromLibDaiFormat (const char*);

    void addVariable (FgVarNode*);

    void addFactor (FgFacNode*);

    void addFactor (const Factor& factor);

    void addEdge (FgVarNode*, FgFacNode*);

    void addEdge (FgFacNode*, FgVarNode*);

    VarNode* getVariableNode (unsigned) const;

    VarNodes getVariableNodes (void) const;

    bool isTree (void) const;

    DAGraph& getStructure (void);

    void setIndexes (void);

    void printGraphicalModel (void) const;

    void exportToGraphViz (const char*) const;

    void exportToUaiFormat (const char*) const;

    void exportToLibDaiFormat (const char*) const;
                      
    static bool orderFactorVariables;

  private:
    // DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    bool containsCycle (void) const;

    bool containsCycle (const FgVarNode*, const FgFacNode*,
        vector<bool>&, vector<bool>&) const;

    bool containsCycle (const FgFacNode*, const FgVarNode*,
        vector<bool>&, vector<bool>&) const;

    FgVarSet  varNodes_;
    FgFacSet  facNodes_;

    bool      fromBayesNet_;
    DAGraph   structure_;

    typedef unordered_map<unsigned, unsigned> IndexMap;
    IndexMap  varMap_;
};

#endif // HORUS_FACTORGRAPH_H

