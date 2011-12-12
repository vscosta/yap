#ifndef HORUS_FACTORGRAPH_H
#define HORUS_FACTORGRAPH_H

#include <vector>

#include "GraphicalModel.h"
#include "Shared.h"
#include "Distribution.h"
#include "Factor.h"

using namespace std;


class FgFacNode;

class FgVarNode : public VarNode
{
  public:
    FgVarNode (VarId varId, unsigned nrStates) : VarNode (varId, nrStates) { }
    FgVarNode (const VarNode* v) : VarNode (v) { }
    void addNeighbor (FgFacNode* fn)
    {
      neighs_.push_back (fn);
    }
    const vector<FgFacNode*>& neighbors (void) const
    {
      return neighs_;
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (FgVarNode);
    // members
    vector<FgFacNode*> neighs_;
};


class FgFacNode
{
  public:
    FgFacNode (Factor* factor)
    {
      factor_ = factor;
      index_  = -1;
    }
    Factor* factor() const
    {
      return factor_;
    }
    void addNeighbor (FgVarNode* vn) 
    {
      neighs_.push_back (vn);
    }
    const vector<FgVarNode*>& neighbors (void) const
    {
      return neighs_;
    }
    int getIndex (void) const
    {
      assert (index_ != -1);
      return index_;
    }
    void setIndex (int index)
    {
      index_ = index;
    }
    Distribution* getDistribution (void)
    {
      return factor_->getDistribution();
    }
    const ParamSet& getParameters (void) const
    {
      return factor_->getParameters();
    }
    string getLabel (void)
    {
      return factor_->getLabel();
    }
  private:
    DISALLOW_COPY_AND_ASSIGN (FgFacNode);

    Factor* factor_;
    int index_;
    vector<FgVarNode*> neighs_;
};


class FactorGraph : public GraphicalModel
{
  public:
    FactorGraph (void) {};
    FactorGraph (const BayesNet&);
   ~FactorGraph (void);

    void                readFromUaiFormat (const char*);
    void                readFromLibDaiFormat (const char*);
    void                addVariable (FgVarNode*);
    void                addFactor (FgFacNode*);
    void                addEdge (FgVarNode*, FgFacNode*);
    void                addEdge (FgFacNode*, FgVarNode*);
    VarNode*            getVariableNode (unsigned) const;
    VarNodes            getVariableNodes (void) const;
    bool                isTree (void) const;
    void                setIndexes (void);
    void                freeDistributions (void);
    void                printGraphicalModel (void) const;
    void                exportToGraphViz (const char*) const;
    void                exportToUaiFormat (const char*) const;
    void                exportToLibDaiFormat (const char*) const;
                      
    const FgVarSet&   getVarNodes (void)    const  { return varNodes_; }
    const FgFacSet&   getFactorNodes (void) const  { return facNodes_; }

    FgVarNode* getFgVarNode (VarId vid) const
    {
      IndexMap::const_iterator it = indexMap_.find (vid);
      if (it == indexMap_.end()) {
        return 0;
      } else {
        return varNodes_[it->second];
      }
    }

  private:
    bool                containsCycle (void) const;
    bool                containsCycle (const FgVarNode*, const FgFacNode*,
                            vector<bool>&, vector<bool>&) const;
    bool                containsCycle (const FgFacNode*, const FgVarNode*,
                            vector<bool>&, vector<bool>&) const;

    DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    FgVarSet         varNodes_;
    FgFacSet         facNodes_;

    typedef unordered_map<unsigned, unsigned> IndexMap;
    IndexMap         indexMap_;
};

#endif // HORUS_FACTORGRAPH_H

