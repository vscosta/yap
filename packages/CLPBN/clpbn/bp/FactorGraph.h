#ifndef HORUS_FACTORGRAPH_H
#define HORUS_FACTORGRAPH_H

#include <vector>

#include "Factor.h"
#include "BayesNet.h"
#include "Horus.h"

using namespace std;


class FacNode;

class VarNode : public Var
{
  public:
    VarNode (VarId varId, unsigned nrStates) 
        : Var (varId, nrStates) { }

    VarNode (const Var* v) : Var (v) { }

    void addNeighbor (FacNode* fn) { neighs_.push_back (fn); }

    const FacNodes& neighbors (void) const { return neighs_; }

  private:
    DISALLOW_COPY_AND_ASSIGN (VarNode);

    FacNodes neighs_;
};


class FacNode
{
  public:
    FacNode (const Factor& f) : factor_(f), index_(-1) { }

    const Factor& factor (void) const { return factor_; }

    Factor& factor (void) { return factor_; }

    void addNeighbor (VarNode* vn) { neighs_.push_back (vn); }

    const VarNodes& neighbors (void) const { return neighs_; }

    int getIndex (void) const { return index_; }

    void setIndex (int index) { index_ = index; }

    string getLabel (void) { return factor_.getLabel(); }

  private:
    DISALLOW_COPY_AND_ASSIGN (FacNode);

    VarNodes  neighs_;
    Factor    factor_;
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
    FactorGraph (bool fbn = false) : fromBayesNet_(fbn) { }

    FactorGraph (const FactorGraph&);

   ~FactorGraph (void);

    const VarNodes& varNodes (void) const { return varNodes_; }

    const FacNodes& facNodes (void) const { return facNodes_; }
 
    bool isFromBayesNetwork (void) const { return fromBayesNet_ ; }

    unsigned nrVarNodes (void) const { return varNodes_.size(); }

    unsigned nrFacNodes (void) const { return facNodes_.size(); }

    VarNode* getVarNode (VarId vid) const
    {
      VarMap::const_iterator it = varMap_.find (vid);
      return it != varMap_.end() ? it->second : 0;
    }

    void readFromUaiFormat (const char*);

    void readFromLibDaiFormat (const char*);

    void addFactor (const Factor& factor);

    void addVarNode (VarNode*);

    void addFacNode (FacNode*);

    void addEdge (VarNode*, FacNode*);

    bool isTree (void) const;

    DAGraph& getStructure (void);

    void print (void) const;

    void exportToGraphViz (const char*) const;

    void exportToUaiFormat (const char*) const;

    void exportToLibDaiFormat (const char*) const;
                      
    static bool orderVars;

  private:
    // DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    void ignoreLines (std::ifstream&) const;

    bool containsCycle (void) const;

    bool containsCycle (const VarNode*, const FacNode*,
        vector<bool>&, vector<bool>&) const;

    bool containsCycle (const FacNode*, const VarNode*,
        vector<bool>&, vector<bool>&) const;

    VarNodes  varNodes_;
    FacNodes  facNodes_;

    DAGraph   structure_;
    bool      fromBayesNet_;

    typedef unordered_map<unsigned, VarNode*> VarMap;
    VarMap varMap_;
};

#endif // HORUS_FACTORGRAPH_H

