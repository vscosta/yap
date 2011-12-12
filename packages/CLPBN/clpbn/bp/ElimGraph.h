#ifndef HORUS_ELIMGRAPH_H
#define HORUS_ELIMGRAPH_H

#include "unordered_map"

#include "FactorGraph.h"
#include "Shared.h"

using namespace std;

enum ElimHeuristic 
{
  MIN_NEIGHBORS,
  MIN_WEIGHT,
  MIN_FILL,
  WEIGHTED_MIN_FILL
};


class EgNode : public VarNode {
  public:
    EgNode (VarNode* var) : VarNode (var) { }
    void addNeighbor (EgNode* n)
    {
      neighs_.push_back (n);
    }

    const vector<EgNode*>& neighbors (void) const { return neighs_; }
  private:
    vector<EgNode*>  neighs_;
};


class ElimGraph
{
  public:
    ElimGraph (const BayesNet&);
   ~ElimGraph (void);
   
    void addEdge (EgNode* n1, EgNode* n2)
    {
      assert (n1 != n2);
      n1->addNeighbor (n2);
      n2->addNeighbor (n1);
    }
    void          addNode (EgNode*);
    EgNode*       getEgNode (VarId) const;
    VarIdSet      getEliminatingOrder (const VarIdSet&);
    void          printGraphicalModel (void) const;
    void          exportToGraphViz (const char*, bool = true,
                                    const VarIdSet& = VarIdSet()) const;
    void          setIndexes();

    static void setEliminationHeuristic (ElimHeuristic h)
    {
      elimHeuristic_ = h;
    }

  private:
    EgNode*       getLowestCostNode (void) const;
    unsigned      getNeighborsCost (const EgNode*) const;
    unsigned      getWeightCost (const EgNode*) const;
    unsigned      getFillCost (const EgNode*) const;
    unsigned      getWeightedFillCost (const EgNode*) const;
    void          connectAllNeighbors (const EgNode*);
    bool          neighbors (const EgNode*, const EgNode*) const;


    vector<EgNode*> nodes_;
    vector<bool>    marked_;
    unordered_map<VarId,EgNode*> vid2nodes_;
    static ElimHeuristic elimHeuristic_;
};

#endif // HORUS_ELIMGRAPH_H

