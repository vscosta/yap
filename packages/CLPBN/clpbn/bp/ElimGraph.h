#ifndef HORUS_ELIMGRAPH_H
#define HORUS_ELIMGRAPH_H

#include "unordered_map"

#include "FactorGraph.h"
#include "Horus.h"

using namespace std;

enum ElimHeuristic 
{
  MIN_NEIGHBORS,
  MIN_WEIGHT,
  MIN_FILL,
  WEIGHTED_MIN_FILL
};


class EgNode : public VarNode
{
  public:
    EgNode (VarId vid, unsigned range) : VarNode (vid, range) { }

    void addNeighbor (EgNode* n) { neighs_.push_back (n);  }

    const vector<EgNode*>& neighbors (void) const { return neighs_; }

  private:
    vector<EgNode*>  neighs_;
};


class ElimGraph
{
  public:
    ElimGraph (const vector<Factor*>&); // TODO

   ~ElimGraph (void);
   
    VarIds getEliminatingOrder (const VarIds&);

    void print (void) const;

    void exportToGraphViz (const char*, bool = true,
        const VarIds& = VarIds()) const;

    static VarIds getEliminationOrder (const vector<Factor*>, VarIds);

    static void setEliminationHeuristic (ElimHeuristic h)
    {
      elimHeuristic_ = h;
    }

  private:

    void addEdge (EgNode* n1, EgNode* n2)
    {
      assert (n1 != n2);
      n1->addNeighbor (n2);
      n2->addNeighbor (n1);
    }

    void addNode (EgNode*);

    EgNode* getEgNode (VarId) const;
    EgNode* getLowestCostNode (void) const;

    unsigned getNeighborsCost (const EgNode*) const;

    unsigned getWeightCost (const EgNode*) const;

    unsigned getFillCost (const EgNode*) const;

    unsigned getWeightedFillCost (const EgNode*) const;

    void connectAllNeighbors (const EgNode*);

    bool neighbors (const EgNode*, const EgNode*) const;

    void setIndexes (void);

    vector<EgNode*> nodes_;
    vector<bool>    marked_;
    unordered_map<VarId, EgNode*> varMap_;
    static ElimHeuristic elimHeuristic_;
};

#endif // HORUS_ELIMGRAPH_H

