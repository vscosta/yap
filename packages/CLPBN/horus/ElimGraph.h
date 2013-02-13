#ifndef YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_
#define YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_

#include <cassert>

#include <vector>
#include <unordered_map>

#include "FactorGraph.h"
#include "TinySet.h"
#include "Horus.h"


namespace Horus {

class EgNode;

typedef TinySet<EgNode*> EGNeighs;


enum ElimHeuristic {
  SEQUENTIAL,
  MIN_NEIGHBORS,
  MIN_WEIGHT,
  MIN_FILL,
  WEIGHTED_MIN_FILL
};


class EgNode : public Var {
  public:
    EgNode (VarId vid, unsigned range) : Var (vid, range) { }

    void addNeighbor (EgNode* n) { neighs_.insert (n);  }

    void removeNeighbor (EgNode* n) { neighs_.remove (n); }

    bool isNeighbor (EgNode* n) const { return neighs_.contains (n); }

    const EGNeighs& neighbors (void) const { return neighs_; }

  private:
    EGNeighs neighs_;
};


class ElimGraph {
  public:
    ElimGraph (const Factors&);

   ~ElimGraph (void);

    VarIds getEliminatingOrder (const VarIds&);

    void print (void) const;

    void exportToGraphViz (const char*, bool = true,
        const VarIds& = VarIds()) const;

    static VarIds getEliminationOrder (const Factors&, VarIds);

    static ElimHeuristic elimHeuristic (void) { return elimHeuristic_; }

    static void setElimHeuristic (ElimHeuristic eh) { elimHeuristic_ = eh; }

  private:
    void addEdge (EgNode* n1, EgNode* n2);

    unsigned getNeighborsCost (const EgNode* n) const;

    unsigned getWeightCost (const EgNode* n) const;

    unsigned getFillCost (const EgNode* n) const;

    unsigned getWeightedFillCost (const EgNode* n) const;

    bool neighbors (EgNode* n1, EgNode* n2) const;

    void addNode (EgNode*);

    EgNode* getEgNode (VarId) const;

    EgNode* getLowestCostNode (void) const;

    void connectAllNeighbors (const EgNode*);

    std::vector<EgNode*>                nodes_;
    TinySet<EgNode*>                    unmarked_;
    std::unordered_map<VarId, EgNode*>  varMap_;

    static ElimHeuristic elimHeuristic_;

    DISALLOW_COPY_AND_ASSIGN (ElimGraph);
};



inline void
ElimGraph::addEdge (EgNode* n1, EgNode* n2)
{
  assert (n1 != n2);
  n1->addNeighbor (n2);
  n2->addNeighbor (n1);
}



inline unsigned
ElimGraph::getNeighborsCost (const EgNode* n) const
{
  return n->neighbors().size();
}



inline unsigned
ElimGraph::getWeightCost (const EgNode* n) const
{
  unsigned cost = 1;
  const EGNeighs& neighs = n->neighbors();
  for (size_t i = 0; i < neighs.size(); i++) {
    cost *= neighs[i]->range();
  }
  return cost;
}



inline unsigned
ElimGraph::getFillCost (const EgNode* n) const
{
  unsigned cost = 0;
  const EGNeighs& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (size_t i = 0; i < neighs.size() - 1; i++) {
      for (size_t j = i + 1; j < neighs.size(); j++) {
        if ( ! neighbors (neighs[i], neighs[j])) {
          cost ++;
        }
      }
    }
  }
  return cost;
}



inline unsigned
ElimGraph::getWeightedFillCost (const EgNode* n) const
{
  unsigned cost = 0;
  const EGNeighs& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (size_t i = 0; i < neighs.size() - 1; i++) {
      for (size_t j = i + 1; j < neighs.size(); j++) {
        if ( ! neighbors (neighs[i], neighs[j])) {
          cost += neighs[i]->range() * neighs[j]->range();
        }
      }
    }
  }
  return cost;
}



inline bool
ElimGraph::neighbors (EgNode* n1, EgNode* n2) const
{
  return n1->isNeighbor (n2);
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_

