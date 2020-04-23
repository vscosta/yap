#ifndef YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_
#define YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_

#include <cassert>

#include <vector>
#include <unordered_map>

#include "FactorGraph.h"
#include "TinySet.h"
#include "Horus.h"


namespace Horus {

class ElimGraph {
  public:
    enum class ElimHeuristic {
      sequentialEh,
      minNeighborsEh,
      minWeightEh,
      minFillEh,
      weightedMinFillEh
    };

    ElimGraph (const Factors&);

   ~ElimGraph();

    VarIds getEliminatingOrder (const VarIds&);

    void print() const;

    void exportToGraphViz (const char*, bool = true,
        const VarIds& = VarIds()) const;

    static VarIds getEliminationOrder (const Factors&, VarIds);

    static ElimHeuristic elimHeuristic() { return elimHeuristic_; }

    static void setElimHeuristic (ElimHeuristic eh) { elimHeuristic_ = eh; }

  private:
    class EGNode;

    typedef TinySet<EGNode*> EGNeighs;

    class EGNode : public Var {
      public:
        EGNode (VarId vid, unsigned range) : Var (vid, range) { }

        void addNeighbor (EGNode* n) { neighs_.insert (n);  }

        void removeNeighbor (EGNode* n) { neighs_.remove (n); }

        bool isNeighbor (EGNode* n) const { return neighs_.contains (n); }

        const EGNeighs& neighbors() const { return neighs_; }

      private:
        EGNeighs neighs_;
    };

    void addEdge (EGNode* n1, EGNode* n2);

    unsigned getNeighborsCost (const EGNode* n) const;

    unsigned getWeightCost (const EGNode* n) const;

    unsigned getFillCost (const EGNode* n) const;

    unsigned getWeightedFillCost (const EGNode* n) const;

    bool neighbors (EGNode* n1, EGNode* n2) const;

    void addNode (EGNode*);

    EGNode* getEGNode (VarId) const;

    EGNode* getLowestCostNode() const;

    void connectAllNeighbors (const EGNode*);

    std::vector<EGNode*>                nodes_;
    EGNeighs                            unmarked_;
    std::unordered_map<VarId, EGNode*>  varMap_;

    static ElimHeuristic elimHeuristic_;

    DISALLOW_COPY_AND_ASSIGN (ElimGraph);
};



/* Profiling shows that we should inline the following functions */



inline void
ElimGraph::addEdge (EGNode* n1, EGNode* n2)
{
  assert (n1 != n2);
  n1->addNeighbor (n2);
  n2->addNeighbor (n1);
}



inline unsigned
ElimGraph::getNeighborsCost (const EGNode* n) const
{
  return n->neighbors().size();
}



inline unsigned
ElimGraph::getWeightCost (const EGNode* n) const
{
  unsigned cost = 1;
  const EGNeighs& neighs = n->neighbors();
  for (size_t i = 0; i < neighs.size(); i++) {
    cost *= neighs[i]->range();
  }
  return cost;
}



inline unsigned
ElimGraph::getFillCost (const EGNode* n) const
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
ElimGraph::getWeightedFillCost (const EGNode* n) const
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
ElimGraph::neighbors (EGNode* n1, EGNode* n2) const
{
  return n1->isNeighbor (n2);
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_ELIMGRAPH_H_

