#ifndef HORUS_ELIMGRAPH_H
#define HORUS_ELIMGRAPH_H

#include "unordered_map"

#include "FactorGraph.h"
#include "TinySet.h"
#include "Horus.h"


using namespace std;

enum ElimHeuristic 
{
  MIN_NEIGHBORS,
  MIN_WEIGHT,
  MIN_FILL,
  WEIGHTED_MIN_FILL
};


class EgNode;

typedef TinySet<EgNode*> EGNeighs;


class EgNode : public Var
{
  public:
    EgNode (VarId vid, unsigned range) : Var (vid, range) { }

    void addNeighbor (EgNode* n) { neighs_.insert (n);  }

    void removeNeighbor (EgNode* n) { neighs_.remove (n); }

    bool isNeighbor (EgNode* n) const { return neighs_.contains (n); }

    const EGNeighs& neighbors (void) const { return neighs_; }

  private:
    EGNeighs neighs_;
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

    static ElimHeuristic elimHeuristic;

  private:

    void addEdge (EgNode* n1, EgNode* n2)
    {
      assert (n1 != n2);
      n1->addNeighbor (n2);
      n2->addNeighbor (n1);
    }

    unsigned getNeighborsCost (const EgNode* n) const
    {
      return n->neighbors().size();
    }

    unsigned getWeightCost (const EgNode* n) const
    {
      unsigned cost = 1;
      const EGNeighs& neighs = n->neighbors();
      for (unsigned i = 0; i < neighs.size(); i++) {
        cost *= neighs[i]->range();
      }
      return cost;
    }

    unsigned getFillCost (const EgNode* n) const
    {
      unsigned cost = 0;
      const EGNeighs& neighs = n->neighbors();
      if (neighs.size() > 0) {
        for (unsigned i = 0; i < neighs.size() - 1; i++) {
          for (unsigned j = i + 1; j < neighs.size(); j++) {
            if ( ! neighbors (neighs[i], neighs[j])) {
              cost ++;
            }
         }
       }
     }
     return cost;
   }

    unsigned getWeightedFillCost (const EgNode* n) const
    {
      unsigned cost = 0;
      const EGNeighs& neighs = n->neighbors();
      if (neighs.size() > 0) {
        for (unsigned i = 0; i < neighs.size() - 1; i++) {
          for (unsigned j = i+1; j < neighs.size(); j++) {
            if ( ! neighbors (neighs[i], neighs[j])) {
              cost += neighs[i]->range() * neighs[j]->range();
            }
          }
        }
      }
      return cost;
    }

    bool neighbors (EgNode* n1, EgNode* n2) const
    {
      return n1->isNeighbor (n2);
    }

    void addNode (EgNode*);

    EgNode* getEgNode (VarId) const;

    EgNode* getLowestCostNode (void) const;

    void connectAllNeighbors (const EgNode*);

    vector<EgNode*> nodes_;
    TinySet<EgNode*> unmarked_;
    unordered_map<VarId, EgNode*> varMap_;
};

#endif // HORUS_ELIMGRAPH_H

