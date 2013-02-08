#include <iostream>
#include <fstream>

#include "ElimGraph.h"


namespace horus {

ElimHeuristic ElimGraph::elimHeuristic_ = MIN_NEIGHBORS;


ElimGraph::ElimGraph (const std::vector<Factor*>& factors)
{
  for (size_t i = 0; i < factors.size(); i++) {
    if (factors[i]) {
      const VarIds& args = factors[i]->arguments();
      for (size_t j = 0; j < args.size() - 1; j++) {
        EgNode* n1 = getEgNode (args[j]);
        if (!n1) {
          n1 = new EgNode (args[j], factors[i]->range (j));
          addNode (n1);
        }
        for (size_t k = j + 1; k < args.size(); k++) {
          EgNode* n2 = getEgNode (args[k]);
          if (!n2) {
            n2 = new EgNode (args[k], factors[i]->range (k));
            addNode (n2);
          }
          if (!neighbors (n1, n2)) {
            addEdge (n1, n2);
          }
        }
      }
      if (args.size() == 1 && !getEgNode (args[0])) {
        addNode (new EgNode (args[0], factors[i]->range (0)));
      }
    }
  }
}



ElimGraph::~ElimGraph (void)
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
}



VarIds
ElimGraph::getEliminatingOrder (const VarIds& excludedVids)
{
  VarIds elimOrder;
  unmarked_.reserve (nodes_.size());
  for (size_t i = 0; i < nodes_.size(); i++) {
    if (util::contains (excludedVids, nodes_[i]->varId()) == false) {
      unmarked_.insert (nodes_[i]);
    }
  }
  size_t nrVarsToEliminate = nodes_.size() - excludedVids.size();
  for (size_t i = 0; i < nrVarsToEliminate; i++) {
    EgNode* node = getLowestCostNode();
    unmarked_.remove (node);
    const EGNeighs& neighs = node->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      neighs[j]->removeNeighbor (node);
    }
    elimOrder.push_back (node->varId());
    connectAllNeighbors (node);
  }
  return elimOrder;
}



void
ElimGraph::print (void) const
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    std::cout << "node " << nodes_[i]->label() << " neighs:" ;
    EGNeighs neighs = nodes_[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      std::cout << "  " << neighs[j]->label();
    }
    std::cout << std::endl;
  }
}



void
ElimGraph::exportToGraphViz (
    const char* fileName,
    bool showNeighborless,
    const VarIds& highlightVarIds) const
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    return;
  }
  out << "strict graph {" << std::endl;
  for (size_t i = 0; i < nodes_.size(); i++) {
    if (showNeighborless || nodes_[i]->neighbors().empty() == false) {
      out << '"' << nodes_[i]->label() << '"' << std::endl;
    }
  }
  for (size_t i = 0; i < highlightVarIds.size(); i++) {
    EgNode* node =getEgNode (highlightVarIds[i]);
    if (node) {
      out << '"' << node->label() << '"' ;
      out << " [shape=box3d]" << std::endl;
    } else {
      std::cerr << "Error: invalid variable id: " ;
      std::cerr << highlightVarIds[i] << "." ;
      std::cerr << std::endl;
      exit (EXIT_FAILURE);
    }
  }
  for (size_t i = 0; i < nodes_.size(); i++) {
    EGNeighs neighs = nodes_[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      out << '"' << nodes_[i]->label() << '"' << " -- " ;
      out << '"' << neighs[j]->label() << '"' << std::endl;
    }
  }
  out << "}" << std::endl;
  out.close();
}



VarIds
ElimGraph::getEliminationOrder (
    const Factors& factors,
    VarIds excludedVids)
{
  if (elimHeuristic_ == ElimHeuristic::SEQUENTIAL) {
    VarIds allVids;
    Factors::const_iterator first = factors.begin();
    Factors::const_iterator end   = factors.end();
    for (; first != end; ++first) {
      util::addToVector (allVids, (*first)->arguments());
    }
    TinySet<VarId> elimOrder (allVids);
    elimOrder -= TinySet<VarId> (excludedVids);
    return elimOrder.elements();
  }
  ElimGraph graph (factors);
  return graph.getEliminatingOrder (excludedVids);
}



void
ElimGraph::addNode (EgNode* n)
{
  nodes_.push_back (n);
  n->setIndex (nodes_.size() - 1);
  varMap_.insert (std::make_pair (n->varId(), n));
}



EgNode*
ElimGraph::getEgNode (VarId vid) const
{
  std::unordered_map<VarId, EgNode*>::const_iterator it;
  it = varMap_.find (vid);
  return (it != varMap_.end()) ? it->second : 0;
}



EgNode*
ElimGraph::getLowestCostNode (void) const
{
  EgNode* bestNode = 0;
  unsigned minCost = util::maxUnsigned();
  EGNeighs::const_iterator it;
  switch (elimHeuristic_) {
    case MIN_NEIGHBORS: {
      for (it = unmarked_.begin(); it != unmarked_.end(); ++ it) {
        unsigned cost = getNeighborsCost (*it);
        if (cost < minCost) {
          bestNode = *it;
          minCost  = cost;
        }
      }}
      break;
    case MIN_WEIGHT: {
      for (it = unmarked_.begin(); it != unmarked_.end(); ++ it) {
        unsigned cost = getWeightCost (*it);
        if (cost < minCost) {
          bestNode = *it;
          minCost  = cost;
        }
      }}
      break;
    case MIN_FILL: {
      for (it = unmarked_.begin(); it != unmarked_.end(); ++ it) {
        unsigned cost = getFillCost (*it);
        if (cost < minCost) {
          bestNode = *it;
          minCost  = cost;
        }
      }}
      break;
    case WEIGHTED_MIN_FILL: {
      for (it = unmarked_.begin(); it != unmarked_.end(); ++ it) {
        unsigned cost = getWeightedFillCost (*it);
        if (cost < minCost) {
          bestNode = *it;
          minCost  = cost;
        }
      }}
      break;
    default:
      assert (false);
  }
  assert (bestNode);
  return bestNode;
}



void
ElimGraph::connectAllNeighbors (const EgNode* n)
{
  const EGNeighs& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (size_t i = 0; i < neighs.size() - 1; i++) {
      for (size_t j = i + 1; j < neighs.size(); j++) {
        if (!neighbors (neighs[i], neighs[j])) {
          addEdge (neighs[i], neighs[j]);
        }
      }
    }
  }
}

}  // namespace horus

