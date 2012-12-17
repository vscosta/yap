#include <limits>

#include <fstream>

#include "ElimGraph.h"

ElimHeuristic ElimGraph::elimHeuristic = MIN_NEIGHBORS;


ElimGraph::ElimGraph (const vector<Factor*>& factors)
{
  for (size_t i = 0; i < factors.size(); i++) {
    if (factors[i] == 0) { // if contained just one var with evidence
      continue;
    }
    const VarIds& vids = factors[i]->arguments();
    for (size_t j = 0; j < vids.size() - 1; j++) {
      EgNode* n1 = getEgNode (vids[j]);
      if (n1 == 0) {
        n1 = new EgNode (vids[j], factors[i]->range (j));
        addNode (n1);
      }
      for (size_t k = j + 1; k < vids.size(); k++) {
        EgNode* n2 = getEgNode (vids[k]);
        if (n2 == 0) {
          n2 = new EgNode (vids[k], factors[i]->range (k));
          addNode (n2);
        }
        if (neighbors (n1, n2) == false) {
          addEdge (n1, n2);
        } 
      }
    }
    if (vids.size() == 1) {
      if (getEgNode (vids[0]) == 0) {
        addNode (new EgNode (vids[0], factors[i]->range (0)));
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
ElimGraph::getEliminatingOrder (const VarIds& exclude)
{
  VarIds elimOrder;
  unmarked_.reserve (nodes_.size());
  for (size_t i = 0; i < nodes_.size(); i++) {
    if (Util::contains (exclude, nodes_[i]->varId()) == false) {
      unmarked_.insert (nodes_[i]);
    }
  }
  size_t nrVarsToEliminate = nodes_.size() - exclude.size();
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
    cout << "node " << nodes_[i]->label() << " neighs:" ;
    EGNeighs neighs = nodes_[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      cout << "  " << neighs[j]->label();
    }
    cout << endl;
  }  
}



void
ElimGraph::exportToGraphViz (
    const char* fileName,
    bool showNeighborless,
    const VarIds& highlightVarIds) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "Markov::exportToDotFile()" << endl;
    abort();
  }

  out << "strict graph {" << endl;

  for (size_t i = 0; i < nodes_.size(); i++) {
    if (showNeighborless || nodes_[i]->neighbors().size() != 0) {
      out << '"' << nodes_[i]->label() << '"' << endl;
    }
  }

  for (size_t i = 0; i < highlightVarIds.size(); i++) {
    EgNode* node =getEgNode (highlightVarIds[i]);
    if (node) {
      out << '"' << node->label() << '"' ;
      out << " [shape=box3d]" << endl;
    } else {
      cout << "error: invalid variable id: " << highlightVarIds[i] << endl;
      abort();
    }
  }

  for (size_t i = 0; i < nodes_.size(); i++) {
    EGNeighs neighs = nodes_[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      out << '"' << nodes_[i]->label() << '"' << " -- " ;
      out << '"' << neighs[j]->label() << '"' << endl;
    }
  }

  out << "}" << endl;
  out.close();
}



VarIds
ElimGraph::getEliminationOrder (
    const Factors& factors,
    VarIds excludedVids)
{
  if (elimHeuristic == ElimHeuristic::SEQUENTIAL) {
    VarIds allVids;
    Factors::const_iterator first = factors.begin();
    Factors::const_iterator end   = factors.end();
    for (; first != end; ++first) {
      Util::addToVector (allVids, (*first)->arguments());      
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
  varMap_.insert (make_pair (n->varId(), n));
}



EgNode*
ElimGraph::getEgNode (VarId vid) const
{
  unordered_map<VarId, EgNode*>::const_iterator it;
  it = varMap_.find (vid);
  return (it != varMap_.end()) ? it->second : 0;
}



EgNode*
ElimGraph::getLowestCostNode (void) const
{
  EgNode* bestNode = 0;
  unsigned minCost = std::numeric_limits<unsigned>::max();
  EGNeighs::const_iterator it;
  switch (elimHeuristic) {
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
        if ( ! neighbors (neighs[i], neighs[j])) {
          addEdge (neighs[i], neighs[j]);
        }
      }
    }
  }
}

