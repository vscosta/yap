#include <limits>

#include <fstream>

#include "ElimGraph.h"

ElimHeuristic ElimGraph::elimHeuristic_ = MIN_NEIGHBORS;


ElimGraph::ElimGraph (const vector<Factor*>& factors)
{
  for (unsigned i = 0; i < factors.size(); i++) {
    if (factors[i] == 0) { // if contained just one var with evidence
      continue;
    }
    const VarIds& vids = factors[i]->arguments();
    for (unsigned j = 0; j < vids.size() - 1; j++) {
      EgNode* n1 = getEgNode (vids[j]);
      if (n1 == 0) {
        n1 = new EgNode (vids[j], factors[i]->range (j));
        addNode (n1);
      }
      for (unsigned k = j + 1; k < vids.size(); k++) {
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
  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];    
  }
}



VarIds
ElimGraph::getEliminatingOrder (const VarIds& exclude)
{
  VarIds elimOrder;
  marked_.resize (nodes_.size(), false);
  for (unsigned i = 0; i < exclude.size(); i++) {
    assert (getEgNode (exclude[i]));
    EgNode* node = getEgNode (exclude[i]);
    marked_[*node] = true;
  }
  unsigned nVarsToEliminate = nodes_.size() - exclude.size();
  for (unsigned i = 0; i < nVarsToEliminate; i++) {
    EgNode* node = getLowestCostNode();
    marked_[*node] = true;
    elimOrder.push_back (node->varId());
    connectAllNeighbors (node);
  }
  return elimOrder;
}



void
ElimGraph::print (void) const
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    cout << "node " << nodes_[i]->label() << " neighs:" ;
    vector<EgNode*> neighs = nodes_[i]->neighbors();
    for (unsigned j = 0; j < neighs.size(); j++) {
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

  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (showNeighborless || nodes_[i]->neighbors().size() != 0) {
      out << '"' << nodes_[i]->label() << '"' << endl;
    }
  }

  for (unsigned i = 0; i < highlightVarIds.size(); i++) {
    EgNode* node =getEgNode (highlightVarIds[i]);
    if (node) {
      out << '"' << node->label() << '"' ;
      out << " [shape=box3d]" << endl;
    } else {
      cout << "error: invalid variable id: " << highlightVarIds[i] << endl;
      abort();
    }
  }

  for (unsigned i = 0; i < nodes_.size(); i++) {
    vector<EgNode*> neighs = nodes_[i]->neighbors();
    for (unsigned j = 0; j < neighs.size(); j++) {
      out << '"' << nodes_[i]->label() << '"' << " -- " ;
      out << '"' << neighs[j]->label() << '"' << endl;
    }
  }

  out << "}" << endl;
  out.close();
}



VarIds
ElimGraph::getEliminationOrder (
    const vector<Factor*> factors,
    VarIds excludedVids)
{
  ElimGraph graph (factors);
  // graph.print();
  // graph.exportToGraphViz ("_egg.dot");
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
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (marked_[i]) continue;
    unsigned cost = 0;
    switch (elimHeuristic_) {
      case MIN_NEIGHBORS:
        cost = getNeighborsCost (nodes_[i]);
        break;
      case MIN_WEIGHT:
        cost = getWeightCost (nodes_[i]);
        break;
      case MIN_FILL:
        cost = getFillCost (nodes_[i]);
        break;
      case WEIGHTED_MIN_FILL:
        cost = getWeightedFillCost (nodes_[i]);
        break;
      default:
        assert (false);
    }
    if (cost < minCost) {
      bestNode = nodes_[i];
      minCost  = cost;
    }
  }
  assert (bestNode);
  return bestNode;
}



unsigned
ElimGraph::getNeighborsCost (const EgNode* n) const
{
  unsigned cost = 0;
  const vector<EgNode*>& neighs = n->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    if (marked_[*neighs[i]] == false) {
      cost ++;
    }
  }
  return cost;
}



unsigned
ElimGraph::getWeightCost (const EgNode* n) const
{
  unsigned cost = 1;
  const vector<EgNode*>& neighs = n->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    if (marked_[*neighs[i]] == false) {
      cost *= neighs[i]->range();
    }
  }
  return cost;
}



unsigned
ElimGraph::getFillCost (const EgNode* n) const
{
  unsigned cost = 0;
  const vector<EgNode*>& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (unsigned i = 0; i < neighs.size() - 1; i++) {
      if (marked_[*neighs[i]] == true) continue;
      for (unsigned j = i+1; j < neighs.size(); j++) {
        if (marked_[*neighs[j]] == true) continue;
        if (!neighbors (neighs[i], neighs[j])) {
          cost ++;
        }
      }
    }
  }
  return cost;
}



unsigned
ElimGraph::getWeightedFillCost (const EgNode* n) const
{
  unsigned cost = 0;
  const vector<EgNode*>& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (unsigned i = 0; i < neighs.size() - 1; i++) {
      if (marked_[*neighs[i]] == true) continue;
      for (unsigned j = i+1; j < neighs.size(); j++) {
        if (marked_[*neighs[j]] == true) continue;
        if (!neighbors (neighs[i], neighs[j])) {
          cost += neighs[i]->range() * neighs[j]->range();
        }
      }
    }
  }
  return cost;
}



void
ElimGraph::connectAllNeighbors (const EgNode* n)
{
  const vector<EgNode*>& neighs = n->neighbors();
  if (neighs.size() > 0) {
    for (unsigned i = 0; i < neighs.size() - 1; i++) {
      if (marked_[*neighs[i]] == true) continue;
      for (unsigned j = i+1; j < neighs.size(); j++) {
        if (marked_[*neighs[j]] == true) continue;
        if (!neighbors (neighs[i], neighs[j])) {
          addEdge (neighs[i], neighs[j]);
        }
      }
    }
  }
}



bool
ElimGraph::neighbors (const EgNode* n1, const EgNode* n2) const
{
  const vector<EgNode*>& neighs = n1->neighbors();
  for (unsigned i = 0; i < neighs.size(); i++) {
    if (neighs[i] == n2) {
      return true;
    }
  }
  return false;
}

