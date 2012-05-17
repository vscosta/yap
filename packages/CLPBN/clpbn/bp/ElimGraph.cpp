#include <limits>

#include <fstream>

#include "ElimGraph.h"

ElimHeuristic ElimGraph::elimHeuristic = MIN_NEIGHBORS;


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
  unmarked_.reserve (nodes_.size());
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (Util::contains (exclude, nodes_[i]->varId()) == false) {
      unmarked_.insert (nodes_[i]);
    }
  }
  unsigned nVarsToEliminate = nodes_.size() - exclude.size();
  for (unsigned i = 0; i < nVarsToEliminate; i++) {
    EgNode* node = getLowestCostNode();
    unmarked_.remove (node);
    const EGNeighs& neighs = node->neighbors();
    for (unsigned j = 0; j < neighs.size(); j++) {
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
  for (unsigned i = 0; i < nodes_.size(); i++) {
    cout << "node " << nodes_[i]->label() << " neighs:" ;
    EGNeighs neighs = nodes_[i]->neighbors();
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
    EGNeighs neighs = nodes_[i]->neighbors();
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
  unsigned cost = 0;
  EGNeighs::const_iterator it;
  switch (elimHeuristic) {
    case MIN_NEIGHBORS: {
      for (it = unmarked_.begin(); it != unmarked_.end(); ++ it) {
        cost = getNeighborsCost (*it);
        if (cost < minCost) {
          bestNode = *it;
          minCost  = cost;
        }
      }}
      break;
    case MIN_WEIGHT:
      //cost = getWeightCost (unmarked_[i]);
      break;
    case MIN_FILL:
      //cost = getFillCost (unmarked_[i]);
      break;
    case WEIGHTED_MIN_FILL:
      //cost = getWeightedFillCost (unmarked_[i]);
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
    for (unsigned i = 0; i < neighs.size() - 1; i++) {
      for (unsigned j = i+1; j < neighs.size(); j++) {
        if ( ! neighbors (neighs[i], neighs[j])) {
          addEdge (neighs[i], neighs[j]);
        }
      }
    }
  }
}

