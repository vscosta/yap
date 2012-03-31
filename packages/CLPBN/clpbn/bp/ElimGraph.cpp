#include <limits>

#include <fstream>

#include "ElimGraph.h"
#include "BayesNet.h"


ElimHeuristic ElimGraph::elimHeuristic_ = MIN_NEIGHBORS;


ElimGraph::ElimGraph (const BayesNet& bayesNet)
{
  const BnNodeSet& bnNodes = bayesNet.getBayesNodes();
  for (unsigned i = 0; i < bnNodes.size(); i++) {
    if (bnNodes[i]->hasEvidence() == false) {
      addNode (new EgNode (bnNodes[i]));
    }
  }

  for (unsigned i = 0; i < bnNodes.size(); i++) {
    if (bnNodes[i]->hasEvidence() == false) {
      EgNode* n = getEgNode (bnNodes[i]->varId());
      const BnNodeSet& childs = bnNodes[i]->getChilds();
      for (unsigned j = 0; j < childs.size(); j++) {
        if (childs[j]->hasEvidence() == false) {
          addEdge (n, getEgNode (childs[j]->varId()));
        }
      }
    }
  }

  for (unsigned i = 0; i < bnNodes.size(); i++) {
    vector<EgNode*> neighs;
    const vector<BayesNode*>& parents = bnNodes[i]->getParents();
    for (unsigned i = 0; i < parents.size(); i++) {
      if (parents[i]->hasEvidence() == false) {
        neighs.push_back (getEgNode (parents[i]->varId()));
      }
    }
    if (neighs.size() > 0) {
      for (unsigned i = 0; i < neighs.size() - 1; i++) {
        for (unsigned j = i+1; j < neighs.size(); j++) {
          if (!neighbors (neighs[i], neighs[j])) {
            addEdge (neighs[i], neighs[j]);
          }
        }
      }
    }
  }

  setIndexes();
}



ElimGraph::~ElimGraph (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];    
  }
}



void
ElimGraph::addNode (EgNode* n)
{
  nodes_.push_back (n);
 varMap_.insert (make_pair (n->varId(), n));
}



EgNode*
ElimGraph::getEgNode (VarId vid) const
{
  unordered_map<VarId,EgNode*>::const_iterator it =varMap_.find (vid);
  if (it ==varMap_.end()) {
    return 0;
  } else {
    return it->second;
  }
}



VarIds
ElimGraph::getEliminatingOrder (const VarIds& exclude)
{
  VarIds elimOrder;
  marked_.resize (nodes_.size(), false);

  for (unsigned i = 0; i < exclude.size(); i++) {
    EgNode* node = getEgNode (exclude[i]);
    assert (node);
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
      cost *= neighs[i]->nrStates();
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
          cost += neighs[i]->nrStates() * neighs[j]->nrStates();
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



void
ElimGraph::setIndexes (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    nodes_[i]->setIndex (i);
  }
}



void
ElimGraph::printGraphicalModel (void) const
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
ElimGraph::exportToGraphViz (const char* fileName,
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
      out << '"' << nodes_[i]->label() << '"' ;
      if (nodes_[i]->hasEvidence()) {
        out << " [style=filled, fillcolor=yellow]" << endl;
      } else {
        out << endl;
      }
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

