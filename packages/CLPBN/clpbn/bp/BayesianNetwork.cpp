#include <iostream>
#include <fstream>
#include <cassert>
#include <cstdlib>

#include "BayesianNetwork.h"
#include "BayesianNode.h"

BayesianNetwork::BayesianNetwork (void)
{
}



BayesianNetwork::~BayesianNetwork (void)
{
 for (unsigned int i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
 for (unsigned int i = 0; i < dists_.size(); i++) {
    delete dists_[i];
  }
}



void
BayesianNetwork::addNode (string varName,
                          vector<BayesianNode*> parents,
                          int evidence,
                          int distId)
{
  for (unsigned int i = 0; i < dists_.size(); i++) {
    if (dists_[i]->id == distId) {
      BayesianNode* node = new BayesianNode (varName, parents,
                                             dists_[i], evidence);
      nodes_.push_back (node);
      break;
    }
  }
}


void
BayesianNetwork::addNode (string varName,
                          vector<BayesianNode*> parents, 
                          double* params,
                          int nParams,
                          vector<string> domain)
{
  Distribution* dist = new Distribution (params, nParams, domain);
  BayesianNode* node = new BayesianNode (varName, parents, dist);
  dists_.push_back (dist);
  nodes_.push_back (node);
}



BayesianNode*
BayesianNetwork::getNode (string varName) const
{
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->getVariableName() == varName) {
      return nodes_[i];
    }
  }
  return 0;
}



void
BayesianNetwork::addDistribution (int distId,
                                  double* params, 
                                  int nParams,
                                  vector<string> domain)
{
  dists_.push_back (new Distribution (distId, params, nParams, domain));
}



vector<BayesianNode*> 
BayesianNetwork::getNodes (void) const
{
  return nodes_;
}



vector<BayesianNode*> 
BayesianNetwork::getRootNodes (void) const
{
  vector<BayesianNode*> roots;
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->isRoot()) {
      roots.push_back (nodes_[i]);
    }
  }
  return roots;
}



vector<BayesianNode*> 
BayesianNetwork::getLeafNodes (void) const
{
  vector<BayesianNode*> leafs;
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->isLeaf()) {
      leafs.push_back (nodes_[i]);
    }
  }
  return leafs;
}



bool
BayesianNetwork::isPolyTree (void) const
{
  return !containsCycle();
}



void
BayesianNetwork::printNetwork (void) const
{
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    cout << *nodes_[i];
  }
}



bool
BayesianNetwork::containsCycle (void) const
{
  vector<bool> visited (nodes_.size());
  for (unsigned int v = 0; v < nodes_.size(); v++) {
    visited[v] = false;
  }

  for (unsigned int v = 0; v < nodes_.size(); v++) {
    if (!visited[v]) {
      if (containsCycle (v, -1, visited)) {
		return true;
      }
    }
  }

  return false;
}



bool
BayesianNetwork::containsCycle (int v,
                                int predecessor,
                                vector<bool>& visited) const
{
  visited[v] = true;
  vector<int> adjs = getAdjacentVertexes (v);
  for (unsigned int i = 0; i < adjs.size(); i++) {
    int w = adjs[i];
    if (!visited[w]) {
      if (containsCycle (w, v, visited)) {
        return true;
      }
    }
    else if (visited[w] && w != predecessor) {
      return true;
    }
  }
  return false; // no cycle detected in this component
}



int
BayesianNetwork::getIndexOf (const BayesianNode* node) const
{
  for (unsigned int i = 0; i < nodes_.size(); i++) {
    if (node == nodes_[i]) {
      return i;
    }
  }
  return -1;
}



vector<int>
BayesianNetwork::getAdjacentVertexes (int v) const
{
  vector<int> adjs;
  vector<BayesianNode*> parents = nodes_[v]->getParents();
  vector<BayesianNode*> childs  = nodes_[v]->getChilds();
  for (unsigned int i = 0; i < parents.size(); i++) {
    adjs.push_back (getIndexOf (parents[i]));
  }
  for (unsigned int i = 0; i < childs.size(); i++) {
    adjs.push_back (getIndexOf (childs[i]));
  }
  return adjs;
}

