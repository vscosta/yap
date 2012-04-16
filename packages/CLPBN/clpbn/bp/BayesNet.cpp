#include <cstdlib>
#include <cassert>

#include <iostream>
#include <fstream>
#include <sstream>

#include "BayesNet.h"
#include "Util.h"


void
DAGraph::addNode (DAGraphNode* n)
{
  assert (Util::contains (varMap_, n->varId()) == false);
  nodes_.push_back (n);
  varMap_[n->varId()] = n;
}



void
DAGraph::addEdge (VarId vid1, VarId vid2)
{
  unordered_map<VarId, DAGraphNode*>::iterator it1;
  unordered_map<VarId, DAGraphNode*>::iterator it2;
  it1 = varMap_.find (vid1);
  it2 = varMap_.find (vid2);
  assert (it1 != varMap_.end());
  assert (it2 != varMap_.end());
  it1->second->addChild  (it2->second);
  it2->second->addParent (it1->second);
}



const DAGraphNode*
DAGraph::getNode (VarId vid) const
{
  unordered_map<VarId, DAGraphNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



DAGraphNode*
DAGraph::getNode (VarId vid)
{
  unordered_map<VarId, DAGraphNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



void
DAGraph::setIndexes (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    nodes_[i]->setIndex (i);
  }
}



void
DAGraph::clear (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    nodes_[i]->clear();
  }
}



void
DAGraph::exportToGraphViz (const char* fileName)
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "DAGraph::exportToDotFile()" << endl;
    abort();
  }
  out << "digraph {" << endl;
  out << "ranksep=1" << endl;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    out << nodes_[i]->varId() ;
    out << " [" ;
    out << "label=\"" << nodes_[i]->label() << "\"" ;
    if (nodes_[i]->hasEvidence()) {
      out << ",style=filled, fillcolor=yellow" ;
    }
    out << "]" << endl;
  }
  for (unsigned i = 0; i < nodes_.size(); i++) {
    const vector<DAGraphNode*>& childs = nodes_[i]->childs();
    for (unsigned j = 0; j < childs.size(); j++) {
      out << nodes_[i]->varId() << " -> " << childs[j]->varId();
      out << " [style=bold]" << endl ;
    }
  }
  out << "}" << endl;
  out.close();
}

