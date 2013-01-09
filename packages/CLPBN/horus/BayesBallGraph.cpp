#include <cstdlib>
#include <cassert>

#include <iostream>
#include <sstream>
#include <fstream>

#include "BayesBallGraph.h"
#include "Util.h"


void
BayesBallGraph::addNode (BBNode* n)
{
  assert (Util::contains (varMap_, n->varId()) == false);
  nodes_.push_back (n);
  varMap_[n->varId()] = n;
}



void
BayesBallGraph::addEdge (VarId vid1, VarId vid2)
{
  unordered_map<VarId, BBNode*>::iterator it1;
  unordered_map<VarId, BBNode*>::iterator it2;
  it1 = varMap_.find (vid1);
  it2 = varMap_.find (vid2);
  assert (it1 != varMap_.end());
  assert (it2 != varMap_.end());
  it1->second->addChild  (it2->second);
  it2->second->addParent (it1->second);
}



const BBNode*
BayesBallGraph::getNode (VarId vid) const
{
  unordered_map<VarId, BBNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



BBNode*
BayesBallGraph::getNode (VarId vid)
{
  unordered_map<VarId, BBNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



void
BayesBallGraph::setIndexes (void)
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    nodes_[i]->setIndex (i);
  }
}



void
BayesBallGraph::clear (void)
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    nodes_[i]->clear();
  }
}



void
BayesBallGraph::exportToGraphViz (const char* fileName)
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "Error: couldn't open file '" << fileName << "'." ;
    return;
  }
  out << "digraph {" << endl;
  out << "ranksep=1" << endl;
  for (size_t i = 0; i < nodes_.size(); i++) {
    out << nodes_[i]->varId() ;
    out << " [" ;
    out << "label=\"" << nodes_[i]->label() << "\"" ;
    if (nodes_[i]->hasEvidence()) {
      out << ",style=filled, fillcolor=yellow" ;
    }
    out << "]" << endl;
  }
  for (size_t i = 0; i < nodes_.size(); i++) {
    const vector<BBNode*>& childs = nodes_[i]->childs();
    for (size_t j = 0; j < childs.size(); j++) {
      out << nodes_[i]->varId() << " -> " << childs[j]->varId();
      out << " [style=bold]" << endl ;
    }
  }
  out << "}" << endl;
  out.close();
}

