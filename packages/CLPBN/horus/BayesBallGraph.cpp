
#if __ANDROID__
#define assert(P)
#else
#include <cassert>
#endif

#include <iostream>
#include <fstream>
#include <sstream>

#include "BayesBallGraph.h"
#include "Util.h"


namespace Horus {

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
  std::unordered_map<VarId, BBNode*>::iterator it1;
  std::unordered_map<VarId, BBNode*>::iterator it2;
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
  std::unordered_map<VarId, BBNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



BBNode*
BayesBallGraph::getNode (VarId vid)
{
  std::unordered_map<VarId, BBNode*>::const_iterator it;
  it = varMap_.find (vid);
  return it != varMap_.end() ? it->second : 0;
}



void
BayesBallGraph::setIndexes()
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    nodes_[i]->setIndex (i);
  }
}



void
BayesBallGraph::clear()
{
  for (size_t i = 0; i < nodes_.size(); i++) {
    nodes_[i]->clear();
  }
}



void
BayesBallGraph::exportToGraphViz (const char* fileName)
{
  std::ofstream out (fileName);
  if (!out.is_open()) {
    std::cerr << "Error: couldn't open file '" << fileName << "'." ;
    std::cerr << std::endl;
    return;
  }
  out << "digraph {" << std::endl;
  out << "ranksep=1" << std::endl;
  for (size_t i = 0; i < nodes_.size(); i++) {
    out << nodes_[i]->varId() ;
    out << " [" ;
    out << "label=\"" << nodes_[i]->label() << "\"" ;
    if (nodes_[i]->hasEvidence()) {
      out << ",style=filled, fillcolor=yellow" ;
    }
    out << "]" << std::endl;
  }
  for (size_t i = 0; i < nodes_.size(); i++) {
    const std::vector<BBNode*>& childs = nodes_[i]->childs();
    for (size_t j = 0; j < childs.size(); j++) {
      out << nodes_[i]->varId() << " -> " << childs[j]->varId();
      out << " [style=bold]" << std::endl;
    }
  }
  out << "}" << std::endl;
  out.close();
}

}  // namespace Horus

