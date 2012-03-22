#include <cstdlib>
#include <cassert>

#include <iostream>
#include <fstream>
#include <sstream>

#include "xmlParser/xmlParser.h"

#include "BayesNet.h"
#include "Util.h"



BayesNet::~BayesNet (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
}



void
BayesNet::readFromBifFormat (const char* fileName)
{
  XMLNode xMainNode = XMLNode::openFileHelper (fileName, "BIF");
  // only the first network is parsed, others are ignored
  XMLNode xNode   = xMainNode.getChildNode ("NETWORK");
  unsigned nVars = xNode.nChildNode ("VARIABLE");
  for (unsigned i = 0; i < nVars; i++) {
    XMLNode var = xNode.getChildNode ("VARIABLE", i);
    if (string (var.getAttribute ("TYPE")) != "nature") {
      cerr << "error: only \"nature\" variables are supported" << endl;
      abort();
    }
    States states;
    string label      = var.getChildNode("NAME").getText();
    unsigned nrStates = var.nChildNode ("OUTCOME");
    for (unsigned j = 0; j < nrStates; j++) {
      if (var.getChildNode("OUTCOME", j).getText() == 0) {
        stringstream ss;
        ss << j + 1;
        states.push_back (ss.str());
      } else {
        states.push_back (var.getChildNode("OUTCOME", j).getText());
      }
    }
    addNode (label, states);
  }

  unsigned nDefs = xNode.nChildNode ("DEFINITION");
  if (nVars != nDefs) {
    cerr << "error: different number of variables and definitions" << endl;
    abort();
  }
  for (unsigned i = 0; i < nDefs; i++) {
    XMLNode def  = xNode.getChildNode ("DEFINITION", i);
    string label = def.getChildNode("FOR").getText();
    BayesNode* node = getBayesNode (label);
    if (!node) {
      cerr << "error: unknow variable `" << label << "'" << endl;
      abort();
    }
    BnNodeSet parents;
    unsigned nParams = node->nrStates();
    for (int j = 0; j < def.nChildNode ("GIVEN"); j++) {
      string parentLabel = def.getChildNode("GIVEN", j).getText();
      BayesNode* parentNode = getBayesNode (parentLabel);
      if (!parentNode) {
        cerr << "error: unknow variable `" << parentLabel << "'" << endl;
        abort();
      }
      nParams *= parentNode->nrStates();
      parents.push_back (parentNode);
    }
    node->setParents (parents);
    unsigned count = 0;
    Params params (nParams);
    stringstream s (def.getChildNode("TABLE").getText());
    while (!s.eof() && count < nParams) {
      s >> params[count];
      count ++;
    }
    if (count != nParams) {
      cerr << "error: invalid number of parameters " ;
      cerr << "for variable `" << label << "'" << endl;
      abort();
    }
    params = reorderParameters (params, node->nrStates());
    Distribution* dist = new Distribution (params);
    node->setDistribution (dist);
    addDistribution (dist);
  }

  setIndexes();
  if (Globals::logDomain) {
    distributionsToLogs();
  } 
}



BayesNode*
BayesNet::addNode (string label, const States& states)
{
  VarId vid = nodes_.size();
  varMap_.insert (make_pair (vid, nodes_.size()));
  GraphicalModel::addVariableInformation (vid, label, states);
  BayesNode* node = new BayesNode (VarNode (vid, states.size()));
  nodes_.push_back (node);
  return node;
}



BayesNode*
BayesNet::addNode (VarId vid, unsigned dsize, int evidence, Distribution* dist)
{
  varMap_.insert (make_pair (vid, nodes_.size()));
  nodes_.push_back (new BayesNode (vid, dsize, evidence, dist));
  return nodes_.back();
}



BayesNode*
BayesNet::getBayesNode (VarId vid) const
{
  IndexMap::const_iterator it = varMap_.find (vid);
  if (it == varMap_.end()) {
    return 0;
  } else {
    return nodes_[it->second];
  }
}



BayesNode*
BayesNet::getBayesNode (string label) const
{
  BayesNode* node = 0;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->label() == label) {
      node = nodes_[i];
      break;
    }
  }
  return node;
}




VarNode*
BayesNet::getVariableNode (VarId vid) const
{
  BayesNode* node = getBayesNode (vid);
  assert (node);
  return node;
}




VarNodes
BayesNet::getVariableNodes (void) const
{
  VarNodes vars;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    vars.push_back (nodes_[i]);
  }
  return vars;
}



void
BayesNet::addDistribution (Distribution* dist)
{
  dists_.push_back (dist);
}



Distribution*
BayesNet::getDistribution (unsigned distId) const
{
  Distribution* dist = 0;
  for (unsigned i = 0; i < dists_.size(); i++) {
    if (dists_[i]->id == (int) distId) {
      dist = dists_[i];
      break;
    }
  }
  return dist;
}



const BnNodeSet&
BayesNet::getBayesNodes (void) const
{
  return nodes_;
}



unsigned
BayesNet::nrNodes (void) const
{
  return nodes_.size();
}



BnNodeSet
BayesNet::getRootNodes (void) const
{
  BnNodeSet roots;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->isRoot()) {
      roots.push_back (nodes_[i]);
    }
  }
  return roots;
}



BnNodeSet
BayesNet::getLeafNodes (void) const
{
  BnNodeSet leafs;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->isLeaf()) {
      leafs.push_back (nodes_[i]);
    }
  }
  return leafs;
}



BayesNet*
BayesNet::getMinimalRequesiteNetwork (VarId vid) const
{
  return getMinimalRequesiteNetwork (VarIds() = {vid});
}



BayesNet*
BayesNet::getMinimalRequesiteNetwork (const VarIds& queryVarIds) const
{
  BnNodeSet queryVars;
  Scheduling scheduling;
  for (unsigned i = 0; i < queryVarIds.size(); i++) {
    BayesNode* n = getBayesNode (queryVarIds[i]);
    assert (n);
    queryVars.push_back (n);
    scheduling.push (ScheduleInfo (n, false, true));
  }

  vector<StateInfo*> states (nodes_.size(), 0);

  while (!scheduling.empty()) {
    ScheduleInfo& sch = scheduling.front();
    StateInfo* state  = states[sch.node->getIndex()];
    if (!state) {
      state = new StateInfo();
      states[sch.node->getIndex()] = state;
    } else {
      state->visited = true;
    }
    if (!sch.node->hasEvidence() && sch.visitedFromChild) {
      if (!state->markedOnTop) {
        state->markedOnTop = true;
        scheduleParents (sch.node, scheduling);
      }
      if (!state->markedOnBottom) {
        state->markedOnBottom = true;
        scheduleChilds (sch.node, scheduling);
      }
    }
    if (sch.visitedFromParent) {
      if (sch.node->hasEvidence() && !state->markedOnTop) {
        state->markedOnTop = true;
        scheduleParents (sch.node, scheduling);
      }
      if (!sch.node->hasEvidence() && !state->markedOnBottom) {
        state->markedOnBottom = true;
        scheduleChilds (sch.node, scheduling);
      }
    }
    scheduling.pop();
  }
  /*
  cout << "\t\ttop\tbottom" << endl;
  cout << "variable\t\tmarked\tmarked\tvisited\tobserved" << endl;
  cout << "----------------------------------------------------------" ;
  cout << endl;
  for (unsigned i = 0; i < states.size(); i++) {
    cout << nodes_[i]->label() << ":\t\t" ;
    if (states[i]) {
      states[i]->markedOnTop    ? cout << "yes\t" : cout << "no\t" ;
      states[i]->markedOnBottom ? cout << "yes\t" : cout << "no\t" ;
      states[i]->visited        ? cout << "yes\t" : cout << "no\t" ;
      nodes_[i]->hasEvidence()  ? cout << "yes"   : cout << "no" ;
      cout << endl;
    } else {
      cout << "no\tno\tno\t" ;
      nodes_[i]->hasEvidence() ? cout << "yes" : cout << "no" ;
      cout << endl;
    }
  }
  cout << endl;
  */
  BayesNet* bn = new BayesNet();
  constructGraph (bn, states);

  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete states[i];
  }
  return bn;
}



void
BayesNet::constructGraph (BayesNet* bn,
                          const vector<StateInfo*>& states) const
{
  BnNodeSet mrnNodes;
  vector<VarIds> parents;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    bool isRequired = false;
    if (states[i]) {
      isRequired = (nodes_[i]->hasEvidence() && states[i]->visited)
                     || 
                   states[i]->markedOnTop;
    }
    if (isRequired) {
      parents.push_back (VarIds());
      if (states[i]->markedOnTop) {
        const BnNodeSet& ps = nodes_[i]->getParents();
        for (unsigned j = 0; j < ps.size(); j++) {
          parents.back().push_back (ps[j]->varId());
        }
      }
      assert (bn->getBayesNode (nodes_[i]->varId()) == 0);
      BayesNode* mrnNode = bn->addNode (nodes_[i]->varId(),
                                        nodes_[i]->nrStates(),
                                        nodes_[i]->getEvidence(),
                                        nodes_[i]->getDistribution());
      mrnNodes.push_back (mrnNode);
    }
  }
  for (unsigned i = 0; i < mrnNodes.size(); i++) {
    BnNodeSet ps;
    for (unsigned j = 0; j < parents[i].size(); j++) {
      assert (bn->getBayesNode (parents[i][j]) != 0);
      ps.push_back (bn->getBayesNode (parents[i][j]));
    }
    mrnNodes[i]->setParents (ps);
  }
  bn->setIndexes();
}



bool
BayesNet::isPolyTree (void) const
{
  return !containsUndirectedCycle();
}



void
BayesNet::setIndexes (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    nodes_[i]->setIndex (i);
  }
}



void
BayesNet::distributionsToLogs (void)
{
  for (unsigned i = 0; i < dists_.size(); i++) {
    Util::toLog (dists_[i]->params);
  }
}



void
BayesNet::freeDistributions (void)
{
  for (unsigned i = 0; i < dists_.size(); i++) {
    delete dists_[i];
  }
}



void
BayesNet::printGraphicalModel (void) const
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    cout << *nodes_[i];
  }
}



void
BayesNet::exportToGraphViz (const char* fileName,
                            bool showNeighborless,
                            const VarIds& highlightVarIds) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesNet::exportToDotFile()" << endl;
    abort();
  }

  out << "digraph {" << endl;
  out << "ranksep=1" << endl;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (showNeighborless || nodes_[i]->hasNeighbors()) {
      out << nodes_[i]->varId() ;
      if (nodes_[i]->hasEvidence()) {
        out << " [" ;
        out << "label=\"" << nodes_[i]->label() << "\"," ;
        out << "style=filled, fillcolor=yellow" ;
        out << "]" ;
      } else {
        out << " [" ;
        out << "label=\"" << nodes_[i]->label() << "\"" ;
        out << "]" ;
      }
      out << endl;
    }
  }

  for (unsigned i = 0; i < highlightVarIds.size(); i++) {
    BayesNode* node = getBayesNode (highlightVarIds[i]);
    if (node) {
      out << node->varId() ;
      out << " [shape=box3d]" << endl;
    } else {
      cout << "error: invalid variable id: " << highlightVarIds[i] << endl;
      abort();
    }
  }

  for (unsigned i = 0; i < nodes_.size(); i++) {
    const BnNodeSet& childs = nodes_[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      out << nodes_[i]->varId() << " -> " << childs[j]->varId() << " [style=bold]" << endl ;
    }
  }

  out << "}" << endl;
  out.close();
}



void
BayesNet::exportToBifFormat (const char* fileName) const
{
  ofstream out (fileName);
  if(!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesNet::exportToBifFile()" << endl;
    abort();
  }
  out << "<?xml version=\"1.0\" encoding=\"US-ASCII\"?>" << endl;
  out << "<BIF VERSION=\"0.3\">" << endl;
  out << "<NETWORK>" << endl;
  out << "<NAME>" << fileName << "</NAME>" << endl << endl;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    out << "<VARIABLE TYPE=\"nature\">" << endl;
    out << "\t<NAME>" << nodes_[i]->label() << "</NAME>" << endl;
    const States& states = nodes_[i]->states();
    for (unsigned j = 0; j < states.size(); j++) {
      out << "\t<OUTCOME>" << states[j] << "</OUTCOME>" << endl;
    }
    out << "</VARIABLE>" << endl << endl;
  }

  for (unsigned i = 0; i < nodes_.size(); i++) {
    out << "<DEFINITION>" << endl;
    out << "\t<FOR>" << nodes_[i]->label() << "</FOR>" << endl;
    const BnNodeSet& parents = nodes_[i]->getParents();
    for (unsigned j = 0; j < parents.size(); j++) {
      out << "\t<GIVEN>" << parents[j]->label();
      out << "</GIVEN>" << endl;
    }
    Params params = revertParameterReorder (nodes_[i]->getParameters(),
                                              nodes_[i]->nrStates());
    out << "\t<TABLE>" ;
    for (unsigned j = 0; j < params.size(); j++) {
       out << " " << params[j];
    }
    out << " </TABLE>" << endl;
    out << "</DEFINITION>" << endl << endl;
  }
  out << "</NETWORK>" << endl;
  out << "</BIF>" << endl << endl;
  out.close();
}



bool
BayesNet::containsUndirectedCycle (void) const
{
  vector<bool> visited (nodes_.size(), false);
  for (unsigned i = 0; i < nodes_.size(); i++) {
    int v = nodes_[i]->getIndex();
    if (!visited[v]) {
      if (containsUndirectedCycle (v, -1, visited)) {
        return true;
      }
    }
  }
  return false;
}



bool
BayesNet::containsUndirectedCycle (int v, int p, vector<bool>& visited) const
{
  visited[v] = true;
  vector<int> adjacencies = getAdjacentNodes (v);
  for (unsigned i = 0; i < adjacencies.size(); i++) {
    int w = adjacencies[i];
    if (!visited[w]) {
      if (containsUndirectedCycle (w, v, visited)) {
        return true;
      }
    }
    else if (visited[w] && w != p) {
      return true;
    }
  }
  return false; // no cycle detected in this component
}



vector<int>
BayesNet::getAdjacentNodes (int v) const
{
  vector<int> adjacencies;
  const BnNodeSet& parents = nodes_[v]->getParents();
  const BnNodeSet& childs  = nodes_[v]->getChilds();
  for (unsigned i = 0; i < parents.size(); i++) {
    adjacencies.push_back (parents[i]->getIndex());
  }
  for (unsigned i = 0; i < childs.size(); i++) {
    adjacencies.push_back (childs[i]->getIndex());
  }
  return adjacencies;
}



Params
BayesNet::reorderParameters (const Params& params, unsigned dsize) const
{
  // the interchange format for bayesian networks keeps the probabilities 
  // in the following order:
  // p(a1|b1,c1) p(a2|b1,c1) p(a1|b1,c2) p(a2|b1,c2) p(a1|b2,c1) p(a2|b2,c1)
  // p(a1|b2,c2) p(a2|b2,c2).
  //
  // however, in clpbn we keep the probabilities in this order:
  // p(a1|b1,c1) p(a1|b1,c2) p(a1|b2,c1) p(a1|b2,c2) p(a2|b1,c1) p(a2|b1,c2)
  // p(a2|b2,c1) p(a2|b2,c2).
  unsigned count    = 0;
  unsigned rowSize  = params.size() / dsize;
  Params reordered;
  while (reordered.size() < params.size()) {
    unsigned idx = count;
    for (unsigned i = 0; i < rowSize; i++) {
      reordered.push_back (params[idx]);
      idx += dsize	;
    }
    count++;
  }
  return reordered;
}



Params
BayesNet::revertParameterReorder (const Params& params, unsigned dsize) const
{
  unsigned count    = 0;
  unsigned rowSize  = params.size() / dsize;
  Params reordered;
  while (reordered.size() < params.size()) {
    unsigned idx = count;
    for (unsigned i = 0; i < dsize; i++) {
      reordered.push_back (params[idx]);
      idx += rowSize;
    }
    count ++;
  }
  return reordered;
}

