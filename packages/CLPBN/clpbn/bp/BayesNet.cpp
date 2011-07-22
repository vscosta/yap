#include <cstdlib>
#include <cassert>

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>

#include "xmlParser/xmlParser.h"

#include "BayesNet.h"


BayesNet::BayesNet (const char* fileName)
{
  map<string, Domain> domains;
  XMLNode xMainNode = XMLNode::openFileHelper (fileName, "BIF");
  // only the first network is parsed, others are ignored
  XMLNode xNode = xMainNode.getChildNode ("NETWORK");
  unsigned nVars = xNode.nChildNode ("VARIABLE");
  for (unsigned i = 0; i < nVars; i++) {
    XMLNode var = xNode.getChildNode ("VARIABLE", i);
    string type = var.getAttribute ("TYPE");
    if (type != "nature") {
      cerr << "error: only \"nature\" variables are supported" << endl;
      abort();
    }
    Domain domain;
    string varLabel = var.getChildNode("NAME").getText();
    unsigned dsize = var.nChildNode ("OUTCOME");
    for (unsigned j = 0; j < dsize; j++) {
      if (var.getChildNode("OUTCOME", j).getText() == 0) {
        stringstream ss;
        ss << j + 1;
        domain.push_back (ss.str());
      } else {
        domain.push_back (var.getChildNode("OUTCOME", j).getText());
      }
    }
    domains.insert (make_pair (varLabel, domain));
  }

  unsigned nDefs = xNode.nChildNode ("DEFINITION");
  if (nVars != nDefs) {
    cerr << "error: different number of variables and definitions" << endl;
    abort();
  }

  queue<unsigned> indexes;
  for (unsigned i = 0; i < nDefs; i++) {
    indexes.push (i);
  }

  while (!indexes.empty()) {
    unsigned index = indexes.front();
    indexes.pop();
    XMLNode def = xNode.getChildNode ("DEFINITION", index);
    string varLabel = def.getChildNode("FOR").getText();
    map<string, Domain>::const_iterator iter;
    iter = domains.find (varLabel);
    if (iter == domains.end()) {
      cerr << "error: unknow variable `" << varLabel << "'" << endl;
      abort();
    }
    bool processItLatter = false;
    BnNodeSet parents;
    unsigned nParams = iter->second.size();
    for (int j = 0; j < def.nChildNode ("GIVEN"); j++) {
      string parentLabel = def.getChildNode("GIVEN", j).getText();
      BayesNode* parentNode = getBayesNode (parentLabel);
      if (parentNode) {
        nParams *= parentNode->getDomainSize();
        parents.push_back (parentNode);
      }
      else {
        iter = domains.find (parentLabel);
        if (iter == domains.end()) {
          cerr << "error: unknow parent `" << parentLabel << "'" << endl;
          abort();
        } else {
          // this definition contains a parent that doesn't
          // have a corresponding bayesian node instance yet,
          // so process this definition latter
          indexes.push (index);
          processItLatter = true;
          break;
        }
      }
    }

    if (!processItLatter) {
      unsigned count = 0;
      ParamSet params (nParams);
      stringstream s (def.getChildNode("TABLE").getText());
      while (!s.eof() && count < nParams) {
        s >> params[count];
        count ++;
      }
       if (count != nParams) {
        cerr << "error: invalid number of parameters " ;
        cerr << "for variable `" << varLabel << "'" << endl;
        abort();
      } 
      params = reorderParameters (params, iter->second.size());
      addNode (varLabel, iter->second, parents, params);
    }
  }
  setIndexes();
}



BayesNet::~BayesNet (void)
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
}



BayesNode*
BayesNet::addNode (Vid vid)
{
  indexMap_.insert (make_pair (vid, nodes_.size()));
  nodes_.push_back (new BayesNode (vid));
  return nodes_.back();
}



BayesNode*
BayesNet::addNode (Vid vid,
                   unsigned dsize,
                   int evidence,
                   BnNodeSet& parents,
                   Distribution* dist)
{
  indexMap_.insert (make_pair (vid, nodes_.size()));
  nodes_.push_back (new BayesNode (
      vid, dsize, evidence, parents, dist));
  return nodes_.back();
}



BayesNode*
BayesNet::addNode (string label,
                   Domain domain,
                   BnNodeSet& parents, 
                   ParamSet& params)
{
  indexMap_.insert (make_pair (nodes_.size(), nodes_.size()));
  Distribution* dist = new Distribution (params);
  BayesNode* node = new BayesNode (
      nodes_.size(), label, domain, parents, dist);
  dists_.push_back (dist);
  nodes_.push_back (node);
  return node;
}



BayesNode*
BayesNet::getBayesNode (Vid vid) const
{
  IndexMap::const_iterator it = indexMap_.find (vid);
  if (it == indexMap_.end()) {
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
    if (nodes_[i]->getLabel() == label) {
      node = nodes_[i];
      break;
    }
  }
  return node;
}




Variable*
BayesNet::getVariable (Vid vid) const
{
  return getBayesNode (vid);
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
    if (dists_[i]->id == distId) {
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
BayesNet::getNumberOfNodes (void) const
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



VarSet
BayesNet::getVariables (void) const
{
  VarSet vars;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    vars.push_back (nodes_[i]);
  }
  return vars;
}



BayesNet*
BayesNet::getMinimalRequesiteNetwork (Vid vid) const
{
  return getMinimalRequesiteNetwork (VidSet() = {vid});
}



BayesNet*
BayesNet::getMinimalRequesiteNetwork (const VidSet& queryVids) const
{
  BnNodeSet queryVars;
  for (unsigned i = 0; i < queryVids.size(); i++) {
    assert (getBayesNode (queryVids[i]));
    queryVars.push_back (getBayesNode (queryVids[i]));
  }
  // cout << "query vars: " ;
  // for (unsigned i = 0; i < queryVars.size(); i++) {
  //   cout << queryVars[i]->getLabel() << " " ;
  // }
  // cout << endl;

  vector<StateInfo*> states (nodes_.size(), 0);

  Scheduling scheduling;
  for (BnNodeSet::const_iterator it = queryVars.begin();
      it != queryVars.end(); it++) {
    scheduling.push (ScheduleInfo (*it, false, true));
  }

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
    cout << nodes_[i]->getLabel() << ":\t\t" ;
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
  for (unsigned i = 0; i < nodes_.size(); i++) {
    bool isRequired = false;
    if (states[i]) {
      isRequired = (nodes_[i]->hasEvidence() && states[i]->visited)
                   || 
                   states[i]->markedOnTop;
    }
    if (isRequired) {
      BnNodeSet parents;
      if (states[i]->markedOnTop) {
        const BnNodeSet& ps = nodes_[i]->getParents();
        for (unsigned j = 0; j < ps.size(); j++) {
          BayesNode* parent = bn->getBayesNode (ps[j]->getVarId());
          if (!parent) {
            parent = bn->addNode (ps[j]->getVarId());
          }
          parents.push_back (parent);
        }
      }
      BayesNode* node = bn->getBayesNode (nodes_[i]->getVarId());
      if (node) {
        node->setData      (nodes_[i]->getDomainSize(),
                            nodes_[i]->getEvidence(), parents,
                            nodes_[i]->getDistribution());
      } else {
        node = bn->addNode (nodes_[i]->getVarId(),
                            nodes_[i]->getDomainSize(),
                            nodes_[i]->getEvidence(), parents, 
                            nodes_[i]->getDistribution());
      }
      if (nodes_[i]->hasDomain()) {
        node->setDomain (nodes_[i]->getDomain());
      }
      if (nodes_[i]->hasLabel()) {
        node->setLabel (nodes_[i]->getLabel());
      }
    }
  }
  bn->setIndexes();
}



bool
BayesNet::isSingleConnected (void) const
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
BayesNet::exportToDotFormat (const char* fileName,
                             bool showNeighborless,
                             CVidSet& highlightVids) const
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesNet::exportToDotFile()" << endl;
    abort();
  }

  out << "digraph \"" << fileName << "\" {" << endl;

  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (showNeighborless || nodes_[i]->hasNeighbors()) {
      out << '"' << nodes_[i]->getLabel() << '"' ;
      if (nodes_[i]->hasEvidence()) {
        out << " [style=filled, fillcolor=yellow]" << endl;
      } else {
        out << endl;
      }
    }
  }

  for (unsigned i = 0; i < highlightVids.size(); i++) {
    BayesNode* node = getBayesNode (highlightVids[i]);
    if (node) {
      out << '"' << node->getLabel() << '"' ;
     // out << " [shape=polygon, sides=6]" << endl;
      out << " [shape=box3d]" << endl;
    } else {
      cout << "error: invalid variable id: " << highlightVids[i] << endl;
      abort();
    }
  }

  for (unsigned i = 0; i < nodes_.size(); i++) {
    const BnNodeSet& childs = nodes_[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      out << '"' << nodes_[i]->getLabel() << '"' << " -> " ;
      out << '"' << childs[j]->getLabel() << '"' << endl;
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
    out << "\t<NAME>" << nodes_[i]->getLabel() << "</NAME>" << endl;
    const Domain& domain = nodes_[i]->getDomain();
    for (unsigned j = 0; j < domain.size(); j++) {
      out << "\t<OUTCOME>" << domain[j] << "</OUTCOME>" << endl;
    }
    out << "</VARIABLE>" << endl << endl;
  }

  for (unsigned i = 0; i < nodes_.size(); i++) {
    out << "<DEFINITION>" << endl;
    out << "\t<FOR>" << nodes_[i]->getLabel() << "</FOR>" << endl;
    const BnNodeSet& parents = nodes_[i]->getParents();
    for (unsigned j = 0; j < parents.size(); j++) {
      out << "\t<GIVEN>" << parents[j]->getLabel();
      out << "</GIVEN>" << endl;
    }
    ParamSet params = revertParameterReorder (nodes_[i]->getParameters(),
                                              nodes_[i]->getDomainSize());
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
BayesNet::containsUndirectedCycle (int v,
                                   int p,
                                   vector<bool>& visited) const
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



ParamSet
BayesNet::reorderParameters (CParamSet params,
                             unsigned domainSize) const
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
  unsigned rowSize  = params.size() / domainSize;
  ParamSet reordered;
  while (reordered.size() < params.size()) {
    unsigned idx = count;
    for (unsigned i = 0; i < rowSize; i++) {
      reordered.push_back (params[idx]);
      idx += domainSize;
    }
    count++;
  }
  return reordered;
}



ParamSet
BayesNet::revertParameterReorder (CParamSet params,
                                  unsigned domainSize) const
{
  unsigned count    = 0;
  unsigned rowSize  = params.size() / domainSize;
  ParamSet reordered;
  while (reordered.size() < params.size()) {
    unsigned idx = count;
    for (unsigned i = 0; i < domainSize; i++) {
      reordered.push_back (params[idx]);
      idx += rowSize;
    }
    count ++;
  }
  return reordered;
}

