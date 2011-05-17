#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <cstdlib>
#include <map>

#include "xmlParser/xmlParser.h"

#include "BayesNet.h"


BayesNet::BayesNet (void)
{
}



BayesNet::BayesNet (const char* fileName)
{
  map<string, Domain> domains;
  XMLNode xMainNode = XMLNode::openFileHelper (fileName, "BIF");
  // only the first network is parsed, others are ignored
  XMLNode xNode = xMainNode.getChildNode ("NETWORK");
  int nVars = xNode.nChildNode ("VARIABLE");
  for (int i = 0; i < nVars; i++) {
    XMLNode var = xNode.getChildNode ("VARIABLE", i);
    string type = var.getAttribute ("TYPE");
    if (type != "nature") {
      cerr << "error: only \"nature\" variables are supported" << endl;
      abort();
    }
    Domain domain;
    string label = var.getChildNode("NAME").getText();
    int domainSize = var.nChildNode ("OUTCOME");
    for (int j = 0; j < domainSize; j++) {
      if (var.getChildNode("OUTCOME", j).getText() == 0) {
        stringstream ss;
        ss << j + 1;
        domain.push_back (ss.str());
      } else {
        domain.push_back (var.getChildNode("OUTCOME", j).getText());
      }
    }
    domains.insert (make_pair (label, domain));
  }

  int nDefs = xNode.nChildNode ("DEFINITION");
  if (nVars != nDefs) {
    cerr << "error: different number of variables and definitions";
    cerr << endl;
  }

  queue<int> indexes;
  for (int i = 0; i < nDefs; i++) {
    indexes.push (i);
  }

  while (!indexes.empty()) {
    int index = indexes.front();
    indexes.pop();
    XMLNode def = xNode.getChildNode ("DEFINITION", index);
    string label = def.getChildNode("FOR").getText();
    map<string, Domain>::const_iterator iter;
    iter = domains.find (label);
    if (iter == domains.end()) {
      cerr << "error: unknow variable `" << label << "'" << endl;
      abort();
    }
    bool processItLatter = false;
    NodeSet parents;
    int nParams = iter->second.size();
    for (int j = 0; j < def.nChildNode ("GIVEN"); j++) {
      string parentLabel = def.getChildNode("GIVEN", j).getText();
      BayesNode* parentNode = getNode (parentLabel);
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
      int count = 0;
      ParamSet params (nParams);
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
      params = reorderParameters (params, iter->second.size());
      addNode (label, iter->second, parents, params);
    }
  }
  setIndexes();
}



BayesNet::~BayesNet (void)
{
  Statistics::writeStats();
  for (unsigned i = 0; i < nodes_.size(); i++) {
    delete nodes_[i];
  }
}



BayesNode*
BayesNet::addNode (unsigned varId)
{
  indexMap_.insert (make_pair (varId, nodes_.size()));
  nodes_.push_back (new BayesNode (varId));
  return nodes_.back();
}



BayesNode*
BayesNet::addNode (unsigned varId,
                   unsigned dsize,
                   int evidence,
                   NodeSet& parents,
                   Distribution* dist)
{
  indexMap_.insert (make_pair (varId, nodes_.size()));
  nodes_.push_back (new BayesNode (
      varId, dsize, evidence, parents, dist));
  return nodes_.back();
}



BayesNode*
BayesNet::addNode (string label,
                   Domain domain,
                   NodeSet& parents, 
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
BayesNet::getNode (unsigned varId) const
{
  IndexMap::const_iterator it = indexMap_.find(varId);
  if (it == indexMap_.end()) {
    return 0;
  } else {
    return nodes_[it->second];
  }
}



BayesNode*
BayesNet::getNode (string label) const
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



const NodeSet&
BayesNet::getNodes (void) const
{
  return nodes_;
}



int
BayesNet::getNumberOfNodes (void) const
{
  return nodes_.size();
}



NodeSet
BayesNet::getRootNodes (void) const
{
  NodeSet roots;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (nodes_[i]->isRoot()) {
      roots.push_back (nodes_[i]);
    }
  }
  return roots;
}



NodeSet
BayesNet::getLeafNodes (void) const
{
  NodeSet leafs;
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
BayesNet::pruneNetwork (BayesNode* queryNode) const
{
  NodeSet queryNodes;
  queryNodes.push_back (queryNode);
  return pruneNetwork (queryNodes);
}



BayesNet*
BayesNet::pruneNetwork (const NodeSet& interestedVars) const
{
  /*
  cout << "interested vars: " ;
  for (unsigned i = 0; i < interestedVars.size(); i++) {
    cout << interestedVars[i]->getLabel() << " " ;
  }
  cout << endl;
  */
  vector<StateInfo*> states (nodes_.size(), 0);

  Scheduling scheduling;
  for (NodeSet::const_iterator it = interestedVars.begin();
      it != interestedVars.end(); it++) {
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
      NodeSet parents;
      if (states[i]->markedOnTop) {
        const NodeSet& ps = nodes_[i]->getParents();
        for (unsigned j = 0; j < ps.size(); j++) {
          BayesNode* parent = bn->getNode (ps[j]->getVarId());
          if (!parent) {
            parent = bn->addNode (ps[j]->getVarId());
          }
          parents.push_back (parent);
        }
      }
      BayesNode* node = bn->getNode (nodes_[i]->getVarId());
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

/*
void
BayesNet::constructGraph (BayesNet* bn,
                          const vector<StateInfo*>& states) const
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    if (states[i]) {
      if (nodes_[i]->hasEvidence() && states[i]->visited) {
        NodeSet parents;
        if (states[i]->markedOnTop) {
          const NodeSet& ps = nodes_[i]->getParents();
          for (unsigned j = 0; j < ps.size(); j++) {
            BayesNode* parent = bn->getNode (ps[j]->getVarId());
            if (parent == 0) {
              parent = bn->addNode (ps[j]->getVarId());
            }
            parents.push_back (parent);
          }
        }

        BayesNode* n = bn->getNode (nodes_[i]->getVarId());
        if (n) {
          n->setData  (nodes_[i]->getDomainSize(),
                       nodes_[i]->getEvidence(), parents,
                       nodes_[i]->getDistribution());
        } else {
          bn->addNode (nodes_[i]->getVarId(),
                       nodes_[i]->getDomainSize(),
                       nodes_[i]->getEvidence(), parents, 
                       nodes_[i]->getDistribution());
        }
        
      } else if (states[i]->markedOnTop) {
        NodeSet parents;
        const NodeSet& ps = nodes_[i]->getParents();
        for (unsigned j = 0; j < ps.size(); j++) {
          BayesNode* parent = bn->getNode (ps[j]->getVarId());
          if (parent == 0) {
            parent = bn->addNode (ps[j]->getVarId());
          }
          parents.push_back (parent);
        }

        BayesNode* n = bn->getNode (nodes_[i]->getVarId());
        if (n) {
          n->setData  (nodes_[i]->getDomainSize(),
                       nodes_[i]->getEvidence(), parents,
                       nodes_[i]->getDistribution());
        } else {
          bn->addNode (nodes_[i]->getVarId(),
                       nodes_[i]->getDomainSize(),
                       nodes_[i]->getEvidence(), parents,
                       nodes_[i]->getDistribution());
        }
      }
    }
  }
}*/



bool
BayesNet::isSingleConnected (void) const
{
  return !containsUndirectedCycle();
}



vector<DomainConf>
BayesNet::getDomainConfigurationsOf (const NodeSet& nodes)
{
  int nConfs = 1;
  for (unsigned i = 0; i < nodes.size(); i++) {
    nConfs *= nodes[i]->getDomainSize();
  }

  vector<DomainConf> confs (nConfs);
  for (int i = 0; i < nConfs; i++) {
    confs[i].resize (nodes.size());
  }

  int nReps = 1;
  for (int i = nodes.size() - 1; i >= 0; i--) {
    int index = 0;
    while (index < nConfs) {
      for (int j = 0; j < nodes[i]->getDomainSize(); j++) {
        for (int r = 0; r < nReps; r++) {
          confs[index][i] = j; 
          index++;
        }
      }
    }
    nReps *= nodes[i]->getDomainSize();
  }

  return confs;
}



vector<string>
BayesNet::getInstantiations (const NodeSet& parents_)
{
  int nParents = parents_.size();
  int rowSize = 1;
  for (unsigned i = 0; i < parents_.size(); i++) {
    rowSize *= parents_[i]->getDomainSize();
  }
  int nReps     = 1;
  vector<string> headers (rowSize);
  for (int i = nParents - 1; i >= 0; i--) {
    Domain domain = parents_[i]->getDomain();
    int index = 0;
    while (index < rowSize) {
      for (int j = 0; j < parents_[i]->getDomainSize(); j++) {
        for (int r = 0; r < nReps; r++) {
          if (headers[index] != "") {
            headers[index] = domain[j] + "," + headers[index];
          } else {
            headers[index] = domain[j];
          }
          index++;
        }
      }
    }
    nReps *= parents_[i]->getDomainSize();
  }
  return headers;
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
BayesNet::printNetwork (void) const
{
  for (unsigned i = 0; i < nodes_.size(); i++) {
    cout << *nodes_[i];
  }
}



void
BayesNet::printNetworkToFile (const char* fileName) const
{
  string s = "../../" ;
  s += fileName;
  ofstream out (s.c_str());
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesNet::printToFile()" << endl;
    abort();
  }
  for (unsigned i = 0; i < nodes_.size(); i++) {
    out << *nodes_[i];
  }
  out.close();
}
 


void
BayesNet::exportToDotFile (const char* fileName,
                           bool showNeighborless,
                           const NodeSet& highlightNodes) const
{
  string s = "../../" ;
  s+= fileName;
  ofstream out (s.c_str());
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "BayesNet::exportToDotFile()" << endl;
    abort();
  }

  out << "digraph \"" << fileName << "\" {" << endl;
  for (unsigned i = 0; i < nodes_.size(); i++) {
    const NodeSet& childs = nodes_[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      out << '"' << nodes_[i]->getLabel() << '"' << " -> " ;
      out << '"' << childs[j]->getLabel() << '"' << endl;
    }
  }

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

  for (unsigned i = 0; i < highlightNodes.size(); i++) {
    out << '"' << highlightNodes[i]->getLabel() << '"' ;
    out << " [shape=box]" << endl;
  }

  out << "}" << endl;
  out.close();
}



void
BayesNet::exportToBifFile (const char* fileName) const
{
  string s = "../../" ;
  s += fileName;
  ofstream out (s.c_str());
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
    const NodeSet& parents = nodes_[i]->getParents();
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
  const NodeSet& parents = nodes_[v]->getParents();
  const NodeSet& childs  = nodes_[v]->getChilds();
  for (unsigned i = 0; i < parents.size(); i++) {
    adjacencies.push_back (parents[i]->getIndex());
  }
  for (unsigned i = 0; i < childs.size(); i++) {
    adjacencies.push_back (childs[i]->getIndex());
  }
  return adjacencies;
}



ParamSet
BayesNet::reorderParameters (const ParamSet& params,
                             int domainSize) const
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
BayesNet::revertParameterReorder (const ParamSet& params,
                                  int domainSize) const
{
  unsigned count    = 0;
  unsigned rowSize  = params.size() / domainSize;
  ParamSet reordered;
  while (reordered.size() < params.size()) {
    unsigned idx = count;
    for (int i = 0; i < domainSize; i++) {
      reordered.push_back (params[idx]);
      idx += rowSize;
    }
    count ++;
  }
  return reordered;
}

