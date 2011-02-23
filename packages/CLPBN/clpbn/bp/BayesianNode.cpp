#include <iostream>
#include <iomanip>
#include <cassert>

#include "BayesianNode.h"
#include "CptEntry.h"


BayesianNode::BayesianNode (string varName,
                            vector<BayesianNode*> parents, 
                            Distribution* dist,
                            int evidence)
{
  varName_  = varName;
  parents_  = parents;
  dist_     = dist;
  evidence_ = evidence;
  for (unsigned int i = 0; i < parents.size(); i++) {
    parents[i]->addChild (this);
  }
}



BayesianNode::~BayesianNode (void)
{
}
 


string
BayesianNode::getVariableName (void) const
{
  return varName_;
}



vector<BayesianNode*> 
BayesianNode::getParents (void) const
{
  return parents_;
}



vector<BayesianNode*> 
BayesianNode::getChilds (void) const
{
  return childs_;
}



void 
BayesianNode::addChild (BayesianNode* node)
{
  childs_.push_back (node);
}



double* 
BayesianNode::getParameters (void)
{
  return dist_->params;
}



double* 
BayesianNode::getRow (int rowIndex) const
{
  int offset = getRowSize() * rowIndex;
  return &dist_->params[offset];
}



double 
BayesianNode::getProbability (CptEntry& entry)
{
  int index = entry.getCptIndex();
  return dist_->params[index];
}



void 
BayesianNode::setProbability (CptEntry& entry, double prob)
{
  int index = entry.getCptIndex();
  dist_->params[index] = prob;
}



bool 
BayesianNode::isRoot (void)
{
  return parents_.empty();
}



bool 
BayesianNode::isLeaf (void)
{
  return childs_.empty();
}



int 
BayesianNode::getRowSize (void) const
{ 
  return dist_->nParams / dist_->domain.size();
}



int 
BayesianNode::getCptSize (void)
{
  return dist_->nParams;
}



vector<string>
BayesianNode::getDomain (void) const
{
  return dist_->domain;
}



int 
BayesianNode::getDomainSize (void) const
{
  return dist_->domain.size();
}



vector<CptEntry>
BayesianNode::getCptEntries (const vector<pair<int,int> >& constraints) 
{
  vector<CptEntry> matchedEntries;
  if (constraints.size() > 0 && constraints[0].first == 0) {
    vector<CptEntry> entries = getCptEntriesOfRow (constraints[0].second);
    for (unsigned int i = 0; i < entries.size(); i++) {
      if (entries[i].matchConstraints (constraints)) {
        matchedEntries.push_back (entries[i]);
      }
    }
  }
  else {
    for (unsigned int i = 0; i < dist_->domain.size(); i++) {
      vector<CptEntry> entries = getCptEntriesOfRow (i);
      for (unsigned int j = 0; j < entries.size(); j++) {
        if (entries[j].matchConstraints (constraints)) {
          matchedEntries.push_back (entries[j]);
        }
      }
    }
  }
  return matchedEntries;
}



vector<CptEntry>
BayesianNode::getCptEntriesOfRow (int rowIndex)
{
  int rowSize  = getRowSize();
  int nParents = parents_.size();
  vector<vector<int> > insts (rowSize);

  for (int i = 0; i < rowSize; i++) {  
    insts[i].resize (nParents + 1);
    insts[i][0] = rowIndex;
  }

  int reps = 1;
  for (int i = nParents - 1; i >= 0; i--) {
    int index = 0;
    while (index < rowSize) {
      for (int j = 0; j < parents_[i]->getDomainSize(); j++) {
        for (int k = 0; k < reps; k++) {
          insts[index][i + 1] = j; 
          index++;
        }
      }
    }
    reps *= parents_[i]->getDomainSize();
  }
  
  vector<CptEntry> entries;
  for (int i = 0; i < rowSize; i++ ) {
    entries.push_back (CptEntry ((rowIndex * rowSize) + i, insts[i]));
  }

  return entries;
}



int
BayesianNode::getIndexOfParent (const BayesianNode* myParent) const
{
  for (unsigned int i = 0; i < parents_.size(); i++) {
    if (myParent == parents_[i]) {
      return i;
    }
  }
  return -1;
}



bool
BayesianNode::hasEvidence (void)
{
  return evidence_ != -1;
}



int
BayesianNode::getEvidence (void)
{
  return evidence_;
}



void
BayesianNode::setEvidence (int evidence)
{
  evidence_ = evidence;
}



string
BayesianNode::entryToString (const CptEntry& entry) const
{
  string s = "p(" ;
  vector<int> insts = entry.getDomainInstantiations();
  s += getDomain()[insts[0]]; 
  if (parents_.size() > 0) {
    s += "|" ;
    for (unsigned int i = 1; i < insts.size() - 1; i++) {
      s += parents_[i - 1]->getDomain()[insts[i]] + ",";
    }
    BayesianNode* lastParent = parents_[parents_.size() - 1];
    int lastIndex = insts[insts.size() - 1];
    s += lastParent->getDomain()[lastIndex];
  }
  s += ")" ;
  return s;
}



vector<string> 
BayesianNode::getDomainHeaders (void) const
{
  int rowSize  = getRowSize(); 
  int nParents = parents_.size();
  int reps     = 1;
  vector<string> headers (rowSize);
  for (int i = nParents - 1; i >= 0; i--) {
    vector<string> domain = parents_[i]->getDomain();
    int index = 0;
    while (index < rowSize) {
      for (int j = 0; j < parents_[i]->getDomainSize(); j++) {
        for (int k = 0; k < reps; k++) {
          if (headers[index] != "") {
            headers[index] = domain[j] + "," + headers[index];
          } else {
            headers[index] = domain[j];
          }
          index++;
        }
      }
    }
    reps *= parents_[i]->getDomainSize();
  }
  return headers;
}



ostream& 
operator << (ostream& o, const BayesianNode& node)
{
  o << "Variable: " << node.getVariableName() << endl;

  o << "Domain:   " ;
  vector<string> domain = node.dist_->domain;
  for (unsigned int i = 0; i < domain.size() - 1; i++) {
    o << domain[i] << ", " ;
  }
  if (domain.size() != 0) {
    o << domain[domain.size() - 1];
  }
  o << endl;

  o << "Parents:  " ;
  vector<BayesianNode*> parents = node.getParents();
  if (parents.size() != 0) {
    for (unsigned int i = 0; i < parents.size() - 1; i++) {
      o << parents[i]->getVariableName() << ", " ;
    }
    o << parents[parents.size() - 1]->getVariableName();
  }
  o << endl;
  
  o << "Childs:   " ;
  vector<BayesianNode*> childs = node.getChilds();
  if (childs.size() != 0) {
    for (unsigned int i = 0; i < childs.size() - 1; i++) {
      o << childs[i]->getVariableName() << ", " ;
    }
    o << childs[childs.size() - 1]->getVariableName();
  }
  o << endl;

  const unsigned int MIN_DOMAIN_WIDTH = 4; // min width of first column
  const unsigned int MIN_COMBO_WIDTH = 12; // min width of following columns

  unsigned int domainWidth = domain[0].length();
  for (unsigned int i = 1; i < domain.size(); i++) {
    if (domain[i].length() > domainWidth) {
      domainWidth = domain[i].length();
    }
  }

  domainWidth = (domainWidth < MIN_DOMAIN_WIDTH) 
              ?  MIN_DOMAIN_WIDTH 
              :  domainWidth;

  o << left << setw (domainWidth) << "cpt" << right;

  vector<int> widths;
  int lineWidth = domainWidth;
  vector<string> headers = node.getDomainHeaders();
  if (!headers.empty()) {
    for (unsigned int i = 0; i < headers.size(); i++) {
      unsigned int len = headers[i].length();
      int w = (len < MIN_COMBO_WIDTH) ? MIN_COMBO_WIDTH : len;
      widths.push_back (w);
      o << setw (w) << headers[i];
      lineWidth += w;
    }
    o << endl;
  } else {
    cout << endl;
    widths.push_back (domainWidth);
    lineWidth += MIN_COMBO_WIDTH;
  }

  for (int i = 0; i < lineWidth; i++) {
    o << "-" ;
  }
  o << endl;

  for (unsigned int i = 0; i < domain.size(); i++) {   
    double* row = node.getRow (i);
    o << left << setw (domainWidth) << domain[i] << right;
    for (int j = 0; j < node.getRowSize(); j++) {
      o << setw (widths[j]) << row[j];
    }
    o << endl;
  }
  o << endl;

  return o;
}

