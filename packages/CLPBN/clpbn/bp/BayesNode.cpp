#include <cstdlib>
#include <cassert>

#include <iomanip>
#include <iostream>
#include <sstream>

#include "BayesNode.h"


void
BayesNode::setParams (const Params& params)
{
  params_ = params;
}



void
BayesNode::setParents (const BnNodeSet& parents)
{
  parents_ = parents;
  for (unsigned int i = 0; i < parents.size(); i++) {
    parents[i]->addChild (this);
  }
}



void 
BayesNode::addChild (BayesNode* node)
{
  childs_.push_back (node);
}



Params
BayesNode::getRow (int rowIndex) const
{
  int rowSize = getRowSize();
  int offset  = rowSize * rowIndex;
  Params row (rowSize);
  for (int i = 0; i < rowSize; i++) {
    row[i] = params_[offset + i] ;
  }
  return row;
}



bool 
BayesNode::isRoot (void)
{
  return getParents().empty();
}



bool 
BayesNode::isLeaf (void)
{
  return getChilds().empty();
}



bool
BayesNode::hasNeighbors (void) const
{
  return childs_.size() != 0 || parents_.size() != 0;
}


int 
BayesNode::getCptSize (void)
{
  return params_.size();
}



int
BayesNode::indexOfParent (const BayesNode* parent) const
{
  for (unsigned int i = 0; i < parents_.size(); i++) {
    if (parents_[i] == parent) {
      return i;
    }
  }
  return -1;
}



string
BayesNode::cptEntryToString (
    int row,
    const vector<unsigned>& stateConf) const
{
  stringstream ss;
  ss << "p(" ;
  ss << states()[row]; 
  if (parents_.size() > 0) {
    ss << "|" ;
    for (unsigned int i = 0; i < stateConf.size(); i++) {
      if (i != 0) {
        ss << ",";
      }
      ss << parents_[i]->states()[stateConf[i]];
    }
  }
  ss << ")" ;
  return ss.str();
}



vector<string> 
BayesNode::getDomainHeaders (void) const
{
  unsigned nParents = parents_.size();
  unsigned rowSize  = getRowSize(); 
  unsigned nReps     = 1;
  vector<string> headers (rowSize);
  for (int i = nParents - 1; i >= 0; i--) {
    States states = parents_[i]->states();
    unsigned index = 0;
    while (index < rowSize) {
      for (unsigned j = 0; j < parents_[i]->range(); j++) {
        for (unsigned r = 0; r < nReps; r++) {
          if (headers[index] != "") {
            headers[index] = states[j] + "," + headers[index];
          } else {
            headers[index] = states[j];
          }
          index++;
        }
      }
    }
    nReps *= parents_[i]->range();
  }
  return headers;
}



ostream& 
operator << (ostream& o, const BayesNode& node)
{
  o << "variable " << node.getIndex() << endl;
  o << "Var Id:   " << node.varId() << endl;
  o << "Label:    " << node.label() << endl;
  
  o << "Evidence: " ;
  if (node.hasEvidence()) {
    o << node.getEvidence();
  }
  else {
    o << "no" ;
  }
  o << endl;

  o << "Parents:  " ;
  const BnNodeSet& parents = node.getParents();
  if (parents.size() != 0) {
    for (unsigned int i = 0; i < parents.size() - 1; i++) {
      o << parents[i]->label() << ", " ;
    }
    o << parents[parents.size() - 1]->label();
  }
  o << endl;
  
  o << "Childs:   " ;
  const BnNodeSet& childs = node.getChilds();
  if (childs.size() != 0) {
    for (unsigned int i = 0; i < childs.size() - 1; i++) {
      o << childs[i]->label() << ", " ;
    }
    o << childs[childs.size() - 1]->label();
  }
  o << endl;

  o << "Domain:   " ;
  States states = node.states();
  for (unsigned int i = 0; i < states.size() - 1; i++) {
    o << states[i] << ", " ;
  }
  if (states.size() != 0) {
    o << states[states.size() - 1];
  }
  o << endl;

  // min width of first column
  const unsigned int MIN_DOMAIN_WIDTH = 4;
  // min width of following columns
  const unsigned int MIN_COMBO_WIDTH = 12;

  unsigned int domainWidth = states[0].length();
  for (unsigned int i = 1; i < states.size(); i++) {
    if (states[i].length() > domainWidth) {
      domainWidth = states[i].length();
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

  for (unsigned int i = 0; i < states.size(); i++) {   
    Params row = node.getRow (i);
    o << left << setw (domainWidth) << states[i] << right;
    for (unsigned j = 0; j < node.getRowSize(); j++) {
      o << setw (widths[j]) << row[j];
    }
    o << endl;
  }
  o << endl;

  return o;
}

