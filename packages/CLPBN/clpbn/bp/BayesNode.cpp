#include <cstdlib>
#include <cassert>

#include <iostream>
#include <sstream>
#include <iomanip>

#include "BayesNode.h"


BayesNode::BayesNode (Vid vid,
                      unsigned dsize,
                      int evidence,
                      const BnNodeSet& parents,
                      Distribution* dist) : Variable (vid, dsize, evidence)
{
  parents_  = parents;
  dist_     = dist;
  for (unsigned int i = 0; i < parents.size(); i++) {
    parents[i]->addChild (this);
  }
}



BayesNode::BayesNode (Vid vid,
                      string label,
                      const Domain& domain,
                      const BnNodeSet& parents,
                      Distribution* dist) : Variable (vid, domain,
                                                      NO_EVIDENCE, label)
{
  parents_  = parents;
  dist_     = dist;
  for (unsigned int i = 0; i < parents.size(); i++) {
    parents[i]->addChild (this);
  }
}



void
BayesNode::setData (unsigned dsize,
                    int evidence,
                    const BnNodeSet& parents,
                    Distribution* dist)
{
  setDomainSize (dsize);
  setEvidence (evidence);
  parents_   = parents;
  dist_      = dist;
  for (unsigned int i = 0; i < parents.size(); i++) {
    parents[i]->addChild (this);
  } 
}



void 
BayesNode::addChild (BayesNode* node)
{
  childs_.push_back (node);
}



Distribution*
BayesNode::getDistribution (void)
{
  return dist_;
}



const ParamSet&
BayesNode::getParameters (void)
{
  return dist_->params;
}



ParamSet
BayesNode::getRow (int rowIndex) const
{
  int rowSize = getRowSize();
  int offset  = rowSize * rowIndex;
  ParamSet row (rowSize);
  for (int i = 0; i < rowSize; i++) {
    row[i] = dist_->params[offset + i] ;
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
  return dist_->params.size();
}



const vector<CptEntry>&
BayesNode::getCptEntries (void)
{
  if (dist_->entries.size() == 0) {
    unsigned rowSize  = getRowSize();
    vector<DConf> confs (rowSize);

    for (unsigned i = 0; i < rowSize; i++) {  
      confs[i].resize (parents_.size());
    }

    unsigned nReps = 1;
    for (int i = parents_.size() - 1; i >= 0; i--) {
      unsigned index = 0;
      while (index < rowSize) {
        for (unsigned j = 0; j < parents_[i]->getDomainSize(); j++) {
          for (unsigned r = 0; r < nReps; r++) {
            confs[index][i] = j;
            index++;
          }
        }
      }
      nReps *= parents_[i]->getDomainSize();
    }
  
    dist_->entries.reserve (rowSize);
    for (unsigned i = 0; i < rowSize; i++) {
      dist_->entries.push_back (CptEntry (i, confs[i]));
    }
  }
  return dist_->entries;
}



int
BayesNode::getIndexOfParent (const BayesNode* parent) const
{
  for (unsigned int i = 0; i < parents_.size(); i++) {
    if (parents_[i] == parent) {
      return i;
    }
  }
  return -1;
}



string
BayesNode::cptEntryToString (const CptEntry& entry) const
{
  stringstream ss;
  ss << "p(" ;
  const DConf& conf = entry.getDomainConfiguration();
  int row = entry.getParameterIndex() / getRowSize();
  ss << getDomain()[row]; 
  if (parents_.size() > 0) {
    ss << "|" ;
    for (unsigned int i = 0; i < conf.size(); i++) {
      if (i != 0) {
        ss << ",";
      }
      ss << parents_[i]->getDomain()[conf[i]];
    }
  }
  ss << ")" ;
  return ss.str();
}



string
BayesNode::cptEntryToString (int row, const CptEntry& entry) const
{
  stringstream ss;
  ss << "p(" ;
  const DConf& conf = entry.getDomainConfiguration();
  ss << getDomain()[row]; 
  if (parents_.size() > 0) {
    ss << "|" ;
    for (unsigned int i = 0; i < conf.size(); i++) {
      if (i != 0) {
        ss << ",";
      }
      ss << parents_[i]->getDomain()[conf[i]];
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
    Domain domain = parents_[i]->getDomain();
    unsigned index = 0;
    while (index < rowSize) {
      for (unsigned j = 0; j < parents_[i]->getDomainSize(); j++) {
        for (unsigned r = 0; r < nReps; r++) {
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



ostream& 
operator << (ostream& o, const BayesNode& node)
{
  o << "variable " << node.getIndex() << endl;
  o << "Var Id:   " << node.getVarId() << endl;
  o << "Label:    " << node.getLabel() << endl;
  
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
      o << parents[i]->getLabel() << ", " ;
    }
    o << parents[parents.size() - 1]->getLabel();
  }
  o << endl;
  
  o << "Childs:   " ;
  const BnNodeSet& childs = node.getChilds();
  if (childs.size() != 0) {
    for (unsigned int i = 0; i < childs.size() - 1; i++) {
      o << childs[i]->getLabel() << ", " ;
    }
    o << childs[childs.size() - 1]->getLabel();
  }
  o << endl;

  o << "Domain:   " ;
  Domain domain = node.getDomain();
  for (unsigned int i = 0; i < domain.size() - 1; i++) {
    o << domain[i] << ", " ;
  }
  if (domain.size() != 0) {
    o << domain[domain.size() - 1];
  }
  o << endl;

  // min width of first column
  const unsigned int MIN_DOMAIN_WIDTH = 4;
  // min width of following columns
  const unsigned int MIN_COMBO_WIDTH = 12;

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
    ParamSet row = node.getRow (i);
    o << left << setw (domainWidth) << domain[i] << right;
    for (unsigned j = 0; j < node.getRowSize(); j++) {
      o << setw (widths[j]) << row[j];
    }
    o << endl;
  }
  o << endl;

  return o;
}

