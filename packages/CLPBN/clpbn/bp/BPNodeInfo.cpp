#include <cassert>
#include <cmath>

#include <iostream>

#include "BPNodeInfo.h"
#include "BPSolver.h"

BPNodeInfo::BPNodeInfo (BayesNode* node)
{
  node_ = node;
  ds_   = node->getDomainSize();
  piValsCalc_ = false;
  ldValsCalc_ = false;
  nPiMsgsRcv_ = 0;
  nLdMsgsRcv_ = 0;
  piVals_.resize (ds_, 1);
  ldVals_.resize (ds_, 1);
  const BnNodeSet& childs  = node->getChilds();
  for (unsigned i = 0; i < childs.size(); i++) {
    cmsgs_.insert (make_pair (childs[i], false));
  }
  const BnNodeSet& parents = node->getParents();
  for (unsigned i = 0; i < parents.size(); i++) {
    pmsgs_.insert (make_pair (parents[i], false));
  }
}



ParamSet
BPNodeInfo::getBeliefs (void) const
{
  double sum = 0.0;
  ParamSet beliefs (ds_);
  for (unsigned xi = 0; xi < ds_; xi++) {
    double prod = piVals_[xi] * ldVals_[xi];
    beliefs[xi] = prod;
    sum += prod;
  }
  assert (sum);
  //normalize the beliefs
  for (unsigned xi = 0; xi < ds_; xi++) {
    beliefs[xi] /= sum;
  }
  return beliefs;
}



bool
BPNodeInfo::readyToSendPiMsgTo (const BayesNode* child) const
{
  for (unsigned i = 0; i < inChildLinks_.size(); i++) {
    if (inChildLinks_[i]->getSource() != child
        && !inChildLinks_[i]->messageWasSended())  {
      return false;
    }
  }
  return true;
}



bool
BPNodeInfo::readyToSendLambdaMsgTo (const BayesNode* parent) const
{
  for (unsigned i = 0; i < inParentLinks_.size(); i++) {
    if (inParentLinks_[i]->getSource() != parent
        && !inParentLinks_[i]->messageWasSended())  {
      return false;
    }
  }
  return true;
}



double
BPNodeInfo::getPiValue (unsigned idx) const
{
  assert (idx >=0 && idx < ds_);
  return piVals_[idx];
}



void
BPNodeInfo::setPiValue (unsigned idx, Param value)
{
  assert (idx >=0 && idx < ds_);
  piVals_[idx] = value;
}



double 
BPNodeInfo::getLambdaValue (unsigned idx) const
{
  assert (idx >=0 && idx < ds_);
  return ldVals_[idx];
}



void 
BPNodeInfo::setLambdaValue (unsigned idx, Param value)
{
  assert (idx >=0 && idx < ds_);
  ldVals_[idx] = value;
}



double
BPNodeInfo::getBeliefChange (void)
{
  double change = 0.0;
  if (oldBeliefs_.size() == 0) {
    oldBeliefs_ = getBeliefs();
    change = 9999999999.0;
  } else {
    ParamSet currentBeliefs = getBeliefs();
    for (unsigned xi = 0; xi < ds_; xi++) {
      change += abs (currentBeliefs[xi] - oldBeliefs_[xi]);
    }
    oldBeliefs_ = currentBeliefs;
  }
  return change;
}



bool
BPNodeInfo::receivedBottomInfluence (void) const
{
  // if all lambda values are equal, then neither
  // this node neither its descendents have evidence,
  // we can use this to don't send lambda messages his parents
  bool childInfluenced = false;
  for (unsigned xi = 1; xi < ds_; xi++) {
    if (ldVals_[xi] != ldVals_[0]) {
      childInfluenced = true;
      break;
    }
  }
  return childInfluenced;
}

