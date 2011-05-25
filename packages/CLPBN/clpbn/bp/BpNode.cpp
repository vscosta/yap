#include <iostream>
#include <cassert>
#include <cmath>

#include "BpNode.h"

bool BpNode::calculateMessageResidual_ = true;


BpNode::BpNode (BayesNode* node)
{
  ds_ = node->getDomainSize();
  const NodeSet& childs = node->getChilds();
  piVals_.resize (ds_, 1);
  ldVals_.resize (ds_, 1);
  if (calculateMessageResidual_) {
    piResiduals_.resize (childs.size(), 0.0);
    ldResiduals_.resize (childs.size(), 0.0);
  }
  childs_ = &childs;
  for (unsigned i = 0; i < childs.size(); i++) {
    //indexMap_.insert (make_pair (childs[i]->getVarId(), i));
    currPiMsgs_.push_back (ParamSet (ds_, 1));
    currLdMsgs_.push_back (ParamSet (ds_, 1));
    nextPiMsgs_.push_back (ParamSet (ds_, 1));
    nextLdMsgs_.push_back (ParamSet (ds_, 1));
  }
}



ParamSet
BpNode::getBeliefs (void) const
{
  double sum = 0.0;
  ParamSet beliefs (ds_);
  for (int xi = 0; xi < ds_; xi++) {
    double prod = piVals_[xi] * ldVals_[xi];
    beliefs[xi] = prod;
    sum += prod;
  }
  assert (sum);
  //normalize the beliefs
  for (int xi = 0; xi < ds_; xi++) {
    beliefs[xi] /= sum;
  }
  return beliefs;
}



double
BpNode::getPiValue (int idx) const
{
  assert (idx >=0 && idx < ds_);
  return piVals_[idx];
}



void
BpNode::setPiValue (int idx, double value)
{
  assert (idx >=0 && idx < ds_);
  piVals_[idx] = value;
}



double 
BpNode::getLambdaValue (int idx) const
{
  assert (idx >=0 && idx < ds_);
  return ldVals_[idx];
}



void 
BpNode::setLambdaValue (int idx, double value)
{
  assert (idx >=0 && idx < ds_);
  ldVals_[idx] = value;
}



ParamSet&
BpNode::getPiValues (void)
{
  return piVals_;
}



ParamSet&
BpNode::getLambdaValues (void)
{
  return ldVals_;
}



double 
BpNode::getPiMessageValue (const BayesNode* destination, int idx) const
{
  assert (idx >=0 && idx < ds_);
  return currPiMsgs_[getIndex(destination)][idx];
}



double 
BpNode::getLambdaMessageValue (const BayesNode* source, int idx) const
{
  assert (idx >=0 && idx < ds_);
  return currLdMsgs_[getIndex(source)][idx];
}



const ParamSet&
BpNode::getPiMessage (const BayesNode* destination) const
{
  return currPiMsgs_[getIndex(destination)];
}



const ParamSet&
BpNode::getLambdaMessage (const BayesNode* source) const
{
  return currLdMsgs_[getIndex(source)];
}



ParamSet&
BpNode::piNextMessageReference (const BayesNode* destination)
{
  return nextPiMsgs_[getIndex(destination)];
}



ParamSet&
BpNode::lambdaNextMessageReference (const BayesNode* source)
{
  return nextLdMsgs_[getIndex(source)];
}



void
BpNode::updatePiMessage (const BayesNode* destination)
{
  int idx = getIndex (destination);
  currPiMsgs_[idx] = nextPiMsgs_[idx];
  Util::normalize (currPiMsgs_[idx]);
}



void
BpNode::updateLambdaMessage (const BayesNode* source)
{
  int idx = getIndex (source);
  currLdMsgs_[idx] = nextLdMsgs_[idx];
  Util::normalize (currLdMsgs_[idx]);
}



double
BpNode::getBeliefChange (void)
{
  double change = 0.0;
  if (oldBeliefs_.size() == 0) {
    oldBeliefs_ = getBeliefs();
    change = 9999999999.0;
  } else {
    ParamSet currentBeliefs = getBeliefs();
    for (int xi = 0; xi < ds_; xi++) {
      change += abs (currentBeliefs[xi] - oldBeliefs_[xi]);
    }
    oldBeliefs_ = currentBeliefs;
  }
  return change;
}



void
BpNode::updatePiResidual (const BayesNode* destination)
{
  int idx = getIndex (destination);
  Util::normalize (nextPiMsgs_[idx]);
  //piResiduals_[idx] = Util::getL1dist (
  //    currPiMsgs_[idx], nextPiMsgs_[idx]);
  piResiduals_[idx] = Util::getMaxNorm (
      currPiMsgs_[idx], nextPiMsgs_[idx]);
}



void
BpNode::updateLambdaResidual (const BayesNode* source)
{
  int idx = getIndex (source);
  Util::normalize (nextLdMsgs_[idx]);
  //ldResiduals_[idx] = Util::getL1dist (
  //    currLdMsgs_[idx], nextLdMsgs_[idx]);
  ldResiduals_[idx] = Util::getMaxNorm (
      currLdMsgs_[idx], nextLdMsgs_[idx]);
}



void
BpNode::clearPiResidual (const BayesNode* destination)
{
  piResiduals_[getIndex(destination)] = 0;
}



void
BpNode::clearLambdaResidual (const BayesNode* source)
{
  ldResiduals_[getIndex(source)] = 0;
}



bool
BpNode::hasReceivedChildInfluence (void) const
{
  // if all lambda values are equal, then neither
  // this node neither its descendents have evidence,
  // we can use this to don't send lambda messages his parents
  bool childInfluenced = false;
  for (int xi = 1; xi < ds_; xi++) {
    if (ldVals_[xi] != ldVals_[0]) {
      childInfluenced = true;
      break;
    }
  }
  return childInfluenced;
}

