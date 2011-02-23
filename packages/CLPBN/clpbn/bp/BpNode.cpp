#include <iostream>
#include <cassert>
#include <cmath>

#include "BpNode.h"


bool BpNode::parallelSchedule_ = false;

BpNode::BpNode (string varName, 
                vector<BayesianNode*> parents,
                Distribution* dist,
                int evidence) : BayesianNode (varName, parents, dist, evidence)
{

}



BpNode::~BpNode (void)
{
  delete [] piValues_;
  delete [] lambdaValues_;
  delete [] oldBeliefs_;
  map<BpNode*, double*>::iterator iter;
  for (iter = lambdaMessages_.begin(); iter != lambdaMessages_.end(); ++iter) {
    delete [] iter->second;
  }
  for (iter = piMessages_.begin(); iter != piMessages_.end(); ++iter) {
    delete [] iter->second;
  }
  // FIXME delete new messages
}



void
BpNode::enableParallelSchedule (void)
{
  parallelSchedule_ = true;
}



void 
BpNode::allocateMemory (void)
{
  // FIXME do i need this !?
  int domainSize = getDomainSize();
  piValues_      = new double [domainSize];
  lambdaValues_  = new double [domainSize];
  if (parallelSchedule_) {
    newPiMessages_     = new map<BpNode*, double*>;
    newLambdaMessages_ = new map<BpNode*, double*>;
  }
  oldBeliefs_ = 0;
  vector <BayesianNode*> childs = getChilds();
  for (unsigned int i = 0; i < childs.size(); i++) {
    BpNode* child = static_cast<BpNode*> (childs[i]);
    piMessages_.insert     (make_pair (child, new double [domainSize]));
    lambdaMessages_.insert (make_pair (child, new double [domainSize]));
    if (parallelSchedule_) {
      newPiMessages_->insert (make_pair (child, new double [domainSize]));
      newLambdaMessages_->insert (make_pair (child, new double [domainSize]));
    }
  }
}



double* 
BpNode::getPiValues (void) const
{
  return piValues_;
}



double 
BpNode::getPiValue (int index) const
{
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  return piValues_[index];
}



void 
BpNode::setPiValue (int index, double value)
{
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  piValues_[index] = value;
}



double* 
BpNode::getLambdaValues (void) const
{
  return lambdaValues_;
}



double 
BpNode::getLambdaValue (int index) const
{
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  return lambdaValues_[index];
}



void 
BpNode::setLambdaValue (int index, double value)
{
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  lambdaValues_[index] = value;
}



double*
BpNode::getPiMessages (BpNode* node) const
{
  assert (node);
  map<BpNode*, double*>::const_iterator iter = piMessages_.find (node);
  assert (iter != piMessages_.end());
  return iter->second;
}



double 
BpNode::getPiMessage (BpNode* node, int index) const
{
  assert (node);
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  map<BpNode*, double*>::const_iterator iter = piMessages_.find (node);
  assert (iter != piMessages_.end());
  return iter->second[index];
}



void 
BpNode::setPiMessage (BpNode* node, int index, double probability)
{
  assert (node);
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  map<BpNode*, double*>::const_iterator iter;
  if (parallelSchedule_) {
    // cerr << "set_pi_message" << endl;
    iter = newPiMessages_->find (node); 
    assert (iter != newPiMessages_->end());
  } else {
    iter = piMessages_.find (node);
    assert (iter != piMessages_.end());
  }
  iter->second[index] = probability;
}



double*
BpNode::getLambdaMessages (BpNode* node) const
{
  assert (node);
  map<BpNode*, double*>::const_iterator iter = lambdaMessages_.find (node);
  assert (iter != piMessages_.end());
  return iter->second;
}



double 
BpNode::getLambdaMessage (BpNode* node, int index) const
{
  assert (node);
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  map<BpNode*, double*>::const_iterator iter = lambdaMessages_.find (node);
  assert (iter != piMessages_.end());
  return iter->second[index];
}



void 
BpNode::setLambdaMessage (BpNode* node, int index, double probability)
{
  assert (node);
  const int c = getDomainSize(); 
  assert (index >=0 && index < c);
  map<BpNode*, double*>::const_iterator iter;
  if (parallelSchedule_) {
    //cerr << "set_lambda_message" << endl;
    iter = newLambdaMessages_->find (node);
    assert (iter != newLambdaMessages_->end());
  } else {
    iter = lambdaMessages_.find (node);
    assert (iter != lambdaMessages_.end());
  }
  iter->second[index] = probability;
}



double*
BpNode::getBeliefs (void)
{
  double  sum     = 0.0;
  double* beliefs = new double [getDomainSize()];
  for (int xi = 0; xi < getDomainSize(); xi++) {
    double prod = piValues_[xi] * lambdaValues_[xi];
    beliefs[xi] = prod;
    sum += prod;
  }
  // normalize the beliefs
  for (int xi = 0; xi < getDomainSize(); xi++) {
    beliefs[xi] /= sum;
  }
  return beliefs;
}



double
BpNode::getBeliefChange (void)
{
  double change = 0.0;
  if (!oldBeliefs_) {
    oldBeliefs_ = getBeliefs();
    change = MAX_CHANGE_;
  } else {
    double* currentBeliefs = getBeliefs();
    for (int xi = 0; xi < getDomainSize(); xi++) {
      change += abs (currentBeliefs[xi] - oldBeliefs_[xi]);
    }
    oldBeliefs_ = currentBeliefs;
  }
  //FIXME memory leak
  return change;
}



void
BpNode::normalizeMessages (void)
{
  map<BpNode*, double*>::iterator iter;

  iter = lambdaMessages_.begin();
  while (iter != lambdaMessages_.end()) {
    double* v = iter->second;
    double sum = 0.0;
    for (int xi = 0; xi < getDomainSize(); xi++) {
      sum += v[xi];
    }
    for (int xi = 0; xi < getDomainSize(); xi++) {
      v[xi] /= sum;
    }
    iter ++;
  }
  
  iter = piMessages_.begin();
  while (iter != piMessages_.end()) {
    double* v = iter->second;
    double sum = 0.0;
    for (int xi = 0; xi < getDomainSize(); xi++) {
      sum += v[xi];
    }
    for (int xi = 0; xi < getDomainSize(); xi++) {
      v[xi] /= sum;
    }
    iter ++;
  }
}



void
BpNode::swapMessages (void)
{
  //FIXME fast way to do this
  map<BpNode*, double*>::iterator iter1;
  map<BpNode*, double*>::iterator iter2;

  iter1 = lambdaMessages_.begin();
  iter2 = newLambdaMessages_->begin();
  while (iter1 != lambdaMessages_.end()) {
    double* v1 = iter1->second;
    double* v2 = iter2->second;
    for (int xi = 0; xi < getDomainSize(); xi++) {
      //v1[xi] = v2[xi];
      v1[xi] = (v1[xi] + v2[xi]) / 2;
    }
    iter1 ++;
    iter2 ++;
  }
  
  iter1 = piMessages_.begin();
  iter2 = newPiMessages_->begin();
  while (iter1 != piMessages_.end()) {
    double* v1 = iter1->second;
    double* v2 = iter2->second;
    for (int xi = 0; xi < getDomainSize(); xi++) {
      //v1[xi] = v2[xi];
      v1[xi] = (v1[xi] + v2[xi]) / 2;
    }
    iter1 ++;
    iter2 ++;
  }
}

