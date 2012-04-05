#include <cstdlib>
#include <limits>
#include <time.h>

#include <algorithm>

#include <iostream>
#include <sstream>
#include <iomanip>

#include "BnBpSolver.h"
#include "Indexer.h"

BnBpSolver::BnBpSolver (const BayesNet& bn) : Solver (&bn)
{ 
  bayesNet_ = &bn;
}



BnBpSolver::~BnBpSolver (void)
{
  for (unsigned i = 0; i < nodesI_.size(); i++) {
    delete nodesI_[i];
  }
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



void
BnBpSolver::runSolver (void)
{
  clock_t start;
  if (Constants::COLLECT_STATS) {
    start = clock();
  }
  initializeSolver();
  runLoopySolver();
  if (Constants::DEBUG >= 2) {
    cout << endl;
    if (nIters_ < BpOptions::maxIter) {
      cout << "Belief propagation converged in " ; 
      cout << nIters_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
  }
  
  unsigned size = bayesNet_->nrNodes();
  if (Constants::COLLECT_STATS) {
    unsigned nIters = 0;
    bool loopy = bayesNet_->isPolyTree() == false;
    if (loopy) nIters = nIters_;
    double time = (double (clock() - start)) / CLOCKS_PER_SEC;
    Statistics::updateStatistics (size, loopy, nIters, time);
  }
}



Params
BnBpSolver::getPosterioriOf (VarId vid)
{
  BayesNode* node = bayesNet_->getBayesNode (vid);
  assert (node);
  return nodesI_[node->getIndex()]->getBeliefs();
}



Params
BnBpSolver::getJointDistributionOf (const VarIds& jointVarIds)
{
  if (Constants::DEBUG >= 2) {
    cout << "calculating joint distribution on: " ;
    for (unsigned i = 0; i < jointVarIds.size(); i++) {
      VarNode* var = bayesNet_->getBayesNode (jointVarIds[i]);
      cout << var->label() << " " ;
    }
    cout << endl;
  }
  return getJointByConditioning (jointVarIds);
}



void 
BnBpSolver::initializeSolver (void)
{
  const BnNodeSet& nodes = bayesNet_->getBayesNodes();
  for (unsigned i = 0; i < nodesI_.size(); i++) {
    delete nodesI_[i];
  }
  nodesI_.clear();
  nodesI_.reserve (nodes.size());
  links_.clear();
  sortedOrder_.clear();
  linkMap_.clear();

  for (unsigned i = 0; i < nodes.size(); i++) {
    nodesI_.push_back (new BpNodeInfo (nodes[i]));
  }

  BnNodeSet roots = bayesNet_->getRootNodes();
  for (unsigned i = 0; i < roots.size(); i++) {
    const Params& params = roots[i]->params();
    Params& piVals = ninf(roots[i])->getPiValues();
    for (unsigned ri = 0; ri < roots[i]->range(); ri++) {
      piVals[ri] = params[ri];
    }
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    const BnNodeSet& parents = nodes[i]->getParents();
    for (unsigned j = 0; j < parents.size(); j++) {
      BpLink* newLink = new BpLink (
          parents[j], nodes[i], LinkOrientation::DOWN);
      links_.push_back (newLink);
      ninf(nodes[i])->addIncomingParentLink (newLink);
      ninf(parents[j])->addOutcomingChildLink (newLink);
    }
    const BnNodeSet& childs = nodes[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      BpLink* newLink = new BpLink (
          childs[j], nodes[i], LinkOrientation::UP);
      links_.push_back (newLink);
      ninf(nodes[i])->addIncomingChildLink (newLink);
      ninf(childs[j])->addOutcomingParentLink (newLink);
    }
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->hasEvidence()) {
      Params& piVals = ninf(nodes[i])->getPiValues();
      Params& ldVals = ninf(nodes[i])->getLambdaValues();
      for (unsigned xi = 0; xi < nodes[i]->range(); xi++) {
        piVals[xi] = LogAware::noEvidence();
        ldVals[xi] = LogAware::noEvidence();
      }
      piVals[nodes[i]->getEvidence()] = LogAware::withEvidence();
      ldVals[nodes[i]->getEvidence()] = LogAware::withEvidence();
    }
  }
}



void
BnBpSolver::runLoopySolver()
{ 
  nIters_ = 0;
  while (!converged() && nIters_ < BpOptions::maxIter) {

    nIters_++;
    if (Constants::DEBUG >= 2) {
      Util::printHeader ("Iteration " + nIters_);
      cout << endl;
    }

    switch (BpOptions::schedule) {

      case BpOptions::Schedule::SEQ_RANDOM:
        random_shuffle (links_.begin(), links_.end());
        // no break
  
      case BpOptions::Schedule::SEQ_FIXED:
        for (unsigned i = 0; i < links_.size(); i++) {
          calculateAndUpdateMessage (links_[i]);
          updateValues (links_[i]);
        }
        break;

      case BpOptions::Schedule::PARALLEL:
        for (unsigned i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (unsigned i = 0; i < links_.size(); i++) {
          updateMessage (links_[i]);
          updateValues (links_[i]);
        }
        break;
 
      case BpOptions::Schedule::MAX_RESIDUAL:
        maxResidualSchedule();
        break;

    }
    if (Constants::DEBUG >= 2) {
      cout << endl;
    }
  }
}



bool
BnBpSolver::converged (void) const
{
  // this can happen if the graph is fully disconnected
  if (links_.size() == 0) {
    return true;
  }
  if (nIters_ == 0 || nIters_ == 1) {
    return false;
  }
  bool converged = true;
  if (BpOptions::schedule == BpOptions::Schedule::MAX_RESIDUAL) {
    double maxResidual = (*(sortedOrder_.begin()))->getResidual();
    if (maxResidual < BpOptions::accuracy) {
      converged = true;
    } else {
      converged = false;
    }
  } else {
    for (unsigned i = 0; i < links_.size(); i++) {
      double residual = links_[i]->getResidual();
      if (Constants::DEBUG >= 2) {
        cout << links_[i]->toString() + " residual change = " ;
        cout << residual << endl;
      }
      if (residual > BpOptions::accuracy) {
        converged = false;
        break;
      }
    }
  }
  return converged;
}



void
BnBpSolver::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
    }
    return;
  }

  for (unsigned c = 0; c < sortedOrder_.size(); c++) {
    if (Constants::DEBUG >= 2) {
      cout << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }
 
    SortedOrder::iterator it = sortedOrder_.begin();
    BpLink* link = *it;
    if (link->getResidual() < BpOptions::accuracy) {
      sortedOrder_.erase (it);
      it = sortedOrder_.begin();
      return;
    }
    updateMessage (link);
    updateValues (link);
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    const BpLinkSet& outParentLinks =
        ninf(link->getDestination())->getOutcomingParentLinks();
    for (unsigned i = 0; i < outParentLinks.size(); i++) {
      if (outParentLinks[i]->getDestination() != link->getSource()
          && outParentLinks[i]->getDestination()->hasEvidence() == false) {
        calculateMessage (outParentLinks[i]);
        BpLinkMap::iterator iter = linkMap_.find (outParentLinks[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (outParentLinks[i]);
      }
    }
    const BpLinkSet& outChildLinks = 
        ninf(link->getDestination())->getOutcomingChildLinks();
    for (unsigned i = 0; i < outChildLinks.size(); i++) {
      if (outChildLinks[i]->getDestination() != link->getSource()) {
        calculateMessage (outChildLinks[i]);
        BpLinkMap::iterator iter = linkMap_.find (outChildLinks[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (outChildLinks[i]);
      }
    }

    if (Constants::DEBUG >= 2) {
      Util::printDashedLine();
    }
  }
}



void
BnBpSolver::updatePiValues (BayesNode* x)
{
  // π(Xi)
  if (Constants::DEBUG >= 3) {
    cout << "updating " << PI_SYMBOL << " values for " << x->label() << endl;
  }
  Params& piValues           = ninf(x)->getPiValues();
  const BpLinkSet& parentLinks = ninf(x)->getIncomingParentLinks();
  const BnNodeSet& ps = x->getParents();
  Ranges ranges;
  for (unsigned i = 0; i < ps.size(); i++) {
    ranges.push_back (ps[i]->range());
  }
  StatesIndexer indexer (ranges, false);
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  Params messageProducts (indexer.size());
  for (unsigned k = 0; k < indexer.size(); k++) {
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = LogAware::multIdenty();
    if (Globals::logDomain) {
      for (unsigned i = 0; i < parentLinks.size(); i++) {
        messageProduct += parentLinks[i]->getMessage()[indexer[i]];
      }
    } else {
      for (unsigned i = 0; i < parentLinks.size(); i++) {
        messageProduct *= parentLinks[i]->getMessage()[indexer[i]];
        if (Constants::DEBUG >= 5) {
          if (i != 0) *calcs1 << " + " ;
          if (i != 0) *calcs2 << " + " ;
          *calcs1 << parentLinks[i]->toString (indexer[i]);
          *calcs2 << parentLinks[i]->getMessage()[indexer[i]];
        }
      }
    }
    messageProducts[k] = messageProduct;
    if (Constants::DEBUG >= 5) {
      cout << "    mp" << k;
      cout << " = " << (*calcs1).str();
      if (parentLinks.size() == 1) {
        cout << " = " << messageProduct << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << messageProduct << endl;
      }
      delete calcs1;
      delete calcs2;
    }
    ++ indexer;
  }

  for (unsigned xi = 0; xi < x->range(); xi++) {
    double sum = LogAware::addIdenty();
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    indexer.reset();
    if (Globals::logDomain) {
      for (unsigned k = 0; k < indexer.size(); k++) {
        sum = Util::logSum (sum, 
                  x->getProbability(xi, indexer) + messageProducts[k]);
        ++ indexer;
      }
    } else {
      for (unsigned k = 0; k < indexer.size(); k++) {
        sum += x->getProbability (xi, indexer) * messageProducts[k];
        if (Constants::DEBUG >= 5) {
          if (k != 0) *calcs1 << " + " ;
          if (k != 0) *calcs2 << " + " ;
          *calcs1 << x->cptEntryToString (xi, indexer.indices()); 
          *calcs1 << ".mp" << k;
          *calcs2 << LogAware::fl (x->getProbability (xi, indexer));
          *calcs2 << "*" << messageProducts[k];
        }
        ++ indexer;
      }
    }

    piValues[xi] = sum;
    if (Constants::DEBUG >= 5) {
      cout << "    " << PI_SYMBOL << "(" << x->label() << ")" ;
      cout << "[" << x->states()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      cout << " = " << (*calcs2).str();
      cout << " = " << piValues[xi] << endl;
      delete calcs1;
      delete calcs2;
    }
  }
}



void
BnBpSolver::updateLambdaValues (BayesNode* x)
{
  // λ(Xi)
  if (Constants::DEBUG >= 3) {
    cout << "updating " << LD_SYMBOL << " values for " << x->label() << endl;
  }
  Params& lambdaValues       = ninf(x)->getLambdaValues();
  const BpLinkSet& childLinks  = ninf(x)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  for (unsigned xi = 0; xi < x->range(); xi++) {
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double product = LogAware::multIdenty();
    if (Globals::logDomain) {
      for (unsigned i = 0; i < childLinks.size(); i++) {
        product += childLinks[i]->getMessage()[xi];
      }
    } else {
      for (unsigned i = 0; i < childLinks.size(); i++) {
        product *= childLinks[i]->getMessage()[xi];
        if (Constants::DEBUG >= 5) {
          if (i != 0) *calcs1 << "." ;
          if (i != 0) *calcs2 << "*" ;
          *calcs1 << childLinks[i]->toString (xi);
          *calcs2 << childLinks[i]->getMessage()[xi];
        }
      }
    }
    lambdaValues[xi] = product;
    if (Constants::DEBUG >= 5) {
      cout << "    " << LD_SYMBOL << "(" << x->label() << ")" ;
      cout << "[" << x->states()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      if (childLinks.size() == 1) {
        cout << " = " << product << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << lambdaValues[xi] << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }
}



void
BnBpSolver::calculatePiMessage (BpLink* link)
{
  // πX(Zi)
  BayesNode* z = link->getSource();
  BayesNode* x = link->getDestination();
  Params& zxPiNextMessage = link->getNextMessage();
  const BpLinkSet& zChildLinks = ninf(z)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  const Params& zPiValues = ninf(z)->getPiValues();
  for (unsigned zi = 0; zi < z->range(); zi++) {
    double product = zPiValues[zi];
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
      *calcs1 << PI_SYMBOL << "(" << z->label() << ")";
      *calcs1 << "[" << z->states()[zi] << "]" ;
      *calcs2 << product;
    }
    if (Globals::logDomain) {
      for (unsigned i = 0; i < zChildLinks.size(); i++) {
        if (zChildLinks[i]->getSource() != x) {
          product += zChildLinks[i]->getMessage()[zi];
        }
      }
    } else {
      for (unsigned i = 0; i < zChildLinks.size(); i++) {
        if (zChildLinks[i]->getSource() != x) {
          product *= zChildLinks[i]->getMessage()[zi];
          if (Constants::DEBUG >= 5) {
            *calcs1 << "." << zChildLinks[i]->toString (zi);
            *calcs2 << " * " << zChildLinks[i]->getMessage()[zi];
          }
        }
      }
    }
    zxPiNextMessage[zi] = product;
    if (Constants::DEBUG >= 5) {
      cout << "    " << link->toString();
      cout << "["  << z->states()[zi] << "]" ;
      cout << " = " << (*calcs1).str();
      if (zChildLinks.size() == 1) {
        cout << " = " << product << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << product << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }
  LogAware::normalize (zxPiNextMessage);
}



void
BnBpSolver::calculateLambdaMessage (BpLink* link)
{
  // λY(Xi)
  BayesNode* y = link->getSource();
  BayesNode* x = link->getDestination();
  if (x->hasEvidence()) {
    return;
  }
  Params& yxLambdaNextMessage   = link->getNextMessage();
  const BpLinkSet& yParentLinks = ninf(y)->getIncomingParentLinks();
  const Params& yLambdaValues   = ninf(y)->getLambdaValues();
  int parentIndex               = y->indexOfParent (x);
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  const BnNodeSet& ps = y->getParents();
  Ranges ranges;
  for (unsigned i = 0; i < ps.size(); i++) {
    ranges.push_back (ps[i]->range());
  }
  StatesIndexer indexer (ranges, false);

 
  unsigned N = indexer.size() / x->range();
  Params messageProducts (N);
  for (unsigned k = 0; k < N; k++) {
    while (indexer[parentIndex] != 0) {
      ++ indexer;
    }
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = LogAware::multIdenty();
    if (Globals::logDomain) {
      for (unsigned i = 0; i < yParentLinks.size(); i++) {
        if (yParentLinks[i]->getSource() != x) {
          messageProduct += yParentLinks[i]->getMessage()[indexer[i]];
        }
      }
    } else {
      for (unsigned i = 0; i < yParentLinks.size(); i++) {
        if (yParentLinks[i]->getSource() != x) {
          if (Constants::DEBUG >= 5) {
            if (messageProduct != LogAware::multIdenty()) *calcs1 << "*" ;
            if (messageProduct != LogAware::multIdenty()) *calcs2 << "*" ;
            *calcs1 << yParentLinks[i]->toString (indexer[i]);
            *calcs2 << yParentLinks[i]->getMessage()[indexer[i]];
          }
          messageProduct *= yParentLinks[i]->getMessage()[indexer[i]];
        }
      }
    }
    messageProducts[k] = messageProduct;
    ++ indexer;
    if (Constants::DEBUG >= 5) {
      cout << "    mp" << k;
      cout << " = " << (*calcs1).str();
      if  (yParentLinks.size() == 1) {
        cout << 1 << endl;
      } else if (yParentLinks.size() == 2) {
        cout << " = " << messageProduct << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << messageProduct << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }

  for (unsigned xi = 0; xi < x->range(); xi++) {
    if (Constants::DEBUG >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double outerSum = LogAware::addIdenty();
    for (unsigned yi = 0; yi < y->range(); yi++) {
      if (Constants::DEBUG >= 5) {
        (yi != 0) ? *calcs1 << " + {" : *calcs1 << "{" ;
        (yi != 0) ? *calcs2 << " + {" : *calcs2 << "{" ;
      } 
      double innerSum = LogAware::addIdenty();
      indexer.reset();
      if (Globals::logDomain) {
        for (unsigned k = 0; k < N; k++) {
          while (indexer[parentIndex] != xi) {
            ++ indexer;
          }
          innerSum = Util::logSum (innerSum,
              y->getProbability (yi, indexer) + messageProducts[k]);
          ++ indexer;
        }
        outerSum = Util::logSum (outerSum, innerSum + yLambdaValues[yi]);
      } else {
        for (unsigned k = 0; k < N; k++) {
         while (indexer[parentIndex] != xi) {
           ++ indexer;
         }
         if (Constants::DEBUG >= 5) {
           if (k != 0) *calcs1 << " + " ;
           if (k != 0) *calcs2 << " + " ;
           *calcs1 << y->cptEntryToString (yi, indexer.indices());
           *calcs1 << ".mp" << k;
           *calcs2 << y->getProbability (yi, indexer);
           *calcs2 << "*" << messageProducts[k];
         }
         innerSum += y->getProbability (yi, indexer) * messageProducts[k];
          ++ indexer;
        }
        outerSum += innerSum * yLambdaValues[yi];
      }
      if (Constants::DEBUG >= 5) {
        *calcs1 << "}." << LD_SYMBOL << "(" << y->label() << ")" ;
        *calcs1 << "["  << y->states()[yi] << "]";
        *calcs2 << "}*" << yLambdaValues[yi];
      }
    }
    yxLambdaNextMessage[xi] = outerSum;
    if (Constants::DEBUG >= 5) {
      cout << "    " << link->toString();
      cout << "[" << x->states()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      cout << " = " << (*calcs2).str();
      cout << " = " << yxLambdaNextMessage[xi] << endl;
      delete calcs1;
      delete calcs2;
    }
  }
  LogAware::normalize (yxLambdaNextMessage);
}



Params
BnBpSolver::getJointByConditioning (const VarIds& jointVarIds) const
{
/*
  BnNodeSet jointVars;
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    assert (bayesNet_->getBayesNode (jointVarIds[i]));
    jointVars.push_back (bayesNet_->getBayesNode (jointVarIds[i]));
  }

  BayesNet* mrn = bayesNet_->getMinimalRequesiteNetwork (jointVarIds[0]);
  BnBpSolver solver (*mrn);
  solver.runSolver();
  Params prevBeliefs = solver.getPosterioriOf (jointVarIds[0]);
  delete mrn;

  VarIds observedVids = {jointVars[0]->varId()};

  for (unsigned i = 1; i < jointVarIds.size(); i++) {
    assert (jointVars[i]->hasEvidence() == false);
    VarIds reqVars = {jointVarIds[i]};
    Util::addToVector (reqVars, observedVids);
    mrn = bayesNet_->getMinimalRequesiteNetwork (reqVars);
    Params newBeliefs;
    VarNodes observedVars;
    for (unsigned j = 0; j < observedVids.size(); j++) {
      observedVars.push_back (mrn->getBayesNode (observedVids[j]));
    }
    StatesIndexer idx (observedVars, false);
    while (idx.valid()) {
      for (unsigned j = 0; j < observedVars.size(); j++) {
        observedVars[j]->setEvidence (idx[j]);
      }
      BnBpSolver solver (*mrn);
      solver.runSolver();
      Params beliefs = solver.getPosterioriOf (jointVarIds[i]);
      for (unsigned k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
      ++ idx;
    }

    int count = -1;
    for (unsigned j = 0; j < newBeliefs.size(); j++) {
      if (j % jointVars[i]->range() == 0) {
        count ++;
      }
      newBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = newBeliefs;
    observedVids.push_back (jointVars[i]->varId());
    delete mrn;
  }
  return prevBeliefs;
*/
  return Params();
}



void
BnBpSolver::printPiLambdaValues (const BayesNode* var) const
{
  cout << left;
  cout << setw (10) << "states" ;
  cout << setw (20) << PI_SYMBOL << "(" + var->label() + ")" ;
  cout << setw (20) << LD_SYMBOL << "(" + var->label() + ")" ;
  cout << setw (16) << "belief" ;
  cout << endl;
  Util::printDashedLine();
  cout << endl;
  const States&    states   = var->states();
  const Params&  piVals   = ninf(var)->getPiValues();
  const Params&  ldVals   = ninf(var)->getLambdaValues();
  const Params&  beliefs  = ninf(var)->getBeliefs();
  for (unsigned xi = 0; xi < var->range(); xi++) {
    cout << setw (10) << states[xi];
    cout << setw (19) << piVals[xi];
    cout << setw (19) << ldVals[xi];
    cout.precision (Constants::PRECISION);
    cout << setw (16) << beliefs[xi];
    cout << endl;
  }
  cout << endl;
}



void 
BnBpSolver::printAllMessageStatus (void) const
{ 
  const BnNodeSet& nodes = bayesNet_->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    printPiLambdaValues (nodes[i]);
  }
}



BpNodeInfo::BpNodeInfo (BayesNode* node)
{
  node_ = node;
  piVals_.resize (node->range(), LogAware::one());
  ldVals_.resize (node->range(), LogAware::one());
}



Params
BpNodeInfo::getBeliefs (void) const
{
  double sum = 0.0;
  Params beliefs (node_->range());
  if (Globals::logDomain) {
    for (unsigned xi = 0; xi < node_->range(); xi++) {
      beliefs[xi] = exp (piVals_[xi] + ldVals_[xi]);
      sum += beliefs[xi];
    }
  } else {
    for (unsigned xi = 0; xi < node_->range(); xi++) {
      beliefs[xi] = piVals_[xi] * ldVals_[xi];
      sum += beliefs[xi];
    } 
  }
  assert (sum);
  for (unsigned xi = 0; xi < node_->range(); xi++) {
    beliefs[xi] /= sum;
  }
  return beliefs;
}



bool
BpNodeInfo::receivedBottomInfluence (void) const
{
  // if all lambda values are equal, then neither
  // this node neither its descendents have evidence,
  // we can use this to don't send lambda messages his parents
  bool childInfluenced = false;
  for (unsigned xi = 1; xi < node_->range(); xi++) {
    if (ldVals_[xi] != ldVals_[0]) {
      childInfluenced = true;
      break;
    }
  }
  return childInfluenced;
}

