#include <cstdlib>
#include <limits>
#include <time.h>

#include <algorithm>

#include <iostream>
#include <sstream>
#include <iomanip>

#include "BnBpSolver.h"

BnBpSolver::BnBpSolver (const BayesNet& bn) : Solver (&bn)
{ 
  bayesNet_ = &bn;
  jointCalcType_  = CHAIN_RULE;
  //jointCalcType_  = JUNCTION_NODE;
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
  if (COLLECT_STATISTICS) {
    start = clock();
  }
  initializeSolver();
  if (!BpOptions::useAlwaysLoopySolver && bayesNet_->isPolyTree()) {
    runPolyTreeSolver();
  } else {
    runLoopySolver();
    if (DL >= 2) {
      cout << endl;
      if (nIters_ < BpOptions::maxIter) {
        cout << "Belief propagation converged in " ; 
        cout << nIters_ << " iterations" << endl;
      } else {
        cout << "The maximum number of iterations was hit, terminating..." ;
        cout << endl;
      }
    }
  }
  unsigned size = bayesNet_->nrNodes();
  if (COLLECT_STATISTICS) {
    unsigned nIters = 0;
    bool loopy = bayesNet_->isPolyTree() == false;
    if (loopy) nIters = nIters_;
    double time = (double (clock() - start)) / CLOCKS_PER_SEC;
    Statistics::updateStatistics (size, loopy, nIters, time);
  }
  if (EXPORT_TO_GRAPHVIZ && size > EXPORT_MINIMAL_SIZE) {
    stringstream ss;
    ss << Statistics::getSolvedNetworksCounting() << "." << size << ".dot" ;
    bayesNet_->exportToGraphViz (ss.str().c_str());
  }
}



ParamSet
BnBpSolver::getPosterioriOf (VarId vid)
{
  BayesNode* node = bayesNet_->getBayesNode (vid);
  assert (node);
  return nodesI_[node->getIndex()]->getBeliefs();
}



ParamSet
BnBpSolver::getJointDistributionOf (const VarIdSet& jointVarIds)
{
  if (DL >= 2) {
    cout << "calculating joint distribution on: " ;
    for (unsigned i = 0; i < jointVarIds.size(); i++) {
      VarNode* var = bayesNet_->getBayesNode (jointVarIds[i]);
      cout << var->label() << " " ;
    }
    cout << endl;
  }

  if (jointCalcType_ == JUNCTION_NODE) {
    return getJointByJunctionNode (jointVarIds);
  } else {
    return getJointByChainRule (jointVarIds);
  }
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
    const ParamSet& params = roots[i]->getParameters();
    ParamSet& piVals = ninf(roots[i])->getPiValues();
    for (unsigned ri = 0; ri < roots[i]->nrStates(); ri++) {
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
      ParamSet& piVals = ninf(nodes[i])->getPiValues();
      ParamSet& ldVals = ninf(nodes[i])->getLambdaValues();
      for (unsigned xi = 0; xi < nodes[i]->nrStates(); xi++) {
        piVals[xi] = Util::noEvidence();
        ldVals[xi] = Util::noEvidence();
      }
      piVals[nodes[i]->getEvidence()] = Util::withEvidence();
      ldVals[nodes[i]->getEvidence()] = Util::withEvidence();
    }
  }
}



void
BnBpSolver::runPolyTreeSolver (void)
{
  const BnNodeSet& nodes = bayesNet_->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->isRoot()) {
      ninf(nodes[i])->markPiValuesAsCalculated();
    }
    if (nodes[i]->isLeaf()) {
      ninf(nodes[i])->markLambdaValuesAsCalculated();
    }
  }

  bool finish = false;
  while (!finish) {
    finish = true;
    for (unsigned i = 0; i < nodes.size(); i++) {
      if (ninf(nodes[i])->piValuesCalculated() == false
          && ninf(nodes[i])->receivedAllPiMessages()) {
        if (!nodes[i]->hasEvidence()) {
          updatePiValues (nodes[i]);
        }
        ninf(nodes[i])->markPiValuesAsCalculated();
        finish = false;
      }

      if (ninf(nodes[i])->lambdaValuesCalculated() == false
          && ninf(nodes[i])->receivedAllLambdaMessages()) {
        if (!nodes[i]->hasEvidence()) {
          updateLambdaValues (nodes[i]);
        }
        ninf(nodes[i])->markLambdaValuesAsCalculated();
        finish = false;
      }

      if (ninf(nodes[i])->piValuesCalculated()) {
        const BpLinkSet& outChildLinks
            = ninf(nodes[i])->getOutcomingChildLinks();
        for (unsigned j = 0; j < outChildLinks.size(); j++) {
          BayesNode* child = outChildLinks[j]->getDestination();
          if (!outChildLinks[j]->messageWasSended()) {
            if (ninf(nodes[i])->readyToSendPiMsgTo (child)) {
              calculateAndUpdateMessage (outChildLinks[j], false);
              ninf(child)->incNumPiMsgsReceived();
            }
            finish = false;
          }
        }
      }

      if (ninf(nodes[i])->lambdaValuesCalculated()) {
        const BpLinkSet& outParentLinks = 
            ninf(nodes[i])->getOutcomingParentLinks();
        for (unsigned j = 0; j < outParentLinks.size(); j++) {
          BayesNode* parent = outParentLinks[j]->getDestination();
          if (!outParentLinks[j]->messageWasSended()) {
            if (ninf(nodes[i])->readyToSendLambdaMsgTo (parent)) {
              calculateAndUpdateMessage (outParentLinks[j], false);
              ninf(parent)->incNumLambdaMsgsReceived();
            }
            finish = false;
          }
        }
      }
    }
  }
}



void
BnBpSolver::runLoopySolver()
{ 
  nIters_ = 0;
  while (!converged() && nIters_ < BpOptions::maxIter) {

    nIters_++;
    if (DL >= 2) {
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
      cout << " Iteration " << nIters_ << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
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
    if (DL >= 2) {
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
    Param maxResidual = (*(sortedOrder_.begin()))->getResidual();
    if (maxResidual < BpOptions::accuracy) {
      converged = true;
    } else {
      converged = false;
    }
  } else {
    for (unsigned i = 0; i < links_.size(); i++) {
      Param residual = links_[i]->getResidual();
      if (DL >= 2) {
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
    if (DL >= 2) {
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

    if (DL >= 2) {
      cout << "----------------------------------------" ;
      cout << "----------------------------------------" << endl;
    }
  }
}



void
BnBpSolver::updatePiValues (BayesNode* x)
{
  // π(Xi)
  if (DL >= 3) {
    cout << "updating " << PI_SYMBOL << " values for " << x->label() << endl;
  }
  ParamSet& piValues               = ninf(x)->getPiValues();
  const BpLinkSet& parentLinks     = ninf(x)->getIncomingParentLinks();
  const vector<CptEntry>& entries  = x->getCptEntries();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  ParamSet messageProducts (entries.size());
  for (unsigned k = 0; k < entries.size(); k++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = Util::multIdenty();
    const DConf& conf = entries[k].getDomainConfiguration();
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        for (unsigned i = 0; i < parentLinks.size(); i++) {
          messageProduct *= parentLinks[i]->getMessage()[conf[i]];
          if (DL >= 5) {
            if (i != 0) *calcs1 << " + " ;
            if (i != 0) *calcs2 << " + " ;
            *calcs1 << parentLinks[i]->toString (conf[i]);
            *calcs2 << parentLinks[i]->getMessage()[conf[i]];
          }
        }
        break;
      case NumberSpace::LOGARITHM:
        for (unsigned i = 0; i < parentLinks.size(); i++) {
          messageProduct += parentLinks[i]->getMessage()[conf[i]];
        }
    }
    messageProducts[k] = messageProduct;
    if (DL >= 5) {
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
  }

  for (unsigned xi = 0; xi < x->nrStates(); xi++) {
    double sum = Util::addIdenty();
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        for (unsigned k = 0; k < entries.size(); k++) {
          sum += x->getProbability (xi, entries[k]) * messageProducts[k];
          if (DL >= 5) {
            if (k != 0) *calcs1 << " + " ;
            if (k != 0) *calcs2 << " + " ;
            *calcs1 << x->cptEntryToString (xi, entries[k]); 
            *calcs1 << ".mp" << k;
            *calcs2 << Util::fl (x->getProbability (xi, entries[k]));
            *calcs2 << "*" << messageProducts[k];
          }
        }
        break;
      case NumberSpace::LOGARITHM:
        for (unsigned k = 0; k < entries.size(); k++) {
          Util::logSum (sum,
              x->getProbability(xi,entries[k]) + messageProducts[k]);
        }
    }
    piValues[xi] = sum;
    if (DL >= 5) {
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
  if (DL >= 3) {
    cout << "updating " << LD_SYMBOL << " values for " << x->label() << endl;
  }
  ParamSet& lambdaValues       = ninf(x)->getLambdaValues();
  const BpLinkSet& childLinks  = ninf(x)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  for (unsigned xi = 0; xi < x->nrStates(); xi++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double product = Util::multIdenty();
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        for (unsigned i = 0; i < childLinks.size(); i++) {
          product *= childLinks[i]->getMessage()[xi];
          if (DL >= 5) {
            if (i != 0) *calcs1 << "." ;
            if (i != 0) *calcs2 << "*" ;
            *calcs1 << childLinks[i]->toString (xi);
            *calcs2 << childLinks[i]->getMessage()[xi];
          }
        }
        break;
      case NumberSpace::LOGARITHM:
        for (unsigned i = 0; i < childLinks.size(); i++) {
          product += childLinks[i]->getMessage()[xi];
        }
    }
    lambdaValues[xi] = product;
    if (DL >= 5) {
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
  ParamSet& zxPiNextMessage = link->getNextMessage();
  const BpLinkSet& zChildLinks = ninf(z)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  const ParamSet& zPiValues = ninf(z)->getPiValues();
  for (unsigned zi = 0; zi < z->nrStates(); zi++) {
    double product = zPiValues[zi];
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
      *calcs1 << PI_SYMBOL << "(" << z->label() << ")";
      *calcs1 << "[" << z->states()[zi] << "]" ;
      *calcs2 << product;
    }
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        for (unsigned i = 0; i < zChildLinks.size(); i++) {
          if (zChildLinks[i]->getSource() != x) {
            product *= zChildLinks[i]->getMessage()[zi];
            if (DL >= 5) {
              *calcs1 << "." << zChildLinks[i]->toString (zi);
              *calcs2 << " * " << zChildLinks[i]->getMessage()[zi];
            }
          }
        }
        break;
      case NumberSpace::LOGARITHM:
        for (unsigned i = 0; i < zChildLinks.size(); i++) {
          if (zChildLinks[i]->getSource() != x) {
            product += zChildLinks[i]->getMessage()[zi];
          }
        }
    }
    zxPiNextMessage[zi] = product;
    if (DL >= 5) {
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
  Util::normalize (zxPiNextMessage);
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
  ParamSet& yxLambdaNextMessage       = link->getNextMessage();
  const BpLinkSet& yParentLinks       = ninf(y)->getIncomingParentLinks();
  const ParamSet& yLambdaValues       = ninf(y)->getLambdaValues();
  const vector<CptEntry>& allEntries  = y->getCptEntries();
  int parentIndex                     = y->getIndexOfParent (x);
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;
 
  vector<CptEntry> entries;
  DConstraint constr = make_pair (parentIndex, 0);
  for (unsigned i = 0; i < allEntries.size(); i++) {
    if (allEntries[i].matchConstraints(constr)) {
      entries.push_back (allEntries[i]);
    }
  }

  ParamSet messageProducts (entries.size());
  for (unsigned k = 0; k < entries.size(); k++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = Util::multIdenty();
    const DConf& conf = entries[k].getDomainConfiguration();
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        for (unsigned i = 0; i < yParentLinks.size(); i++) {
          if (yParentLinks[i]->getSource() != x) {
            if (DL >= 5) {
              if (messageProduct != Util::multIdenty()) *calcs1 << "*" ;
              if (messageProduct != Util::multIdenty()) *calcs2 << "*" ;
              *calcs1 << yParentLinks[i]->toString (conf[i]);
              *calcs2 << yParentLinks[i]->getMessage()[conf[i]];
            }
            messageProduct *= yParentLinks[i]->getMessage()[conf[i]];
          }
        }
        break;
     case NumberSpace::LOGARITHM:    
       for (unsigned i = 0; i < yParentLinks.size(); i++) {
         if (yParentLinks[i]->getSource() != x) {
           messageProduct += yParentLinks[i]->getMessage()[conf[i]];
         }
       }
    }
    messageProducts[k] = messageProduct;
    if (DL >= 5) {
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

  for (unsigned xi = 0; xi < x->nrStates(); xi++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    vector<CptEntry> entries;
    DConstraint constr = make_pair (parentIndex, xi);
    for (unsigned i = 0; i < allEntries.size(); i++) {
      if (allEntries[i].matchConstraints(constr)) {
        entries.push_back (allEntries[i]);
      }
    }
    double outerSum = Util::addIdenty();
    for (unsigned yi = 0; yi < y->nrStates(); yi++) {
      if (DL >= 5) {
        (yi != 0) ? *calcs1 << " + {" : *calcs1 << "{" ;
        (yi != 0) ? *calcs2 << " + {" : *calcs2 << "{" ;
      } 
      double innerSum = Util::addIdenty();
      switch (NSPACE) {
        case NumberSpace::NORMAL:
          for (unsigned k = 0; k < entries.size(); k++) {
            if (DL >= 5) {
              if (k != 0) *calcs1 << " + " ;
              if (k != 0) *calcs2 << " + " ;
              *calcs1 << y->cptEntryToString (yi, entries[k]);
              *calcs1 << ".mp" << k;
              *calcs2 << y->getProbability (yi, entries[k]);
              *calcs2 << "*" << messageProducts[k];
            }
            innerSum += y->getProbability (yi, entries[k]) * messageProducts[k];
          }
          outerSum += innerSum * yLambdaValues[yi];
          break;
        case NumberSpace::LOGARITHM:
          for (unsigned k = 0; k < entries.size(); k++) {
            Util::logSum (innerSum,
                y->getProbability(yi, entries[k]) + messageProducts[k]);
          }
          Util::logSum (outerSum, innerSum + yLambdaValues[yi]);
      }
      if (DL >= 5) {
        *calcs1 << "}." << LD_SYMBOL << "(" << y->label() << ")" ;
        *calcs1 << "["  << y->states()[yi] << "]";
        *calcs2 << "}*" << yLambdaValues[yi];
      }
    }
    yxLambdaNextMessage[xi] = outerSum;
    if (DL >= 5) {
      cout << "    " << link->toString();
      cout << "[" << x->states()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      cout << " = " << (*calcs2).str();
      cout << " = " << yxLambdaNextMessage[xi] << endl;
      delete calcs1;
      delete calcs2;
    }
  }
  Util::normalize (yxLambdaNextMessage);
}



ParamSet
BnBpSolver::getJointByJunctionNode (const VarIdSet& jointVarIds)
{
  unsigned msgSize = 1;
  vector<unsigned> dsizes (jointVarIds.size());
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    dsizes[i] = bayesNet_->getBayesNode (jointVarIds[i])->nrStates(); 
    msgSize *=  dsizes[i];
  }
  unsigned reps = 1;
  ParamSet jointDist (msgSize, Util::multIdenty());
  for (int i = jointVarIds.size() - 1 ; i >= 0; i--) {
    Util::multiply (jointDist, getPosterioriOf (jointVarIds[i]), reps);
    reps *= dsizes[i] ;
  }
  return jointDist;
}



ParamSet
BnBpSolver::getJointByChainRule (const VarIdSet& jointVarIds) const
{
  BnNodeSet jointVars;
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    jointVars.push_back (bayesNet_->getBayesNode (jointVarIds[i]));
  }

  BayesNet* mrn = bayesNet_->getMinimalRequesiteNetwork (jointVarIds[0]);
  BnBpSolver solver (*mrn);
  solver.runSolver();
  ParamSet prevBeliefs = solver.getPosterioriOf (jointVarIds[0]);
  delete mrn;

  VarNodes observedVars = {jointVars[0]};

  for (unsigned i = 1; i < jointVarIds.size(); i++) {
    mrn = bayesNet_->getMinimalRequesiteNetwork (jointVarIds[i]);
    ParamSet newBeliefs;
    vector<DConf> confs = 
        Util::getDomainConfigurations (observedVars);
    for (unsigned j = 0; j < confs.size(); j++) {
      for (unsigned k = 0; k < observedVars.size(); k++) {
        if (!observedVars[k]->hasEvidence()) {
          BayesNode* node = mrn->getBayesNode (observedVars[k]->varId());
          if (node) {
            node->setEvidence (confs[j][k]);
          }
        }
      }
      BnBpSolver solver (*mrn);
      solver.runSolver();
      ParamSet beliefs = solver.getPosterioriOf (jointVarIds[i]);
      for (unsigned k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
    }

    int count = -1;
    for (unsigned j = 0; j < newBeliefs.size(); j++) {
      if (j % jointVars[i]->nrStates() == 0) {
        count ++;
      }
      newBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = newBeliefs;
    observedVars.push_back (jointVars[i]);
    delete mrn;
  }
  return prevBeliefs;
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
  cout << "--------------------------------" ;
  cout << "--------------------------------" ;
  cout << endl;
  const States&    states   = var->states();
  const ParamSet&  piVals   = ninf(var)->getPiValues();
  const ParamSet&  ldVals   = ninf(var)->getLambdaValues();
  const ParamSet&  beliefs  = ninf(var)->getBeliefs();
  for (unsigned xi = 0; xi < var->nrStates(); xi++) {
    cout << setw (10) << states[xi];
    cout << setw (19) << piVals[xi];
    cout << setw (19) << ldVals[xi];
    cout.precision (PRECISION);
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
  piValsCalc_ = false;
  ldValsCalc_ = false;
  nPiMsgsRcv_ = 0;
  nLdMsgsRcv_ = 0;
  piVals_.resize (node->nrStates(), Util::one());
  ldVals_.resize (node->nrStates(), Util::one());
}



ParamSet
BpNodeInfo::getBeliefs (void) const
{
  double sum = 0.0;
  ParamSet beliefs (node_->nrStates());
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned xi = 0; xi < node_->nrStates(); xi++) {
        beliefs[xi] = piVals_[xi] * ldVals_[xi];
       sum += beliefs[xi];
      } 
      break;
    case NumberSpace::LOGARITHM:
      for (unsigned xi = 0; xi < node_->nrStates(); xi++) {
        beliefs[xi] = exp (piVals_[xi] + ldVals_[xi]);
        sum += beliefs[xi];
      }
  }
  assert (sum);
  for (unsigned xi = 0; xi < node_->nrStates(); xi++) {
    beliefs[xi] /= sum;
  }
  return beliefs;
}



void 
BpNodeInfo::markPiValuesAsCalculated (void)
{
  piValsCalc_ = true;
}



void
BpNodeInfo::markLambdaValuesAsCalculated (void)
{
  ldValsCalc_ = true;
}



bool
BpNodeInfo::receivedAllPiMessages (void)
{
  return node_->getParents().size() == nPiMsgsRcv_;
}



bool
BpNodeInfo::receivedAllLambdaMessages (void)
{
  return node_->getChilds().size() == nLdMsgsRcv_;
}



bool
BpNodeInfo::readyToSendPiMsgTo (const BayesNode* child) const
{
  for (unsigned i = 0; i < inChildLinks_.size(); i++) {
    if (inChildLinks_[i]->getSource() != child
        && inChildLinks_[i]->messageWasSended() == false)  {
      return false;
    }
  }
  return true;
}



bool
BpNodeInfo::readyToSendLambdaMsgTo (const BayesNode* parent) const
{
  for (unsigned i = 0; i < inParentLinks_.size(); i++) {
    if (inParentLinks_[i]->getSource() != parent
        && inParentLinks_[i]->messageWasSended() == false)  {
      return false;
    }
  }
  return true;
}



bool
BpNodeInfo::receivedBottomInfluence (void) const
{
  // if all lambda values are equal, then neither
  // this node neither its descendents have evidence,
  // we can use this to don't send lambda messages his parents
  bool childInfluenced = false;
  for (unsigned xi = 1; xi < node_->nrStates(); xi++) {
    if (ldVals_[xi] != ldVals_[0]) {
      childInfluenced = true;
      break;
    }
  }
  return childInfluenced;
}

