#include <cstdlib>
#include <limits>
#include <time.h>

#include <iostream>
#include <sstream>
#include <iomanip>

#include "BPSolver.h"

BPSolver::BPSolver (const BayesNet& bn) : Solver (&bn)
{ 
  bn_ = &bn;
  useAlwaysLoopySolver_ = false;
  //jointCalcType_      = CHAIN_RULE;
  jointCalcType_      = JUNCTION_NODE;
}



BPSolver::~BPSolver (void)
{
  for (unsigned i = 0; i < nodesI_.size(); i++) {
    delete nodesI_[i];
  }
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



void
BPSolver::runSolver (void)
{
  clock_t start_ = clock();
  unsigned size = bn_->getNumberOfNodes();
  unsigned nIters = 0;
  initializeSolver();
  if (bn_->isSingleConnected() && !useAlwaysLoopySolver_) {
    runPolyTreeSolver();
    Statistics::numSolvedPolyTrees ++;
  } else {
    runLoopySolver();
    Statistics::numSolvedLoopyNets ++;
    if (nIter_ >= SolverOptions::maxIter) {
      Statistics::numUnconvergedRuns ++;
    } else {
      nIters = nIter_;
    }
    if (DL >= 2) {
      cout << endl;
      if (nIter_ < SolverOptions::maxIter) {
        cout << "Belief propagation converged in " ; 
        cout << nIter_ << " iterations" << endl;
      } else {
        cout << "The maximum number of iterations was hit, terminating..." ;
        cout << endl;
      }
    }
  }
  double time = (double (clock() - start_)) / CLOCKS_PER_SEC;
  Statistics::updateStats (size, nIters, time);
  if (EXPORT_TO_DOT && size > EXPORT_MIN_SIZE) {
    stringstream ss;
    ss << size << "." ;
    ss << Statistics::getCounting (size) << ".dot" ;
    bn_->exportToDotFormat (ss.str().c_str());
  }
}



ParamSet
BPSolver::getPosterioriOf (Vid vid) const
{
  BayesNode* node = bn_->getBayesNode (vid);
  assert (node);
  return nodesI_[node->getIndex()]->getBeliefs();
}



ParamSet
BPSolver::getJointDistributionOf (const VidSet& jointVids)
{
  if (DL >= 2) {
    cout << "calculating joint distribution on: " ;
    for (unsigned i = 0; i < jointVids.size(); i++) {
      Variable* var = bn_->getBayesNode (jointVids[i]);
      cout << var->getLabel() << " " ;
    }
    cout << endl;
  }

  if (jointCalcType_ == JUNCTION_NODE) {
    return getJointByJunctionNode (jointVids);
  } else {
    return getJointByChainRule (jointVids);
  }
}



void 
BPSolver::initializeSolver (void)
{
  if (DL >= 2) {
    if (!useAlwaysLoopySolver_) {
      cout << "-> solver type     = polytree solver" << endl;
      cout << "-> schedule        = n/a";
    } else {
      cout << "-> solver          = loopy solver" << endl;
      cout << "-> schedule        = ";
      switch (SolverOptions::schedule) {
        case SolverOptions::S_SEQ_FIXED:    cout << "sequential fixed" ;  break;
        case SolverOptions::S_SEQ_RANDOM:   cout << "sequential random" ; break;
        case SolverOptions::S_PARALLEL:     cout << "parallel" ;          break;
        case SolverOptions::S_MAX_RESIDUAL: cout << "max residual" ;      break;
      }
    }
    cout << endl;
    cout << "-> joint method    = " ;
    if (jointCalcType_ == JUNCTION_NODE) {
      cout << "junction node" << endl;
    } else {
      cout << "chain rule " << endl;
    }
    cout << "-> max iters       = " << SolverOptions::maxIter << endl;
    cout << "-> accuracy        = " << SolverOptions::accuracy << endl;
    cout << endl;
  }

  CBnNodeSet nodes = bn_->getBayesNodes();
  for (unsigned i = 0; i < nodesI_.size(); i++) {
    delete nodesI_[i];
  }
  nodesI_.clear();
  nodesI_.reserve (nodes.size());
  links_.clear();
  sortedOrder_.clear();
  edgeMap_.clear();

  for (unsigned i = 0; i < nodes.size(); i++) {
    nodesI_.push_back (new BPNodeInfo (nodes[i]));
  }

  BnNodeSet roots = bn_->getRootNodes();
  for (unsigned i = 0; i < roots.size(); i++) {
    const ParamSet& params = roots[i]->getParameters();
    ParamSet& piVals = M(roots[i])->getPiValues();
    for (unsigned ri = 0; ri < roots[i]->getDomainSize(); ri++) {
      piVals[ri] = params[ri];
    }
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    CBnNodeSet parents = nodes[i]->getParents();
    for (unsigned j = 0; j < parents.size(); j++) {
      Edge* newLink = new Edge (parents[j], nodes[i], PI_MSG);
      links_.push_back (newLink);
      M(nodes[i])->addIncomingParentLink (newLink);
      M(parents[j])->addOutcomingChildLink (newLink);
    }
    CBnNodeSet childs = nodes[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      Edge* newLink = new Edge (childs[j], nodes[i], LAMBDA_MSG);
      links_.push_back (newLink);
      M(nodes[i])->addIncomingChildLink (newLink);
      M(childs[j])->addOutcomingParentLink (newLink);
    }
  }

  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->hasEvidence()) {
      ParamSet& piVals = M(nodes[i])->getPiValues();
      ParamSet& ldVals = M(nodes[i])->getLambdaValues();
      for (unsigned xi = 0; xi < nodes[i]->getDomainSize(); xi++) {
        piVals[xi] = 0.0;
        ldVals[xi] = 0.0;
      }
      piVals[nodes[i]->getEvidence()] = 1.0;
      ldVals[nodes[i]->getEvidence()] = 1.0;
    }
  }
}



void
BPSolver::runPolyTreeSolver (void)
{
  CBnNodeSet nodes = bn_->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->isRoot()) {
      M(nodes[i])->markPiValuesAsCalculated();
    }
    if (nodes[i]->isLeaf()) {
      M(nodes[i])->markLambdaValuesAsCalculated();
    }
  }

  bool finish = false;
  while (!finish) {
    finish = true;
    for (unsigned i = 0; i < nodes.size(); i++) {
      if (M(nodes[i])->arePiValuesCalculated() == false
          && M(nodes[i])->receivedAllPiMessages()) {
        if (!nodes[i]->hasEvidence()) {
          updatePiValues (nodes[i]);
        }
        M(nodes[i])->markPiValuesAsCalculated();
        finish = false;
      }

      if (M(nodes[i])->areLambdaValuesCalculated() == false
          && M(nodes[i])->receivedAllLambdaMessages()) {
        if (!nodes[i]->hasEvidence()) {
          updateLambdaValues (nodes[i]);
        }
        M(nodes[i])->markLambdaValuesAsCalculated();
        finish = false;
      }

      if (M(nodes[i])->arePiValuesCalculated()) {
        CEdgeSet outChildLinks = M(nodes[i])->getOutcomingChildLinks();
        for (unsigned j = 0; j < outChildLinks.size(); j++) {
          BayesNode* child = outChildLinks[j]->getDestination();
          if (!outChildLinks[j]->messageWasSended()) {
            if (M(nodes[i])->readyToSendPiMsgTo (child)) {
              outChildLinks[j]->setNextMessage (getMessage (outChildLinks[j]));
              outChildLinks[j]->updateMessage();
              M(child)->incNumPiMsgsRcv();
            }
            finish = false;
          }
        }
      }

      if (M(nodes[i])->areLambdaValuesCalculated()) {
        CEdgeSet outParentLinks = M(nodes[i])->getOutcomingParentLinks();
        for (unsigned j = 0; j < outParentLinks.size(); j++) {
          BayesNode* parent = outParentLinks[j]->getDestination();
          if (!outParentLinks[j]->messageWasSended()) {
            if (M(nodes[i])->readyToSendLambdaMsgTo (parent)) {
              outParentLinks[j]->setNextMessage (getMessage (outParentLinks[j]));
              outParentLinks[j]->updateMessage();
              M(parent)->incNumLambdaMsgsRcv();
            }
            finish = false;
          }
        }
      }
    }
  }
}



void
BPSolver::runLoopySolver()
{ 
  nIter_ = 0;
  while (!converged() && nIter_ < SolverOptions::maxIter) {

    nIter_++;
    if (DL >= 2) {
      cout << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
      cout << " Iteration " << nIter_ << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
    }
 
    switch (SolverOptions::schedule) {

      case SolverOptions::S_SEQ_RANDOM:
        random_shuffle (links_.begin(), links_.end());
        // no break
  
      case SolverOptions::S_SEQ_FIXED:
        for (unsigned i = 0; i < links_.size(); i++) {
          links_[i]->setNextMessage (getMessage (links_[i]));
          links_[i]->updateMessage();
          updateValues (links_[i]);
        }
        break;

      case SolverOptions::S_PARALLEL:
        for (unsigned i = 0; i < links_.size(); i++) {
          //calculateNextMessage (links_[i]);
        }
        for (unsigned i = 0; i < links_.size(); i++) {
          //updateMessage (links_[i]);
          //updateValues (links_[i]);
        }
        break;
 
      case SolverOptions::S_MAX_RESIDUAL:
        maxResidualSchedule();
        break;

    }
  }
}



bool
BPSolver::converged (void) const
{
  // this can happen if the graph is fully disconnected
  if (links_.size() == 0) {
    return true;
  }
  if (nIter_ == 0 || nIter_ == 1) {
    return false;
  }
  bool converged = true;
  if (SolverOptions::schedule == SolverOptions::S_MAX_RESIDUAL) {
    Param maxResidual = (*(sortedOrder_.begin()))->getResidual();
    if (maxResidual < SolverOptions::accuracy) {
      converged = true;
    } else {
      converged = false;
    }
  } else {
    CBnNodeSet nodes = bn_->getBayesNodes();
    for (unsigned i = 0; i < nodes.size(); i++) {
      if (!nodes[i]->hasEvidence()) {
        double change = M(nodes[i])->getBeliefChange();
        if (DL >= 2) {
          cout << nodes[i]->getLabel() + " belief change = " ;
          cout << change << endl;
        }
        if (change > SolverOptions::accuracy) {
          converged = false;
          if (DL == 0) break;
        }
      }
    }
  }
  return converged;
}



void
BPSolver::maxResidualSchedule (void)
{
  if (nIter_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      links_[i]->setNextMessage (getMessage (links_[i]));
      links_[i]->updateResidual();
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      edgeMap_.insert (make_pair (links_[i], it));
      if (DL >= 2) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (unsigned c = 0; c < sortedOrder_.size(); c++) {
    if (DL >= 2) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }
 
    SortedOrder::iterator it = sortedOrder_.begin();
    Edge* edge = *it;
    if (DL >= 2) {
      cout << "updating " << edge->toString() << endl;
    }
    if (edge->getResidual() < SolverOptions::accuracy) {
      return;
    }
    edge->updateMessage();
    updateValues (edge);
    edge->clearResidual();
    sortedOrder_.erase (it);
    edgeMap_.find (edge)->second = sortedOrder_.insert (edge);

     // update the messages that depend on message source --> destin
    CEdgeSet outChildLinks = 
        M(edge->getDestination())->getOutcomingChildLinks();
    for (unsigned i = 0; i < outChildLinks.size(); i++) {
      if (outChildLinks[i]->getDestination() != edge->getSource()) {
        if (DL >= 2) {
          cout << "    calculating " << outChildLinks[i]->toString() << endl;
        }
        outChildLinks[i]->setNextMessage (getMessage (outChildLinks[i]));
        outChildLinks[i]->updateResidual();
        EdgeMap::iterator iter = edgeMap_.find (outChildLinks[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (outChildLinks[i]);
      }
    }
    CEdgeSet outParentLinks =
        M(edge->getDestination())->getOutcomingParentLinks();
    for (unsigned i = 0; i < outParentLinks.size(); i++) {
      if (outParentLinks[i]->getDestination() != edge->getSource()) {
          //&& !outParentLinks[i]->getDestination()->hasEvidence()) FIXME{
        if (DL >= 2) {
          cout << "    calculating " << outParentLinks[i]->toString() << endl;
        }
        outParentLinks[i]->setNextMessage (getMessage (outParentLinks[i]));
        outParentLinks[i]->updateResidual();
        EdgeMap::iterator iter = edgeMap_.find (outParentLinks[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (outParentLinks[i]);
      }
    }

  }
}



void
BPSolver::updatePiValues (BayesNode* x)
{
  // π(Xi)
  if (DL >= 3) {
    cout << "updating " << PI << " values for " << x->getLabel() << endl;
  }
  CEdgeSet parentLinks = M(x)->getIncomingParentLinks();
  assert (x->getParents() == parentLinks.size());
  const vector<CptEntry>& entries = x->getCptEntries();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  ParamSet messageProducts (entries.size());
  for (unsigned k = 0; k < entries.size(); k++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = 1.0;
    const DConf& conf = entries[k].getDomainConfiguration();
    for (unsigned i = 0; i < parentLinks.size(); i++) {
      assert (parentLinks[i]->getSource() == parents[i]);
      assert (parentLinks[i]->getDestination() == x);
      messageProduct *= parentLinks[i]->getMessage()[conf[i]];
      if (DL >= 5) {
        if (i != 0) *calcs1 << "." ;
        if (i != 0) *calcs2 << "*" ;
        *calcs1 << PI << "(" << parentLinks[i]->getSource()->getLabel();
        *calcs1 << " --> " << x->getLabel() << ")" ;
        *calcs1 << "[" ;
        *calcs1 << parentLinks[i]->getSource()->getDomain()[conf[i]];
        *calcs1 << "]";
        *calcs2 << parentLinks[i]->getMessage()[conf[i]];
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

  for (unsigned xi = 0; xi < x->getDomainSize(); xi++) {
    double sum = 0.0;
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    for (unsigned k = 0; k < entries.size(); k++) {
      sum += x->getProbability (xi, entries[k]) * messageProducts[k];
      if (DL >= 5) {
        if (k != 0) *calcs1 << " + " ;
        if (k != 0) *calcs2 << " + " ;
        *calcs1 << x->cptEntryToString (xi, entries[k]); 
        *calcs1 << ".mp" << k;
        *calcs2 << x->getProbability (xi, entries[k]);
        *calcs2 << "*" << messageProducts[k];
      }
    }
    M(x)->setPiValue (xi, sum);
    if (DL >= 5) {
      cout << "    " << PI << "(" << x->getLabel() << ")" ;
      cout << "[" << x->getDomain()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      cout << " = " << (*calcs2).str();
      cout << " = " << sum << endl;
      delete calcs1;
      delete calcs2;
    }
  }
}



void
BPSolver::updateLambdaValues (BayesNode* x)
{
  // λ(Xi)
  if (DL >= 3) {
    cout << "updating " << LD << " values for " << x->getLabel() << endl;
  }
  CEdgeSet childLinks = M(x)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;

  for (unsigned xi = 0; xi < x->getDomainSize(); xi++) {
    double product = 1.0;
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    for (unsigned i = 0; i < childLinks.size(); i++) {
      assert (childLinks[i]->getDestination() == x);
      product *= childLinks[i]->getMessage()[xi];
      if (DL >= 5) {
        if (i != 0) *calcs1 << "." ;
        if (i != 0) *calcs2 << "*" ;
        *calcs1 << LD << "(" << childLinks[i]->getSource()->getLabel();
        *calcs1 << "-->" << x->getLabel() << ")" ;
        *calcs1 << "[" << x->getDomain()[xi] << "]" ;
        *calcs2 << childLinks[i]->getMessage()[xi];
      }
    }
    M(x)->setLambdaValue (xi, product);
    if (DL >= 5) {
      cout << "    " << LD << "(" << x->getLabel() << ")" ;
      cout << "[" << x->getDomain()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      if (childLinks.size() == 1) {
        cout << " = " << product << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << product << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }
}



ParamSet
BPSolver::calculateNextPiMessage (Edge* edge)
{
  // πX(Zi)
  BayesNode* z = edge->getSource();
  BayesNode* x = edge->getDestination();
  ParamSet zxPiNextMessage (z->getDomainSize());
  CEdgeSet zChildLinks = M(z)->getIncomingChildLinks();
  stringstream* calcs1 = 0;
  stringstream* calcs2 = 0;


  for (unsigned zi = 0; zi < z->getDomainSize(); zi++) {
    double product = M(z)->getPiValue (zi);
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
      *calcs1 << PI << "(" << z->getLabel() << ")";
      *calcs1 << "[" << z->getDomain()[zi] << "]" ;
      *calcs2 << product;
    }
    for (unsigned i = 0; i < zChildLinks.size(); i++) {
      assert (zChildLinks[i]->getDestination() == z);
      if (zChildLinks[i]->getSource() != x) {
        product *= zChildLinks[i]->getMessage()[zi];
        if (DL >= 5) {
          *calcs1 << "." << LD << "(" << zChildLinks[i]->getSource()->getLabel();
          *calcs1 << "-->" << z->getLabel() << ")";
          *calcs1 << "[" << z->getDomain()[zi] + "]" ;
          *calcs2 << " * " << zChildLinks[i]->getMessage()[zi];
        }
      }
    }
    zxPiNextMessage[zi] = product;
    if (DL >= 5) {
      cout << "    " << PI << "(" << z->getLabel();
      cout << "-->" << x->getLabel() << ")" ;
      cout << "["  << z->getDomain()[zi] << "]" ;
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
  return zxPiNextMessage;
}



ParamSet
BPSolver::calculateNextLambdaMessage (Edge* edge)
{
  // λY(Xi)
  BayesNode* y = edge->getSource();
  BayesNode* x = edge->getDestination();
  if (!M(y)->receivedBottomInfluence()) {
    //cout << "returning 1" << endl;
    //return edge->getMessage();
  }
  if (x->hasEvidence()) {
    //cout << "returning 2" << endl;
    //return edge->getMessage();
  }
  ParamSet yxLambdaNextMessage (x->getDomainSize());
  CEdgeSet yParentLinks               = M(y)->getIncomingParentLinks();
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
    double messageProduct = 1.0;
    const DConf& conf = entries[k].getDomainConfiguration();
    for (unsigned i = 0; i < yParentLinks.size(); i++) {
      assert (yParentLinks[i]->getDestination() == y);
      if (yParentLinks[i]->getSource() != x) {
        if (DL >= 5) {
          if (messageProduct != 1.0) *calcs1 << "*" ;
          if (messageProduct != 1.0) *calcs2 << "*" ;
          *calcs1 << PI << "(" << yParentLinks[i]->getSource()->getLabel();
          *calcs1 << "-->" << y->getLabel() << ")" ;
          *calcs1 << "[" ;
          *calcs1 << yParentLinks[i]->getSource()->getDomain()[conf[i]];
          *calcs1 << "]" ;
          *calcs2 << yParentLinks[i]->getMessage()[conf[i]];
        }
        messageProduct *= yParentLinks[i]->getMessage()[conf[i]];
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

  for (unsigned xi = 0; xi < x->getDomainSize(); xi++) {
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
    double outerSum = 0.0;
    for (unsigned yi = 0; yi < y->getDomainSize(); yi++) {
      if (DL >= 5) {
        (yi != 0) ? *calcs1 << " + {" : *calcs1 << "{" ;
        (yi != 0) ? *calcs2 << " + {" : *calcs2 << "{" ;
      } 
      double innerSum = 0.0;
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
      outerSum += innerSum * M(y)->getLambdaValue (yi);
      if (DL >= 5) {
        *calcs1 << "}." << LD << "(" << y->getLabel() << ")" ;
        *calcs1 << "["  << y->getDomain()[yi] << "]";
        *calcs2 << "}*" << M(y)->getLambdaValue (yi);
      }
    }
    yxLambdaNextMessage[xi] = outerSum;
    if (DL >= 5) {
      cout << "    " << LD << "(" << y->getLabel();
      cout << "-->" << x->getLabel() << ")" ;
      cout << "[" << x->getDomain()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      cout << " = " << (*calcs2).str();
      cout << " = " << outerSum << endl;
      delete calcs1;
      delete calcs2;
    }
  }
  return yxLambdaNextMessage;
}



ParamSet
BPSolver::getJointByJunctionNode (const VidSet& jointVids) const
{
  BnNodeSet jointVars;
  for (unsigned i = 0; i < jointVids.size(); i++) {
    jointVars.push_back (bn_->getBayesNode (jointVids[i]));
  }

  BayesNet* mrn = bn_->getMinimalRequesiteNetwork (jointVids);

  BnNodeSet parents;
  unsigned dsize = 1;
  for (unsigned i = 0; i < jointVars.size(); i++) {
    parents.push_back (mrn->getBayesNode (jointVids[i]));
    dsize *= jointVars[i]->getDomainSize();
  }

  unsigned nParams = dsize * dsize;
  ParamSet params (nParams);

  for (unsigned i = 0; i < nParams; i++) {
    unsigned row = i / dsize;
    unsigned col = i % dsize;
    if (row == col) {
      params[i] = 1;
    } else {
      params[i] = 0;
    }
  }

  unsigned maxVid = std::numeric_limits<unsigned>::max();
  Distribution* dist = new Distribution (params);

  mrn->addNode (maxVid, dsize, NO_EVIDENCE, parents, dist);
  mrn->setIndexes();

  BPSolver solver (*mrn);
  solver.runSolver();

  const ParamSet& results = solver.getPosterioriOf (maxVid);

  delete mrn;
  delete dist;

  return results;
}



ParamSet
BPSolver::getJointByChainRule (const VidSet& jointVids) const
{
  BnNodeSet jointVars;
  for (unsigned i = 0; i < jointVids.size(); i++) {
    jointVars.push_back (bn_->getBayesNode (jointVids[i]));
  }

  BayesNet* mrn = bn_->getMinimalRequesiteNetwork (jointVids[0]);
  BPSolver solver (*mrn);
  solver.runSolver();
  ParamSet prevBeliefs = solver.getPosterioriOf (jointVids[0]);
  delete mrn;

  VarSet observedVars = {jointVars[0]};

  for (unsigned i = 1; i < jointVids.size(); i++) {
    mrn = bn_->getMinimalRequesiteNetwork (jointVids[i]);
    ParamSet newBeliefs;
    vector<DConf> confs = 
        Util::getDomainConfigurations (observedVars);
    for (unsigned j = 0; j < confs.size(); j++) {
      for (unsigned k = 0; k < observedVars.size(); k++) {
        if (!observedVars[k]->hasEvidence()) {
          BayesNode* node = mrn->getBayesNode (observedVars[k]->getVarId());
          if (node) {
            node->setEvidence (confs[j][k]);
          }
        }
      }
      BPSolver solver (*mrn);
      solver.runSolver();
      ParamSet beliefs = solver.getPosterioriOf (jointVids[i]);
      for (unsigned k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
    }

    int count = -1;
    for (unsigned j = 0; j < newBeliefs.size(); j++) {
      if (j % jointVars[i]->getDomainSize() == 0) {
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
BPSolver::printMessageStatusOf (const BayesNode* var) const
{
  cout << left;
  cout << setw (10) << "domain" ;
  cout << setw (20) << PI << "(" + var->getLabel() + ")" ;
  cout << setw (20) << LD << "(" + var->getLabel() + ")" ;
  cout << setw (16) << "belief" ;
  cout << endl;
  cout << "--------------------------------" ;
  cout << "--------------------------------" ;
  cout << endl;

  BPNodeInfo*     x        = M(var);
  ParamSet&       piVals   = x->getPiValues();
  ParamSet&       ldVals   = x->getLambdaValues();
  ParamSet        beliefs  = x->getBeliefs();
  const Domain&   domain   = var->getDomain();
  CBnNodeSet&     childs   = var->getChilds();

  for (unsigned xi = 0; xi < var->getDomainSize(); xi++) {
    cout << setw (10) << domain[xi];
    cout << setw (19) << piVals[xi];
    cout << setw (19) << ldVals[xi];
    cout.precision (PRECISION);
    cout << setw (16) << beliefs[xi];
    cout << endl;
  }
  cout << endl;
  if (childs.size() > 0) {
    string s = "(" + var->getLabel() + ")" ;
    for (unsigned j = 0; j < childs.size(); j++) {
      cout << setw (10) << "domain" ;
      cout << setw (28) << PI + childs[j]->getLabel() + s;
      cout << setw (28) << LD + childs[j]->getLabel() + s;
      cout << endl;
      cout << "--------------------------------" ;
      cout << "--------------------------------" ;
      cout << endl;
      /* FIXME
      const ParamSet& piMessage     = x->getPiMessage (childs[j]);
      const ParamSet& lambdaMessage = x->getLambdaMessage (childs[j]);
      for (unsigned xi = 0; xi < var->getDomainSize(); xi++) {
        cout << setw (10) << domain[xi];
        cout.precision (PRECISION);
        cout << setw (27) << piMessage[xi];
        cout.precision (PRECISION);
        cout << setw (27) << lambdaMessage[xi];
        cout << endl;
      }
      cout << endl;
      */
    }
  }
}



void 
BPSolver::printAllMessageStatus (void) const
{ 
  CBnNodeSet nodes = bn_->getBayesNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    printMessageStatusOf (nodes[i]);
  }
}

