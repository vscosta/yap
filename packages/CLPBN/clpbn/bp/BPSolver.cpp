#include <cstdlib>
#include <time.h>
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "BPSolver.h"
#include "BpNode.h"


BPSolver* Edge::klass = 0;
StatisticMap Statistics::stats_;
unsigned Statistics::numCreatedNets     = 0;
unsigned Statistics::numSolvedPolyTrees = 0;
unsigned Statistics::numSolvedLoopyNets = 0;
unsigned Statistics::numUnconvergedRuns = 0;
unsigned Statistics::maxIterations      = 0;
unsigned Statistics::totalOfIterations  = 0;


BPSolver::BPSolver (const BayesNet& bn) : Solver (&bn)
{
  bn_ = &bn;
  forceGenericSolver_ = false;
  //forceGenericSolver_ = true;
  schedule_           = S_SEQ_FIXED;
  //schedule_           = S_SEQ_RANDOM;
  //schedule_           = S_PARALLEL;
  //schedule_           = S_MAX_RESIDUAL;
  maxIter_            = 205;
  accuracy_           = 0.000001;
}



BPSolver::~BPSolver (void)
{
  for (unsigned i = 0; i < msgs_.size(); i++) {
    delete msgs_[i];
  }
}



void
BPSolver::runSolver (void)
{
  if (DL >= 1) {
    //bn_->printNetwork();
  }

  clock_t start_ = clock();
  if (bn_->isSingleConnected() && !forceGenericSolver_) {
    runPolyTreeSolver();
    Statistics::numSolvedPolyTrees ++;
  } else {
    runGenericSolver();
    Statistics::numSolvedLoopyNets ++;
    if (nIter_ >= maxIter_) {
      Statistics::numUnconvergedRuns ++;
    } else {
      Statistics::updateIterations (nIter_);
    }
    if (DL >= 1) {
      cout << endl;
      if (nIter_ < maxIter_) {
        cout << "Belief propagation converged in " ; 
        cout << nIter_ << " iterations" << endl;
      } else {
        cout << "The maximum number of iterations was hit, terminating..." ;
        cout << endl;
      }
    }
  }
  double time = (double (clock() - start_)) / CLOCKS_PER_SEC;
  unsigned size = bn_->getNumberOfNodes();
  Statistics::updateStats (size, time);
  //if (size > 30) {
  //   stringstream ss;
  //  ss << size << "." << Statistics::getCounting (size) << ".dot" ;
  //  bn_->exportToDotFile (ss.str().c_str());
  //}
}



ParamSet
BPSolver::getPosterioriOf (const Variable* var) const
{
  assert (var);
  assert (var == bn_->getNode (var->getVarId()));
  assert (var->getIndex() < msgs_.size());
  return msgs_[var->getIndex()]->getBeliefs();
}




ParamSet
BPSolver::getJointDistribution (const NodeSet& jointVars) const
{
  if (DL >= 1) {
    cout << "calculating joint distribuition on: " ;
    for (unsigned i = 0; i < jointVars.size(); i++) {
      cout << jointVars[i]->getLabel() << " " ;
    }
    cout << endl;
  }

  //BayesNet* workingNet = bn_->pruneNetwork (bn_->getNodes());
  //FIXME see if this works:
  BayesNet* workingNet = bn_->pruneNetwork (jointVars);
  BayesNode* node      = workingNet->getNode (jointVars[0]->getVarId());

  BayesNet* tempNet    = workingNet->pruneNetwork (node);
  BPSolver solver (*tempNet);
  solver.runSolver();

  NodeSet observedVars = { jointVars[0] };

  node = tempNet->getNode (jointVars[0]->getVarId());
  ParamSet prevBeliefs = solver.getPosterioriOf (node);

  delete tempNet;

  for (unsigned i = 1; i < jointVars.size(); i++) {
    node = workingNet->getNode (observedVars[i - 1]->getVarId());
    if (!node->hasEvidence()) {
      node->setEvidence (0);
    }
    node = workingNet->getNode (jointVars[i]->getVarId());
    tempNet = workingNet->pruneNetwork (node);

    ParamSet allBeliefs;
    vector<DomainConf> confs = 
        BayesNet::getDomainConfigurationsOf (observedVars);
    for (unsigned j = 0; j < confs.size(); j++) {
      for (unsigned k = 0; k < observedVars.size(); k++) {
        node = tempNet->getNode (observedVars[k]->getVarId());
        if (!observedVars[k]->hasEvidence()) {
          if (node) {
            node->setEvidence (confs[j][k]);
          } else {
            // FIXME try optimize
            //assert (false);
            cout << observedVars[k]->getLabel();
            cout << " is not in temporary net!" ;
            cout << endl;
          }
        } else {
          cout << observedVars[k]->getLabel();
          cout << " already has evidence in original net!" ;
          cout << endl;
        }
      }
      BPSolver solver (*tempNet);
      node = tempNet->getNode (jointVars[i]->getVarId());
      solver.runSolver();
      ParamSet beliefs = solver.getPosterioriOf (node);
      for (unsigned k = 0; k < beliefs.size(); k++) {
        allBeliefs.push_back (beliefs[k]);
      }
    }

    int count = -1;
    for (unsigned j = 0; j < allBeliefs.size(); j++) {
      if (j % jointVars[i]->getDomainSize() == 0) {
        count ++;
      }
      allBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = allBeliefs;
    observedVars.push_back (jointVars[i]);
    delete tempNet;
  }
  delete workingNet;
  return prevBeliefs;
}



void 
BPSolver::initializeSolver (void)
{
  if (DL >= 1) {
    cout << "Initializing solver" << endl;
    cout << "-> schedule        = ";
    if (forceGenericSolver_) {
      switch (schedule_) {
        case S_SEQ_FIXED:    cout << "sequential fixed" ;  break;
        case S_SEQ_RANDOM:   cout << "sequential random" ; break;
        case S_PARALLEL:     cout << "parallel" ;          break;
        case S_MAX_RESIDUAL: cout << "max residual" ;      break;
      }
    } else {
      cout << "polytree solver" ;
    }
    cout << endl;
    cout << "-> max iters       = " << maxIter_ << endl;
    cout << "-> accuracy        = " << accuracy_ << endl;
    cout << endl;
  }

  const NodeSet& nodes = bn_->getNodes();
  for (unsigned i = 0; i < msgs_.size(); i++) {
    delete msgs_[i];
  }
  msgs_.clear();
  msgs_.reserve (nodes.size());
  updateOrder_.clear();
  sortedOrder_.clear();
  edgeMap_.clear();

  for (unsigned i = 0; i < nodes.size(); i++) {
    msgs_.push_back (new BpNode (nodes[i]));
  }

  NodeSet roots = bn_->getRootNodes();
  for (unsigned i = 0; i < roots.size(); i++) {
    const ParamSet& params = roots[i]->getParameters();
    ParamSet& piVals = M(roots[i])->getPiValues();
    for (int ri = 0; ri < roots[i]->getDomainSize(); ri++) {
      piVals[ri] = params[ri];
    }
  }
}



void 
BPSolver::incorporateEvidence (BayesNode* x)
{
  ParamSet& piVals = M(x)->getPiValues();
  ParamSet& ldVals = M(x)->getLambdaValues();
  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    piVals[xi] = 0.0;
    ldVals[xi] = 0.0;
  }
  piVals[x->getEvidence()] = 1.0;
  ldVals[x->getEvidence()] = 1.0;
}



void
BPSolver::runPolyTreeSolver (void)
{
  initializeSolver();
  const NodeSet& nodes = bn_->getNodes();

  // Hack: I need this else this can happen with bayes ball
  // Variable: 174
  // Id:       174
  // Domain:   -1, 0, 1
  // Evidence: 1
  // Parents:  
  // Childs:   176
  // cpt
  // ----------------------------------------------------
  // -1             0           0           0           0 ...
  // 0       0.857143    0.857143    0.857143    0.857143 ...
  // 1       0.142857    0.142857    0.142857    0.142857 ...
  // the cpt for this node would be 0,0,0

  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->hasEvidence()) {
      incorporateEvidence (nodes[i]);
    }
  } 

  // first compute all node marginals ...
  NodeSet roots = bn_->getRootNodes();
  for (unsigned i = 0; i < roots.size(); i++) {
    const NodeSet& childs = roots[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      polyTreePiMessage (roots[i], childs[j]);
    }
  }
  // then propagate the evidence
  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->hasEvidence()) {
      incorporateEvidence (nodes[i]); 
      const NodeSet& parents = nodes[i]->getParents();
      for (unsigned j = 0; j < parents.size(); j++) {
        if (!parents[j]->hasEvidence()) {
          polyTreeLambdaMessage (nodes[i], parents[j]);
        }
      }
      const NodeSet& childs = nodes[i]->getChilds();
      for (unsigned j = 0; j < childs.size(); j++) {
        polyTreePiMessage (nodes[i], childs[j]);
      }
    }
  }
}



void
BPSolver::polyTreePiMessage (BayesNode* z, BayesNode* x)
{
  if (DL >= 1) {
    cout << PI << " (" << z->getLabel();
    cout << " --> " << x->getLabel();
    cout << ")" << endl;
  }
  calculateNextPiMessage (z, x);
  updatePiMessage (z, x);

  if (!x->hasEvidence()) {
    updatePiValues (x);
    const NodeSet& xChilds = x->getChilds();
    for (unsigned i = 0; i < xChilds.size(); i++) {
      polyTreePiMessage (x, xChilds[i]);
    }
  }

  if (M(x)->hasReceivedChildInfluence()) {
    const NodeSet& xParents = x->getParents();
    for (unsigned i = 0; i < xParents.size(); i++) {
      if (xParents[i] != z && !xParents[i]->hasEvidence()) {
        polyTreeLambdaMessage (x, xParents[i]);
      }
    }
  }
}



void 
BPSolver::polyTreeLambdaMessage (BayesNode* y, BayesNode* x)
{
  if (DL >= 1) {
    cout << LD << " (" << y->getLabel();
    cout << " --> " << x->getLabel();
    cout << ")" << endl;
  }
  calculateNextLambdaMessage (y, x);
  updateLambdaMessage (y, x);
  updateLambdaValues (x);

  const NodeSet& xParents = x->getParents();
  for (unsigned i = 0; i < xParents.size(); i++) {
    if (!xParents[i]->hasEvidence()) {
      polyTreeLambdaMessage (x, xParents[i]);
    }
  }

  const NodeSet& xChilds = x->getChilds();
  for (unsigned i = 0; i < xChilds.size(); i++) {
    if (xChilds[i] != y) {
      polyTreePiMessage (x, xChilds[i]);
    }
  }
}



void
BPSolver::runGenericSolver()
{ 
  initializeSolver();
  const NodeSet& nodes = bn_->getNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    if (nodes[i]->hasEvidence()) {
      incorporateEvidence (nodes[i]);
    }
  }
 
  for (unsigned i = 0; i < nodes.size(); i++) {
    // pi messages
    const NodeSet& childs = nodes[i]->getChilds();
    for (unsigned j = 0; j < childs.size(); j++) {
      updateOrder_.push_back (Edge (nodes[i], childs[j], PI_MSG));
    }
    // lambda messages
    const NodeSet& parents = nodes[i]->getParents();
    for (unsigned j = 0; j < parents.size(); j++) {
      if (!parents[j]->hasEvidence()) {
        updateOrder_.push_back (Edge (nodes[i], parents[j], LAMBDA_MSG));
      }
    }
  }

  nIter_ = 0;
  while (!converged() && nIter_ < maxIter_) {

    nIter_++;
    if (DL >= 1) {
      cout << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
      cout << " Iteration " << nIter_ << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
    }
 
    switch (schedule_) {

      case S_SEQ_RANDOM:
        random_shuffle (updateOrder_.begin(), updateOrder_.end());
        // no break
  
      case S_SEQ_FIXED:
        for (unsigned i = 0; i < updateOrder_.size(); i++) {
          calculateNextMessage (updateOrder_[i]);
          updateMessage (updateOrder_[i]);
          updateValues (updateOrder_[i]);
        }
        break;

      case S_PARALLEL:
        for (unsigned i = 0; i < updateOrder_.size(); i++) {
          calculateNextMessage (updateOrder_[i]);
        }
        for (unsigned i = 0; i < updateOrder_.size(); i++) {
          updateMessage (updateOrder_[i]);
          updateValues (updateOrder_[i]);
        }
        break;
 
      case S_MAX_RESIDUAL:
        maxResidualSchedule();
        break;

    }
  }
}



void
BPSolver::maxResidualSchedule (void)
{
  if (nIter_ == 1) {
    Edge::klass = this;
    for (unsigned i = 0; i < updateOrder_.size(); i++) {
      calculateNextMessage (updateOrder_[i]);
      updateResidual (updateOrder_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (updateOrder_[i]);
      edgeMap_.insert (make_pair (updateOrder_[i].getId(), it));
    }
    return;
  }

  for (unsigned c = 0; c < sortedOrder_.size(); c++) {
    if (DL >= 1) {
      for (set<Edge, compare>::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << it->toString() << " residual = " ;
        cout << getResidual (*it) << endl;
      }
    }
 
    set<Edge, compare>::iterator it = sortedOrder_.begin();
    Edge e = *it;
    if (getResidual (e) < accuracy_) {
      return;
    }
    updateMessage (e);
    updateValues (e);
    clearResidual (e);
    sortedOrder_.erase (it);
    assert (edgeMap_.find (e.getId()) != edgeMap_.end());
    edgeMap_.find (e.getId())->second = sortedOrder_.insert (e);

     // update the messages that depend on message source --> destination
    const NodeSet& childs = e.destination->getChilds();
    for (unsigned i = 0; i < childs.size(); i++) {
      if (childs[i] != e.source) {
        Edge neighbor (e.destination, childs[i], PI_MSG);
        calculateNextMessage (neighbor);
        updateResidual (neighbor);
        assert (edgeMap_.find (neighbor.getId()) != edgeMap_.end());
        EdgeMap::iterator iter = edgeMap_.find (neighbor.getId());
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (neighbor);
      }
    }
    const NodeSet& parents = e.destination->getParents();
    for (unsigned i = 0; i < parents.size(); i++) {
      if (parents[i] != e.source && !parents[i]->hasEvidence()) {
        Edge neighbor (e.destination, parents[i], LAMBDA_MSG);
        calculateNextMessage (neighbor);
        updateResidual (neighbor);
        assert (edgeMap_.find (neighbor.getId()) != edgeMap_.end());
        EdgeMap::iterator iter = edgeMap_.find (neighbor.getId());
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (neighbor);
      }
    }
  }
}



bool
BPSolver::converged (void) const
{
  bool converged = true;
  if (schedule_ == S_MAX_RESIDUAL) {
    if (nIter_ <= 2) {
      return false;
    }
    // this can happen if every node does not have neighbors
    if (sortedOrder_.size() == 0) {
      return true;
    }
    Param maxResidual = getResidual (*(sortedOrder_.begin()));
    if (maxResidual > accuracy_) {
      return false;
    }
  } else {
    if (nIter_ == 0) {
      return false;
    }
    const NodeSet& nodes = bn_->getNodes();
    for (unsigned i = 0; i < nodes.size(); i++) {
      if (!nodes[i]->hasEvidence()) {
        double change = M(nodes[i])->getBeliefChange();
        if (DL >= 1) {
          cout << nodes[i]->getLabel() + " belief change = " ;
          cout << change << endl;
        }
        if (change > accuracy_) {
          converged = false;
          if (DL == 0) break;
        }
      }
    }
  }

  return converged;
}



void
BPSolver::updatePiValues (BayesNode* x)
{
  // π(Xi)
  const NodeSet& parents = x->getParents();
  const vector<CptEntry>& entries = x->getCptEntries();
  assert (parents.size() != 0);
  stringstream* calcs1;
  stringstream* calcs2;

  ParamSet messageProducts (entries.size());
  for (unsigned k = 0; k < entries.size(); k++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    double messageProduct = 1.0;
    const DomainConf& conf = entries[k].getParentConfigurations();
    for (unsigned i = 0; i < parents.size(); i++) {
      messageProduct *= M(parents[i])->getPiMessageValue(x, conf[i]);
      if (DL >= 5) {
        if (i != 0) *calcs1 << "." ;
        if (i != 0) *calcs2 << "*" ;
        *calcs1 << PI << "(" << x->getLabel() << ")" ;
        *calcs1 << "[" << parents[i]->getDomain()[conf[i]] << "]";
        *calcs2 << M(parents[i])->getPiMessageValue(x, conf[i]);
      }
    }
    messageProducts[k] = messageProduct;
    if (DL >= 5) {
      cout << "    mp" << k;
      cout << " = " << (*calcs1).str();
      if (parents.size() == 1) {
        cout << " = " << messageProduct << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << messageProduct << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }

  for (int xi = 0; xi < x->getDomainSize(); xi++) {
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
  const NodeSet& childs = x->getChilds();
  assert (childs.size() != 0);
  stringstream* calcs1;
  stringstream* calcs2;

  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    double product = 1.0;
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    for (unsigned i = 0; i < childs.size(); i++) {
      product *= M(x)->getLambdaMessageValue(childs[i], xi);
      if (DL >= 5) {
        if (i != 0) *calcs1 << "." ;
        if (i != 0) *calcs2 << "*" ;
        *calcs1 << LD << "(" << childs[i]->getLabel();
        *calcs1 << "-->" << x->getLabel() << ")" ;
        *calcs1 << "[" << x->getDomain()[xi] << "]" ;
        *calcs2 << M(x)->getLambdaMessageValue(childs[i], xi);
      }
    }
    M(x)->setLambdaValue (xi, product);
    if (DL >= 5) {
      cout << "    " << LD << "(" << x->getLabel() << ")" ;
      cout << "[" << x->getDomain()[xi] << "]" ;
      cout << " = " << (*calcs1).str();
      if (childs.size() == 1) {
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



void
BPSolver::calculateNextPiMessage (BayesNode* z, BayesNode* x)
{
  // πX(Zi)
  ParamSet& zxPiNextMessage = M(z)->piNextMessageReference (x);
  const NodeSet& zChilds = z->getChilds();
  stringstream* calcs1;
  stringstream* calcs2;

  for (int zi = 0; zi < z->getDomainSize(); zi++) {
    double product = M(z)->getPiValue (zi);
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
      *calcs1 << PI << "(" << z->getLabel() << ")";
      *calcs1 << "[" << z->getDomain()[zi] << "]" ;
      *calcs2 << product;
    }
    for (unsigned i = 0; i < zChilds.size(); i++) {
      if (zChilds[i] != x) {
        product *= M(z)->getLambdaMessageValue(zChilds[i], zi);
        if (DL >= 5) {
          *calcs1 << "." << LD << "(" << zChilds[i]->getLabel();
          *calcs1 << "-->" << z->getLabel() << ")";
          *calcs1 << "[" << z->getDomain()[zi] + "]" ;
          *calcs2 << " * " << M(z)->getLambdaMessageValue(zChilds[i], zi);
        }
      }
    }
    zxPiNextMessage[zi] = product;
    if (DL >= 5) {
      cout << "    " << PI << "(" << z->getLabel();
      cout << "-->" << x->getLabel() << ")" ;
      cout << "["  << z->getDomain()[zi] << "]" ;
      cout << " = " << (*calcs1).str();
      if (zChilds.size() == 1) {
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



void
BPSolver::calculateNextLambdaMessage (BayesNode* y, BayesNode* x)
{
  // λY(Xi)
  //if (!y->hasEvidence() && !M(y)->hasReceivedChildInfluence()) {
  //  if (DL >= 5) {
  //    cout << "unnecessary calculation" << endl;
  //  }
  //  return;
  //}
  ParamSet& yxLambdaNextMessage       = M(x)->lambdaNextMessageReference (y);
  const NodeSet& yParents             = y->getParents();
  const vector<CptEntry>& allEntries  = y->getCptEntries();
  int parentIndex                     = y->getIndexOfParent (x);
  stringstream* calcs1;
  stringstream* calcs2;
 
  vector<CptEntry> entries;
  DomainConstr constr = make_pair (parentIndex, 0);
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
    const DomainConf& conf = entries[k].getParentConfigurations();
    for (unsigned i = 0; i < yParents.size(); i++) {
      if (yParents[i] != x) {
        if (DL >= 5) {
          if (messageProduct != 1.0) *calcs1 << "*" ;
          if (messageProduct != 1.0) *calcs2 << "*" ;
          *calcs1 << PI << "(" << yParents[i]->getLabel();
          *calcs1 << "-->" << y->getLabel() << ")" ;
          *calcs1 << "[" << yParents[i]->getDomain()[conf[i]] << "]" ;
          *calcs2 << M(yParents[i])->getPiMessageValue(y, conf[i]);
        }
        messageProduct *= M(yParents[i])->getPiMessageValue(y, conf[i]);
      }
    }
    messageProducts[k] = messageProduct;
    if (DL >= 5) {
      cout << "    mp" << k;
      cout << " = " << (*calcs1).str();
      if  (yParents.size() == 1) {
        cout << 1 << endl;
      } else if (yParents.size() == 2) {
        cout << " = " << messageProduct << endl;
      } else {
        cout << " = " << (*calcs2).str();
        cout << " = " << messageProduct << endl;
      }
      delete calcs1;
      delete calcs2;
    }
  }

  for (int xi = 0; xi < x->getDomainSize(); xi++) {
    if (DL >= 5) {
      calcs1 = new stringstream;
      calcs2 = new stringstream;
    }
    vector<CptEntry> entries;
    DomainConstr constr = make_pair (parentIndex, xi);
    for (unsigned i = 0; i < allEntries.size(); i++) {
      if (allEntries[i].matchConstraints(constr)) {
        entries.push_back (allEntries[i]);
      }
    }
    double outerSum = 0.0;
    for (int yi = 0; yi < y->getDomainSize(); yi++) {
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

  BpNode*         x        = M(var);
  ParamSet&       piVals   = x->getPiValues();
  ParamSet&       ldVals   = x->getLambdaValues();
  ParamSet        beliefs  = x->getBeliefs();
  const Domain&   domain   = var->getDomain();
  const NodeSet&  childs   = var->getChilds();

  for (int xi = 0; xi < var->getDomainSize(); xi++) {
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
      const ParamSet& piMessage     = x->getPiMessage (childs[j]);
      const ParamSet& lambdaMessage = x->getLambdaMessage (childs[j]);
      for (int xi = 0; xi < var->getDomainSize(); xi++) {
        cout << setw (10) << domain[xi];
        cout.precision (PRECISION);
        cout << setw (27) << piMessage[xi];
        cout.precision (PRECISION);
        cout << setw (27) << lambdaMessage[xi];
        cout << endl;
      }
      cout << endl;
    }
  }
}



void 
BPSolver::printAllMessageStatus (void) const
{ 
  const NodeSet& nodes = bn_->getNodes();
  for (unsigned i = 0; i < nodes.size(); i++) {
    printMessageStatusOf (nodes[i]);
  }
}


