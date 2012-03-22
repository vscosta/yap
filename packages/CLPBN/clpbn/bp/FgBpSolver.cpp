#include <cassert>
#include <limits>

#include <algorithm>

#include <iostream>

#include "FgBpSolver.h"
#include "FactorGraph.h"
#include "Factor.h"
#include "Indexer.h"
#include "Horus.h"


FgBpSolver::FgBpSolver (const FactorGraph& fg) : Solver (&fg)
{
  factorGraph_ = &fg;
}



FgBpSolver::~FgBpSolver (void)
{
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  for (unsigned i = 0; i < facsI_.size(); i++) {
    delete facsI_[i];
  }
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



void
FgBpSolver::runSolver (void)
{
  clock_t start;
  if (COLLECT_STATISTICS) {
    start = clock();
  }
  runLoopySolver();
  if (DL >= 2) {
    cout << endl;
    if (nIters_ < BpOptions::maxIter) {
     cout << "Sum-Product converged in " ; 
      cout << nIters_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
  }
  unsigned size = factorGraph_->getVarNodes().size();
  if (COLLECT_STATISTICS) {
    unsigned nIters = 0;
    bool loopy = factorGraph_->isTree() == false;
    if (loopy) nIters = nIters_;
    double time = (double (clock() - start)) / CLOCKS_PER_SEC;
    Statistics::updateStatistics (size, loopy, nIters, time);
  }
  if (EXPORT_TO_GRAPHVIZ && size > EXPORT_MINIMAL_SIZE) {
    stringstream ss;
    ss << Statistics::getSolvedNetworksCounting() << "." << size << ".dot" ;
    factorGraph_->exportToGraphViz (ss.str().c_str());
  }
}



Params
FgBpSolver::getPosterioriOf (VarId vid)
{
  assert (factorGraph_->getFgVarNode (vid));
  FgVarNode* var = factorGraph_->getFgVarNode (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->nrStates(), Util::noEvidence());
    probs[var->getEvidence()] = Util::withEvidence();
  } else {
    probs.resize (var->nrStates(), Util::multIdenty());
    const SpLinkSet& links = ninf(var)->getLinks();
    if (Globals::logDomain) {
      for (unsigned i = 0; i < links.size(); i++) {
        Util::add (probs, links[i]->getMessage());
      }
      Util::normalize (probs);
      Util::fromLog (probs);
    } else {
      for (unsigned i = 0; i < links.size(); i++) {
        Util::multiply (probs, links[i]->getMessage());
      }
      Util::normalize (probs);
    }
  }
  return probs;
}



Params
FgBpSolver::getJointDistributionOf (const VarIds& jointVarIds)
{
  FgVarNode* vn = factorGraph_->getFgVarNode (jointVarIds[0]);
  const FgFacSet& factorNodes = vn->neighbors();
  int idx = -1;  
  for (unsigned i = 0; i < factorNodes.size(); i++) {
    if (factorNodes[i]->factor()->contains (jointVarIds)) {
      idx = i;
      break;
    }
  }
  if (idx == -1) {
    return getJointByConditioning (jointVarIds);
  } else {
    Factor r (*factorNodes[idx]->factor());
    const SpLinkSet& links = ninf(factorNodes[idx])->getLinks();
    for (unsigned i = 0; i < links.size(); i++) {
      Factor msg (links[i]->getVariable()->varId(),
                  links[i]->getVariable()->nrStates(),
                  getVar2FactorMsg (links[i]));
      r.multiply (msg);
    }
    r.sumOutAllExcept (jointVarIds);
    r.reorderVariables (jointVarIds);
    r.normalize();
    Params jointDist = r.getParameters();
    if (Globals::logDomain) {
      Util::fromLog (jointDist);
    }
    return jointDist;
  }
}



void
FgBpSolver::runLoopySolver (void)
{
  initializeSolver();
  nIters_ = 0;

  while (!converged() && nIters_ < BpOptions::maxIter) {

    nIters_ ++;
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
        }
        break;

      case BpOptions::Schedule::PARALLEL:
        for (unsigned i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (unsigned i = 0; i < links_.size(); i++) {
          updateMessage(links_[i]);
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



void
FgBpSolver::initializeSolver (void)
{
  const FgVarSet& varNodes = factorGraph_->getVarNodes();
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  varsI_.reserve (varNodes.size());
  for (unsigned i = 0; i < varNodes.size(); i++) {
    varsI_.push_back (new SPNodeInfo());
  }

  const FgFacSet& facNodes = factorGraph_->getFactorNodes();
  for (unsigned i = 0; i < facsI_.size(); i++) {
    delete facsI_[i];
  }
  facsI_.reserve (facNodes.size());
  for (unsigned i = 0; i < facNodes.size(); i++) {
    facsI_.push_back (new SPNodeInfo());
  }

  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];	
  }
  createLinks();

  for (unsigned i = 0; i < links_.size(); i++) {
    FgFacNode* src = links_[i]->getFactor();
    FgVarNode* dst = links_[i]->getVariable();
    ninf (dst)->addSpLink (links_[i]);
    ninf (src)->addSpLink (links_[i]);
  }
}



void
FgBpSolver::createLinks (void)
{
  const FgFacSet& facNodes = factorGraph_->getFactorNodes();
  for (unsigned i = 0; i < facNodes.size(); i++) {
    const FgVarSet& neighbors = facNodes[i]->neighbors();
    for (unsigned j = 0; j < neighbors.size(); j++) {
      links_.push_back (new SpLink (facNodes[i], neighbors[j]));
    }
  }
}



bool
FgBpSolver::converged (void)
{
  if (links_.size() == 0) {
    return true;
  }
  if (nIters_ == 0 || nIters_ == 1) {
    return false;
  }
  bool converged = true;
  if (BpOptions::schedule == BpOptions::Schedule::MAX_RESIDUAL) {
    double maxResidual = (*(sortedOrder_.begin()))->getResidual();
    if (maxResidual > BpOptions::accuracy) {
      converged = false;
    } else {
      converged = true;
    }
  } else {
    for (unsigned i = 0; i < links_.size(); i++) {
      double residual = links_[i]->getResidual();
      if (DL >= 2) {
        cout << links_[i]->toString() + " residual = " << residual << endl;
      }
      if (residual > BpOptions::accuracy) {
        converged = false;
        if (DL == 0) break;
      }
    }
  }
  return converged;
}



void
FgBpSolver::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
    }
    return;
  }

  for (unsigned c = 0; c < links_.size(); c++) {
    if (DL >= 2) {
      cout << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    SpLink* link = *it;
    if (link->getResidual() < BpOptions::accuracy) {
      return;
    }
    updateMessage (link);
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FgFacSet& factorNeighbors = link->getVariable()->neighbors();
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->getFactor()) {
        const SpLinkSet& links = ninf(factorNeighbors[i])->getLinks();
        for (unsigned j = 0; j < links.size(); j++) {
          if (links[j]->getVariable() != link->getVariable()) {
            calculateMessage (links[j]);
            SpLinkMap::iterator iter = linkMap_.find (links[j]);
            sortedOrder_.erase (iter->second);
            iter->second = sortedOrder_.insert (links[j]);
          }
        }
      }
    }
    if (DL >= 2) {
      cout << "----------------------------------------" ;
      cout << "----------------------------------------" << endl;
    }
  }
}



void
FgBpSolver::calculateFactor2VariableMsg (SpLink* link) const
{
  const FgFacNode* src = link->getFactor();
  const FgVarNode* dst = link->getVariable();
  const SpLinkSet& links = ninf(src)->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned msgSize = 1;
  for (unsigned i = 0; i < links.size(); i++) {
    msgSize *= links[i]->getVariable()->nrStates();
  }
  unsigned repetitions = 1;
  Params msgProduct (msgSize, Util::multIdenty());
  if (Globals::logDomain) {
    for (int i = links.size() - 1; i >= 0; i--) {
      if (links[i]->getVariable() != dst) {
        Util::add (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->nrStates();
      } else {
        unsigned ds = links[i]->getVariable()->nrStates();
        Util::add (msgProduct, Params (ds, 1.0), repetitions);
        repetitions *= ds;
      }
    }
  } else {
    for (int i = links.size() - 1; i >= 0; i--) {
      if (links[i]->getVariable() != dst) {
        if (DL >= 5) {
          cout << "    message from " << links[i]->getVariable()->label();
          cout << ": " << endl;
        }
        Util::multiply (msgProduct, getVar2FactorMsg (links[i]), repetitions);
        repetitions *= links[i]->getVariable()->nrStates();
      } else {
        unsigned ds = links[i]->getVariable()->nrStates();
        Util::multiply (msgProduct, Params (ds, 1.0), repetitions);
        repetitions *= ds;
      }
    }
  }

  Factor result (src->factor()->getVarIds(),
                 src->factor()->getRanges(),
                 msgProduct);
  result.multiply (*(src->factor()));
  if (DL >= 5) {
    cout << "    message product:  " ;
    cout << Util::parametersToString (msgProduct) << endl;
    cout << "    original factor:  " ;
    cout << Util::parametersToString (src->getParameters()) << endl;
    cout << "    factor product:   " ;
    cout << Util::parametersToString (result.getParameters()) << endl;
  }
  result.sumOutAllExcept (dst->varId());
  if (DL >= 5) {
    cout << "    marginalized:     " ;
    cout << Util::parametersToString (result.getParameters()) << endl;
  }
  const Params& resultParams = result.getParameters();
  Params& message = link->getNextMessage();
  for (unsigned i = 0; i < resultParams.size(); i++) {
     message[i] = resultParams[i];
  }
  Util::normalize (message);
  if (DL >= 5) {
    cout << "    curr msg:         " ;
    cout << Util::parametersToString (link->getMessage()) << endl;
    cout << "    next msg:         " ;
    cout << Util::parametersToString (message) << endl;
  }
}



Params
FgBpSolver::getVar2FactorMsg (const SpLink* link) const
{
  const FgVarNode* src = link->getVariable();
  const FgFacNode* dst = link->getFactor();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->nrStates(), Util::noEvidence());
    msg[src->getEvidence()] = Util::withEvidence();
    if (DL >= 5) {
      cout << Util::parametersToString (msg);
    }
  } else {
    msg.resize (src->nrStates(), Util::one());
  }
  if (DL >= 5) {
    cout << Util::parametersToString (msg);
  }
  const SpLinkSet& links = ninf (src)->getLinks();
  if (Globals::logDomain) {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        Util::add (msg, links[i]->getMessage());
      }
    }
  } else {
    for (unsigned i = 0; i < links.size(); i++) {
      if (links[i]->getFactor() != dst) {
        Util::multiply (msg, links[i]->getMessage());
        if (DL >= 5) {
          cout << " x " << Util::parametersToString (links[i]->getMessage());
        }
      }
    }
  }
  if (DL >= 5) {
    cout << " = " << Util::parametersToString (msg);
  }
  return msg;
}



Params
FgBpSolver::getJointByConditioning (const VarIds& jointVarIds) const
{
  FgVarSet jointVars;
  for (unsigned i = 0; i < jointVarIds.size(); i++) {
    assert (factorGraph_->getFgVarNode (jointVarIds[i]));
    jointVars.push_back (factorGraph_->getFgVarNode (jointVarIds[i]));
  }

  FactorGraph* fg = new FactorGraph (*factorGraph_);
  FgBpSolver solver (*fg);
  solver.runSolver();
  Params prevBeliefs = solver.getPosterioriOf (jointVarIds[0]);

  VarIds observedVids = {jointVars[0]->varId()};

  for (unsigned i = 1; i < jointVarIds.size(); i++) {
    assert (jointVars[i]->hasEvidence() == false);
    Params newBeliefs;
    VarNodes observedVars;
    for (unsigned j = 0; j < observedVids.size(); j++) {
      observedVars.push_back (fg->getFgVarNode (observedVids[j]));
    }
    StatesIndexer idx (observedVars, false);
    while (idx.valid()) {
      for (unsigned j = 0; j < observedVars.size(); j++) {
        observedVars[j]->setEvidence (idx[j]);
      }
      ++ idx;
      FgBpSolver solver (*fg);
      solver.runSolver();
      Params beliefs = solver.getPosterioriOf (jointVarIds[i]);
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
    observedVids.push_back (jointVars[i]->varId());
  }
  return prevBeliefs;
}



void
FgBpSolver::printLinkInformation (void) const
{
  for (unsigned i = 0; i < links_.size(); i++) {
    SpLink* l = links_[i]; 
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << Util::parametersToString (l->getMessage()) << endl;
    cout << "    next msg = " ;
    cout << Util::parametersToString (l->getNextMessage()) << endl;
    cout << "    residual = " << l->getResidual() << endl;
  }
}

