#include <cassert>
#include <limits>

#include <iostream>

#include "SPSolver.h"
#include "FactorGraph.h"
#include "FgVarNode.h"
#include "Factor.h"
#include "Shared.h"


SPSolver::SPSolver (FactorGraph& fg) : Solver (&fg)
{
  fg_ = &fg;
}



SPSolver::~SPSolver (void)
{
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  for (unsigned i = 0; i < factorsI_.size(); i++) {
    delete factorsI_[i];
  }
  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
}



void
SPSolver::runTreeSolver (void)
{
  CFactorSet factors = fg_->getFactors();
  bool finish = false;
  while (!finish) {
    finish = true;
    for (unsigned i = 0; i < factors.size(); i++) {
      CLinkSet links = factorsI_[factors[i]->getIndex()]->getLinks();
      for (unsigned j = 0; j < links.size(); j++) {
        if (!links[j]->messageWasSended()) {
          if (readyToSendMessage(links[j])) {
            links[j]->setNextMessage (getFactor2VarMsg (links[j]));
            links[j]->updateMessage();
          }
          finish = false;
        }
      }
    }
  }
}



bool
SPSolver::readyToSendMessage (const Link* link) const
{
  CFgVarSet factorVars = link->getFactor()->getFgVarNodes();
  for (unsigned i = 0; i < factorVars.size(); i++) {
    if (factorVars[i] != link->getVariable()) {
      CLinkSet links = varsI_[factorVars[i]->getIndex()]->getLinks();
      for (unsigned j = 0; j < links.size(); j++) {
        if (links[j]->getFactor() != link->getFactor() &&
            !links[j]->messageWasSended()) {
          return false;
        }
      }
    }
  }
  return true;
}



void
SPSolver::runSolver (void)
{
  initializeSolver();
  runTreeSolver();
  return;
  nIter_ = 0;
  while (!converged() && nIter_ < SolverOptions::maxIter) {

    nIter_ ++;
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
          links_[i]->setNextMessage (getFactor2VarMsg (links_[i]));
          links_[i]->updateMessage();
        }
        break;

      case SolverOptions::S_PARALLEL:
        for (unsigned i = 0; i < links_.size(); i++) {
          links_[i]->setNextMessage (getFactor2VarMsg (links_[i]));
        }
        for (unsigned i = 0; i < links_.size(); i++) {
          links_[i]->updateMessage();
        }
        break;

      case SolverOptions::S_MAX_RESIDUAL:
        maxResidualSchedule();
        break;
    }
  }

  if (DL >= 2) {
    cout << endl;
    if (nIter_ < SolverOptions::maxIter) {
      cout << "Loopy Sum-Product converged in " ; 
      cout << nIter_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
  }
}



ParamSet
SPSolver::getPosterioriOf (Vid vid) const
{
  assert (fg_->getFgVarNode (vid));
  FgVarNode* var = fg_->getFgVarNode (vid);
  ParamSet probs;

  if (var->hasEvidence()) {
    probs.resize (var->getDomainSize(), 0.0);
    probs[var->getEvidence()] = 1.0;
  } else {
    probs.resize (var->getDomainSize(), 1.0);
    CLinkSet links = varsI_[var->getIndex()]->getLinks();
    for (unsigned i = 0; i < links.size(); i++) {
      CParamSet msg = links[i]->getMessage();
      for (unsigned j = 0; j < msg.size(); j++) {
        probs[j] *= msg[j];
      }
    }
    Util::normalize (probs);
  }
  return probs;
}



ParamSet
SPSolver::getJointDistributionOf (const VidSet& jointVids)
{
  FgVarSet jointVars;
  unsigned dsize = 1;
  for (unsigned i = 0; i < jointVids.size(); i++) {
    FgVarNode* varNode = fg_->getFgVarNode (jointVids[i]);
    dsize *= varNode->getDomainSize();
    jointVars.push_back (varNode);
  }

  unsigned maxVid = std::numeric_limits<unsigned>::max();
  FgVarNode* junctionVar = new FgVarNode (maxVid, dsize);
  FgVarSet factorVars = { junctionVar };
  for (unsigned i = 0; i < jointVars.size(); i++) {
    factorVars.push_back (jointVars[i]);
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

  Distribution* dist = new Distribution (params, maxVid);
  Factor* newFactor = new Factor (factorVars, dist);
  fg_->addVariable (junctionVar);
  fg_->addFactor (newFactor);

  runSolver();
  ParamSet results = getPosterioriOf (maxVid);
  deleteJunction (newFactor, junctionVar);

  return results;
}



void
SPSolver::initializeSolver (void)
{
  fg_->setIndexes();

  CFgVarSet vars = fg_->getFgVarNodes();
  for (unsigned i = 0; i < varsI_.size(); i++) {
    delete varsI_[i];
  }
  varsI_.reserve (vars.size());
  for (unsigned i = 0; i < vars.size(); i++) {
    varsI_.push_back (new SPNodeInfo());
  }

  CFactorSet factors = fg_->getFactors();
  for (unsigned i = 0; i < factorsI_.size(); i++) {
    delete factorsI_[i];
  }
  factorsI_.reserve (factors.size());
  for (unsigned i = 0; i < factors.size(); i++) {
    factorsI_.push_back (new SPNodeInfo());
  }

  for (unsigned i = 0; i < links_.size(); i++) {
    delete links_[i];	
  }
  createLinks();

  for (unsigned i = 0; i < links_.size(); i++) {
    Factor* source  = links_[i]->getFactor();
    FgVarNode* dest = links_[i]->getVariable();
    varsI_[dest->getIndex()]->addLink (links_[i]);
    factorsI_[source->getIndex()]->addLink (links_[i]);
  }
}



void
SPSolver::createLinks (void)
{
  CFactorSet factors = fg_->getFactors();
  for (unsigned i = 0; i < factors.size(); i++) {
    CFgVarSet neighbors = factors[i]->getFgVarNodes();
    for (unsigned j = 0; j < neighbors.size(); j++) {
      links_.push_back (new Link (factors[i], neighbors[j]));
    }
  }
}



void
SPSolver::deleteJunction (Factor* f, FgVarNode* v)
{
  fg_->removeFactor (f);
  f->freeDistribution();
  delete f;
  fg_->removeVariable (v);
  delete v;
}



bool
SPSolver::converged (void)
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
    for (unsigned i = 0; i < links_.size(); i++) {
      double residual = links_[i]->getResidual();
      if (DL >= 2) {
        cout << links_[i]->toString() + " residual = " << residual << endl;
      }
      if (residual > SolverOptions::accuracy) {
        converged = false;
        if (DL == 0) break;
      }
    }
  }
  return converged;
}



void
SPSolver::maxResidualSchedule (void)
{
  if (nIter_ == 1) {
    for (unsigned i = 0; i < links_.size(); i++) {
      links_[i]->setNextMessage (getFactor2VarMsg (links_[i]));
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (DL >= 2 && DL < 5) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (unsigned c = 0; c < links_.size(); c++) {
    if (DL >= 2) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); it ++) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->getResidual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    Link* link = *it;
    if (DL >= 2) {
      cout << "updating " << (*sortedOrder_.begin())->toString() << endl;
    }
    if (link->getResidual() < SolverOptions::accuracy) {
      return;
    }
    link->updateMessage();
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    CFactorSet factorNeighbors = link->getVariable()->getFactors();
    for (unsigned i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->getFactor()) {
        CLinkSet links = factorsI_[factorNeighbors[i]->getIndex()]->getLinks();
        for (unsigned j = 0; j < links.size(); j++) {
          if (links[j]->getVariable() != link->getVariable()) {
            if (DL >= 2 && DL < 5) {
              cout << "    calculating " << links[j]->toString() << endl;
            }
            links[j]->setNextMessage (getFactor2VarMsg (links[j]));
            LinkMap::iterator iter = linkMap_.find (links[j]);
            sortedOrder_.erase (iter->second);
            iter->second = sortedOrder_.insert (links[j]);
          }
        }
      }
    }
  }
}



ParamSet
SPSolver::getFactor2VarMsg (const Link* link) const
{
  const Factor* src     = link->getFactor();
  const FgVarNode* dest = link->getVariable();
  CFgVarSet neighbors = src->getFgVarNodes();
  CLinkSet links = factorsI_[src->getIndex()]->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dest'
  Factor result (*src);
  Factor temp;
  if (DL >= 5) {
    cout << "calculating " ;
    cout << src->getLabel() << " --> " << dest->getLabel();
    cout << endl;
  }
  for (unsigned i = 0; i < neighbors.size(); i++) {
    if (links[i]->getVariable() != dest) {
      if (DL >= 5) {
        cout << "    message from " << links[i]->getVariable()->getLabel();
        cout << ":  " ;
        ParamSet p = getVar2FactorMsg (links[i]);
        cout << endl;
        Factor temp2 (links[i]->getVariable(), p);
        temp.multiplyByFactor (temp2);
        temp2.freeDistribution();
      } else {
        Factor temp2 (links[i]->getVariable(), getVar2FactorMsg (links[i]));
        temp.multiplyByFactor (temp2);
        temp2.freeDistribution();
      }
    }
  }
  if (links.size() >= 2) {
    result.multiplyByFactor (temp, &(src->getCptEntries()));
    if (DL >= 5) {
      cout << "    message product: " ;
      cout << Util::parametersToString (temp.getParameters()) << endl;
      cout << "    factor product:  " ;
      cout << Util::parametersToString (src->getParameters());
      cout << " x " ;
      cout << Util::parametersToString (temp.getParameters());
      cout << " = " ;
      cout << Util::parametersToString (result.getParameters()) << endl;
    }
    temp.freeDistribution();
  }

  for (unsigned i = 0; i < links.size(); i++) {
    if (links[i]->getVariable() != dest) {
      result.removeVariable (links[i]->getVariable());
    }
  }
  if (DL >= 5) {
    cout << "    final message:   " ;
    cout << Util::parametersToString (result.getParameters()) << endl << endl;
  }
  ParamSet msg = result.getParameters();
  result.freeDistribution();
  return msg;
}



ParamSet
SPSolver::getVar2FactorMsg (const Link* link) const
{
  const FgVarNode* src = link->getVariable();
  const Factor* dest   = link->getFactor();
  ParamSet msg;
  if (src->hasEvidence()) {
    msg.resize (src->getDomainSize(), 0.0);
    msg[src->getEvidence()] = 1.0;
    if (DL >= 5) {
      cout << Util::parametersToString (msg);
    }
  } else {
    msg.resize (src->getDomainSize(), 1.0);
  }
  if (DL >= 5) {
    cout << Util::parametersToString (msg);
  }
  CLinkSet links = varsI_[src->getIndex()]->getLinks();
  for (unsigned i = 0; i < links.size(); i++) {
    if (links[i]->getFactor() != dest) {
      CParamSet msgFromFactor = links[i]->getMessage();
      for (unsigned j = 0; j < msgFromFactor.size(); j++) {
        msg[j] *= msgFromFactor[j];
      }
      if (DL >= 5) {
        cout << " x " << Util::parametersToString (msgFromFactor);
      }
    }
  }
  if (DL >= 5) {
    cout << " = " << Util::parametersToString (msg);
  }
  return msg;
}

