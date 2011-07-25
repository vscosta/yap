#include <cassert>
#include <algorithm>
#include <iostream>

#include "SPSolver.h"
#include "FactorGraph.h"
#include "FgVarNode.h"
#include "Factor.h"

SPSolver* Link::klass = 0;


SPSolver::SPSolver (const FactorGraph& fg) : Solver (&fg)
{
  fg_           = &fg;
  accuracy_     = 0.0001;
  maxIter_      = 10000;
  //schedule_     = S_SEQ_FIXED;
  //schedule_     = S_SEQ_RANDOM;
  //schedule_     = S_SEQ_PARALLEL;
  schedule_     = S_MAX_RESIDUAL;
  Link::klass   = this;
  FgVarSet vars = fg_->getFgVarNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    msgs_.push_back (new MessageBanket (vars[i]));
  }
}



SPSolver::~SPSolver (void)
{
  for (unsigned i = 0; i < msgs_.size(); i++) {
    delete msgs_[i];
  }
}



void
SPSolver::runSolver (void)
{
  nIter_ = 0;
  vector<Factor*> factors = fg_->getFactors();
  for (unsigned i = 0; i < factors.size(); i++) {
    FgVarSet neighbors = factors[i]->getFgVarNodes();
    for (unsigned j = 0; j < neighbors.size(); j++) {
      updateOrder_.push_back (Link (factors[i], neighbors[j]));
    }
  }
  
  while (!converged() && nIter_ < maxIter_) {
    if (DL >= 1) {
      cout << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
      cout << " Iteration " << nIter_ + 1 << endl;
      cout << "****************************************" ;
      cout << "****************************************" ;
      cout << endl;
    }

    switch (schedule_) {

      case S_SEQ_RANDOM:
        random_shuffle (updateOrder_.begin(), updateOrder_.end());
        // no break

      case S_SEQ_FIXED:
        for (unsigned c = 0; c < updateOrder_.size(); c++) {
          Link& link = updateOrder_[c];
          calculateNextMessage (link.source, link.destination);
          updateMessage (updateOrder_[c]);
        }
        break;

      case S_PARALLEL:
        for (unsigned c = 0; c < updateOrder_.size(); c++) {
          Link link = updateOrder_[c];
          calculateNextMessage (link.source, link.destination);
        }
        for (unsigned c = 0; c < updateOrder_.size(); c++) {
          Link link = updateOrder_[c];
          updateMessage (updateOrder_[c]);
        }
        break;

      case S_MAX_RESIDUAL:
        maxResidualSchedule();
        break;
    }

    nIter_++;
  }
  cout << endl;
  if (DL >= 1) {
    if (nIter_ < maxIter_) {
      cout << "Loopy Sum-Product converged in " ; 
      cout << nIter_ << " iterations" << endl;
    } else {
      cout << "The maximum number of iterations was hit, terminating..." ;
      cout << endl;
    }
  }
}



ParamSet
SPSolver::getPosterioriOf (const Variable* var) const
{
  assert (var);
  assert (var == fg_->getVariableById (var->getVarId()));
  assert (var->getIndex() < msgs_.size());

  ParamSet probs (var->getDomainSize(), 1);
  if (var->hasEvidence()) {
    for (unsigned i = 0; i < probs.size(); i++) {
      if ((int)i != var->getEvidence()) {
        probs[i] = 0;
      }
    }

  } else {

    MessageBanket* mb = msgs_[var->getIndex()]; 
    const FgVarNode* varNode  = fg_->getFgVarNodes()[var->getIndex()];
    vector<Factor*> neighbors = varNode->getFactors();
    for (unsigned i = 0; i < neighbors.size(); i++) {
      const Message& msg = mb->getMessage (neighbors[i]);
      for (unsigned j = 0; j < msg.size(); j++) {
        probs[j] *= msg[j];
      }
    }
    Util::normalize (probs);
  }

  return probs;
}



bool
SPSolver::converged (void)
{
  if (nIter_ == 0 || nIter_ == 1) {
    return false;
  }
  bool converged = true;
  for (unsigned i = 0; i < updateOrder_.size(); i++) {
    double residual = getResidual (updateOrder_[i]);
    if (DL >= 1) {
      cout << updateOrder_[i].toString();
      cout << " residual = " << residual << endl;
    }
    if (residual > accuracy_) {
      converged = false;
      if (DL == 0) {
        break;
      }
    } 
  }
  return converged;
}



void
SPSolver::maxResidualSchedule (void)
{
  if (nIter_ == 0) {
    for (unsigned c = 0; c < updateOrder_.size(); c++) {
      Link& l = updateOrder_[c];
      calculateNextMessage (l.source, l.destination);
      if (DL >= 1) {
        cout << updateOrder_[c].toString() << " residual = " ;
        cout << getResidual (updateOrder_[c]) << endl;
      }
    }
    sort (updateOrder_.begin(), updateOrder_.end(), compareResidual);
  } else {

    for (unsigned c = 0; c < updateOrder_.size(); c++) {
      Link& link = updateOrder_.front();
      updateMessage (link); 
      resetResidual (link);

      // update the messages that depend on message source --> destination
      vector<Factor*> fstLevelNeighbors = link.destination->getFactors();
      for (unsigned i = 0; i < fstLevelNeighbors.size(); i++) {
        if (fstLevelNeighbors[i] != link.source) {
          FgVarSet sndLevelNeighbors;
          sndLevelNeighbors = fstLevelNeighbors[i]->getFgVarNodes();
          for (unsigned j = 0; j < sndLevelNeighbors.size(); j++) {
            if (sndLevelNeighbors[j] != link.destination) {
              calculateNextMessage (fstLevelNeighbors[i], sndLevelNeighbors[j]);
            }
          }
        }
      }  
      sort (updateOrder_.begin(), updateOrder_.end(), compareResidual);
    }
  }
}



void
SPSolver::updateMessage (const Link& link)
{
  updateMessage (link.source, link.destination);
}



void
SPSolver::updateMessage (const Factor* src, const FgVarNode* dest)
{
  msgs_[dest->getIndex()]->updateMessage (src);
/*  cout << src->getLabel() << " --> " << dest->getLabel() << endl; 
  cout << "    m: " ;
  Message msg = msgs_[dest->getIndex()]->getMessage (src);
  for (unsigned i = 0; i < msg.size(); i++) {
    if (i != 0) cout << ", " ;
    cout << msg[i];
  }
  cout << endl;
*/
}



void
SPSolver::calculateNextMessage (const Link& link)
{
  calculateNextMessage (link.source, link.destination);
}


void
SPSolver::calculateNextMessage (const Factor* src, const FgVarNode* dest)
{
  FgVarSet neighbors = src->getFgVarNodes(); 
  // calculate the product of MessageBankets sended
  // to factor `src', except from var `dest'
  Factor result = *src;
  for (unsigned i = 0; i < neighbors.size(); i++) {
    if (neighbors[i] != dest) {
      Message msg (neighbors[i]->getDomainSize(), 1);
      calculateVarFactorMessage (neighbors[i], src, msg);
      result *= Factor (neighbors[i], msg);
    }
  }
  // marginalize all vars except `dest'
  for (unsigned i = 0; i < neighbors.size(); i++) {
    if (neighbors[i] != dest) {
      result.marginalizeVariable (neighbors[i]);
    }
  }
  msgs_[dest->getIndex()]->setNextMessage (src, result.getParameters());
}



void
SPSolver::calculateVarFactorMessage (const FgVarNode* src,
                                     const Factor* dest,
                                     Message& placeholder) const
{
  assert (src->getDomainSize() == (int)placeholder.size());
  if (src->hasEvidence()) {
    for (unsigned i = 0; i < placeholder.size(); i++) {
      if ((int)i != src->getEvidence()) {
        placeholder[i] = 0.0;
      } else {
        placeholder[i] = 1.0;
      }
    }

  } else {

    MessageBanket* mb = msgs_[src->getIndex()];
    vector<Factor*> neighbors = src->getFactors();
    for (unsigned i = 0; i < neighbors.size(); i++) {
      if (neighbors[i] != dest) {
        const Message& fromFactor = mb->getMessage (neighbors[i]);
        for (unsigned j = 0; j < fromFactor.size(); j++) {
          placeholder[j] *= fromFactor[j];
        }
      }
    }
  }
}

