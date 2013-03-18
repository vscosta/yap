#include <cassert>

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>

#include "BeliefProp.h"
#include "Indexer.h"
#include "Horus.h"


namespace Horus {

double    BeliefProp::accuracy_ = 0.0001;
unsigned  BeliefProp::maxIter_  = 1000;

BeliefProp::MsgSchedule BeliefProp::schedule_ =
    MsgSchedule::seqFixedSch;



BeliefProp::BeliefProp (const FactorGraph& fg)
    : GroundSolver (fg), nIters_(0), runned_(false)
{

}



BeliefProp::~BeliefProp()
{
  for (size_t i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
  links_.clear();
}



Params
BeliefProp::solveQuery (VarIds queryVids)
{
  assert (queryVids.empty() == false);
  return queryVids.size() == 1
      ? getPosterioriOf (queryVids[0])
      : getJointDistributionOf (queryVids);
}



void
BeliefProp::printSolverFlags() const
{
  std::stringstream ss;
  ss << "belief propagation [" ;
  ss << "bp_msg_schedule=" ;
  switch (schedule_) {
    case MsgSchedule::seqFixedSch:    ss << "seq_fixed";    break;
    case MsgSchedule::seqRandomSch:   ss << "seq_random";   break;
    case MsgSchedule::parallelSch:    ss << "parallel";     break;
    case MsgSchedule::maxResidualSch: ss << "max_residual"; break;
  }
  ss << ",bp_max_iter=" << Util::toString (maxIter_);
  ss << ",bp_accuracy=" << Util::toString (accuracy_);
  ss << ",log_domain="  << Util::toString (Globals::logDomain);
  ss << "]" ;
  std::cout << ss.str() << std::endl;
}



Params
BeliefProp::getPosterioriOf (VarId vid)
{
  if (runned_ == false) {
    runSolver();
  }
  assert (fg.getVarNode (vid));
  VarNode* var = fg.getVarNode (vid);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const BpLinks& links = getLinks (var);
    if (Globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        probs += links[i]->message();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        probs *= links[i]->message();
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



Params
BeliefProp::getJointDistributionOf (const VarIds& jointVarIds)
{
  if (runned_ == false) {
    runSolver();
  }
  VarNode* vn = fg.getVarNode (jointVarIds[0]);
  const FacNodes& facNodes = vn->neighbors();
  size_t idx = facNodes.size();
  for (size_t i = 0; i < facNodes.size(); i++) {
    if (facNodes[i]->factor().contains (jointVarIds)) {
      idx = i;
      break;
    }
  }
  if (idx == facNodes.size()) {
    return getJointByConditioning (jointVarIds);
  }
  return getFactorJoint (facNodes[idx], jointVarIds);
}



Params
BeliefProp::getFactorJoint (
    FacNode* fn,
    const VarIds& jointVarIds)
{
  if (runned_ == false) {
    runSolver();
  }
  Factor res (fn->factor());
  const BpLinks& links = getLinks( fn);
  for (size_t i = 0; i < links.size(); i++) {
    Factor msg ({links[i]->varNode()->varId()},
                {links[i]->varNode()->range()},
                getVarToFactorMsg (links[i]));
    res.multiply (msg);
  }
  res.sumOutAllExcept (jointVarIds);
  res.reorderArguments (jointVarIds);
  res.normalize();
  Params jointDist = res.params();
  if (Globals::logDomain) {
    Util::exp (jointDist);
  }
  return jointDist;
}



BeliefProp::BpLink::BpLink (FacNode* fn, VarNode* vn)
{
  fac_ = fn;
  var_ = vn;
  v1_.resize (vn->range(), LogAware::log (1.0 / vn->range()));
  v2_.resize (vn->range(), LogAware::log (1.0 / vn->range()));
  currMsg_   = &v1_;
  nextMsg_   = &v2_;
  residual_  = 0.0;
}



void
BeliefProp::BpLink::clearResidual()
{
  residual_ = 0.0;
}



void
BeliefProp::BpLink::updateResidual()
{
  residual_ = LogAware::getMaxNorm (v1_, v2_);
}



void
BeliefProp::BpLink::updateMessage()
{
  swap (currMsg_, nextMsg_);
}



std::string
BeliefProp::BpLink::toString() const
{
  std::stringstream ss;
  ss << fac_->getLabel();
  ss << " -- " ;
  ss << var_->label();
  return ss.str();
}



void
BeliefProp::calculateAndUpdateMessage (BpLink* link, bool calcResidual)
{
  if (Globals::verbosity > 2) {
    std::cout << "calculating & updating " << link->toString();
    std::cout << std::endl;
  }
  calcFactorToVarMsg (link);
  if (calcResidual) {
    link->updateResidual();
  }
  link->updateMessage();
}



void
BeliefProp::calculateMessage (BpLink* link, bool calcResidual)
{
  if (Globals::verbosity > 2) {
    std::cout << "calculating " << link->toString();
    std::cout << std::endl;
  }
  calcFactorToVarMsg (link);
  if (calcResidual) {
    link->updateResidual();
  }
}



void
BeliefProp::updateMessage (BpLink* link)
{
  link->updateMessage();
  if (Globals::verbosity > 2) {
    std::cout << "updating " << link->toString();
    std::cout << std::endl;
  }
}



void
BeliefProp::runSolver()
{
  initializeSolver();
  nIters_ = 0;
  while (!converged() && nIters_ < maxIter_) {
    nIters_ ++;
    if (Globals::verbosity > 1) {
      Util::printHeader (std::string ("Iteration ")
          + Util::toString (nIters_));
    }
    switch (schedule_) {
     case MsgSchedule::seqRandomSch:
       std::random_shuffle (links_.begin(), links_.end());
       // no break
      case MsgSchedule::seqFixedSch:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateAndUpdateMessage (links_[i]);
        }
        break;
      case MsgSchedule::parallelSch:
        for (size_t i = 0; i < links_.size(); i++) {
          calculateMessage (links_[i]);
        }
        for (size_t i = 0; i < links_.size(); i++) {
          updateMessage(links_[i]);
        }
        break;
      case MsgSchedule::maxResidualSch:
        maxResidualSchedule();
        break;
    }
  }
  if (Globals::verbosity > 0) {
    if (nIters_ < maxIter_) {
      std::cout << "Belief propagation converged in " ;
      std::cout << nIters_ << " iterations" << std::endl;
    } else {
      std::cout << "The maximum number of iterations was hit," ;
      std::cout << " terminating..." ;
      std::cout << std::endl;
    }
    std::cout << std::endl;
  }
  runned_ = true;
}



void
BeliefProp::createLinks()
{
  const FacNodes& facNodes = fg.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighbors = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighbors.size(); j++) {
      links_.push_back (new BpLink (facNodes[i], neighbors[j]));
    }
  }
}



void
BeliefProp::maxResidualSchedule()
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      std::cout << "current residuals:" << std::endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); ++it) {
        std::cout << "    " << std::setw (30) << std::left;
        std::cout << (*it)->toString();
        std::cout << "residual = " << (*it)->residual();
        std::cout << std::endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    BpLink* link = *it;
    if (link->residual() < accuracy_) {
      return;
    }
    updateMessage (link);
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->varNode()->neighbors();
    for (size_t i = 0; i < factorNeighbors.size(); i++) {
      if (factorNeighbors[i] != link->facNode()) {
        const BpLinks& links = getLinks (factorNeighbors[i]);
        for (size_t j = 0; j < links.size(); j++) {
          if (links[j]->varNode() != link->varNode()) {
            calculateMessage (links[j]);
            BpLinkMap::iterator iter = linkMap_.find (links[j]);
            sortedOrder_.erase (iter->second);
            iter->second = sortedOrder_.insert (links[j]);
          }
        }
      }
    }
    if (Globals::verbosity > 1) {
      Util::printDashedLine();
    }
  }
}



void
BeliefProp::calcFactorToVarMsg (BpLink* link)
{
  FacNode* src = link->facNode();
  const VarNode* dst = link->varNode();
  const BpLinks& links = getLinks (src);
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned reps    = 1;
  unsigned msgSize = Util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->varNode() != dst) {
        if (Constants::showBpCalcs) {
          std::cout << "    message from " << links[i]->varNode()->label();
          std::cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::plus<double>());
        if (Constants::showBpCalcs) {
          std::cout << std::endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  } else {
    for (size_t i = links.size(); i-- > 0; ) {
      if (links[i]->varNode() != dst) {
        if (Constants::showBpCalcs) {
          std::cout << "    message from " << links[i]->varNode()->label();
          std::cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::multiplies<double>());
        if (Constants::showBpCalcs) {
          std::cout << std::endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  }
  Factor result (src->factor().arguments(),
      src->factor().ranges(), msgProduct);
  result.multiply (src->factor());
  if (Constants::showBpCalcs) {
    std::cout << "    message product:  " << msgProduct << std::endl;
    std::cout << "    original factor:  " << src->factor().params();
    std::cout << std::endl;
    std::cout << "    factor product:   " << result.params() << std::endl;
  }
  result.sumOutAllExcept (dst->varId());
  if (Constants::showBpCalcs) {
    std::cout << "    marginalized:     " << result.params() << std::endl;
  }
  link->nextMessage() = result.params();
  LogAware::normalize (link->nextMessage());
  if (Constants::showBpCalcs) {
    std::cout << "    curr msg:         " << link->message() << std::endl;
    std::cout << "    next msg:         " << link->nextMessage() << std::endl;
  }
}



Params
BeliefProp::getVarToFactorMsg (const BpLink* link)
{
  const VarNode* src = link->varNode();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), LogAware::noEvidence());
    msg[src->getEvidence()] = LogAware::withEvidence();
  } else {
    msg.resize (src->range(), LogAware::one());
  }
  if (Constants::showBpCalcs) {
    std::cout << msg;
  }
  BpLinks::const_iterator it;
  const BpLinks& links = getLinks (src);
  if (Globals::logDomain) {
    for (it = links.begin(); it != links.end(); ++it) {
      if (*it != link) {
        msg += (*it)->message();
      }
      if (Constants::showBpCalcs) {
        std::cout << " x " << (*it)->message();
      }
    }
  } else {
    for (it = links.begin(); it != links.end(); ++it) {
      if (*it != link) {
        msg *= (*it)->message();
      }
      if (Constants::showBpCalcs) {
        std::cout << " x " << (*it)->message();
      }
    }
  }
  if (Constants::showBpCalcs) {
    std::cout << " = " << msg;
  }
  return msg;
}



Params
BeliefProp::getJointByConditioning (const VarIds& jointVarIds) const
{
  return GroundSolver::getJointByConditioning (
      GroundSolverType::bpSolver, fg, jointVarIds);
}



void
BeliefProp::initializeSolver()
{
  const VarNodes& varNodes = fg.varNodes();
  varsLinks_.reserve (varNodes.size());
  for (size_t i = 0; i < varNodes.size(); i++) {
    varsLinks_.push_back (BpLinks());
  }
  const FacNodes& facNodes = fg.facNodes();
  facsLinks_.reserve (facNodes.size());
  for (size_t i = 0; i < facNodes.size(); i++) {
    facsLinks_.push_back (BpLinks());
  }
  createLinks();
  for (size_t i = 0; i < links_.size(); i++) {
    FacNode* src = links_[i]->facNode();
    VarNode* dst = links_[i]->varNode();
    getLinks (dst).push_back (links_[i]);
    getLinks (src).push_back (links_[i]);
  }
}



bool
BeliefProp::converged()
{
  if (links_.empty()) {
    return true;
  }
  if (nIters_ == 0) {
    return false;
  }
  if (Globals::verbosity > 2) {
    std::cout << std::endl;
  }
  if (nIters_ == 1) {
    if (Globals::verbosity > 1) {
      std::cout << "no residuals" << std::endl << std::endl;
    }
    return false;
  }
  bool converged = true;
  if (schedule_ == MsgSchedule::maxResidualSch) {
    double maxResidual = (*(sortedOrder_.begin()))->residual();
    if (maxResidual > accuracy_) {
      converged = false;
    } else {
      converged = true;
    }
  } else {
    for (size_t i = 0; i < links_.size(); i++) {
      double residual = links_[i]->residual();
      if (Globals::verbosity > 1) {
        std::cout << links_[i]->toString() + " residual = " << residual;
        std::cout << std::endl;
      }
      if (residual > accuracy_) {
        converged = false;
        if (Globals::verbosity < 2) {
          break;
        }
      }
    }
    if (Globals::verbosity > 1) {
      std::cout << std::endl;
    }
  }
  return converged;
}



void
BeliefProp::printLinkInformation() const
{
  using std::cout;
  using std::endl;
  for (size_t i = 0; i < links_.size(); i++) {
    BpLink* l = links_[i];
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " ;
    cout << l->message() << endl;
    cout << "    next msg = " ;
    cout << l->nextMessage() << endl;
    cout << "    residual = " << l->residual() << endl;
  }
}

}  // namespace Horus

