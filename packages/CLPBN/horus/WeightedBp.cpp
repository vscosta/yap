#include <cassert>

#include <iostream>
#include <iomanip>

#include "WeightedBp.h"


namespace Horus {

WeightedBp::WeightedBp (
    const FactorGraph& fg,
    const std::vector<std::vector<unsigned>>& weights)
      : BeliefProp (fg), weights_(weights)
{

}



WeightedBp::~WeightedBp()
{
  for (size_t i = 0; i < links_.size(); i++) {
    delete links_[i];
  }
  links_.clear();
}



Params
WeightedBp::getPosterioriOf (VarId vid)
{
  if (runned_ == false) {
    runSolver();
  }
  VarNode* var = fg.getVarNode (vid);
  assert (var);
  Params probs;
  if (var->hasEvidence()) {
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const BpLinks& links = getLinks (var);
    if (Globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        WeightedLink* l = static_cast<WeightedLink*> (links[i]);
        probs += l->powMessage();
      }
      LogAware::normalize (probs);
      Util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        WeightedLink* l = static_cast<WeightedLink*> (links[i]);
        probs *= l->powMessage();
      }
      LogAware::normalize (probs);
    }
  }
  return probs;
}



WeightedBp::WeightedLink::WeightedLink (
    FacNode* fn,
    VarNode* vn,
    size_t idx,
    unsigned weight)
      : BpLink (fn, vn), index_(idx), weight_(weight),
        pwdMsg_(vn->range(), LogAware::one())
{

}



void
WeightedBp::createLinks()
{
  using std::cout;
  using std::endl;
  if (Globals::verbosity > 0) {
    cout << "compressed factor graph contains " ;
    cout << fg.nrVarNodes() << " variables and " ;
    cout << fg.nrFacNodes() << " factors " << endl;
    cout << endl;
  }
  const FacNodes& facNodes = fg.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighs = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      if (Globals::verbosity > 1) {
        cout << "creating link " ;
        cout << facNodes[i]->getLabel();
        cout << " -- " ;
        cout << neighs[j]->label();
        cout << " idx=" << j << ", weight=" << weights_[i][j] << endl;
      }
      links_.push_back (new WeightedLink (
          facNodes[i], neighs[j], j, weights_[i][j]));
    }
  }
  if (Globals::verbosity > 1) {
    cout << endl;
  }
}



void
WeightedBp::maxResidualSchedule()
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (Globals::verbosity >= 1) {
        std::cout << "calculating " << links_[i]->toString() << std::endl;
      }
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      std::cout << std::endl << "current residuals:" << std::endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); ++it) {
        std::cout << "    " << std::setw (30) << std::left;
        std::cout << (*it)->toString();
        std::cout << "residual = " << (*it)->residual() << std::endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    BpLink* link = *it;
    if (Globals::verbosity >= 1) {
      std::cout << "updating " << (*sortedOrder_.begin())->toString();
      std::cout << std::endl;
    }
    if (link->residual() < accuracy_) {
      return;
    }
    link->updateMessage();
    link->clearResidual();
    sortedOrder_.erase (it);
    linkMap_.find (link)->second = sortedOrder_.insert (link);

    // update the messages that depend on message source --> destin
    const FacNodes& factorNeighbors = link->varNode()->neighbors();
    for (size_t i = 0; i < factorNeighbors.size(); i++) {
      const BpLinks& links = getLinks (factorNeighbors[i]);
      for (size_t j = 0; j < links.size(); j++) {
        if (links[j]->varNode() != link->varNode()) {
          if (Globals::verbosity > 1) {
            std::cout << "    calculating " << links[j]->toString();
            std::cout << std::endl;
          }
          calculateMessage (links[j]);
          BpLinkMap::iterator iter = linkMap_.find (links[j]);
          sortedOrder_.erase (iter->second);
          iter->second = sortedOrder_.insert (links[j]);
        }
      }
    }
    // in counting bp, the message that a variable X sends to
    // to a factor F depends on the message that F sent to the X
    const BpLinks& links = getLinks (link->facNode());
    for (size_t i = 0; i < links.size(); i++) {
      if (links[i]->varNode() != link->varNode()) {
        if (Globals::verbosity > 1) {
          std::cout << "    calculating " << links[i]->toString();
          std::cout << std::endl;
        }
        calculateMessage (links[i]);
        BpLinkMap::iterator iter = linkMap_.find (links[i]);
        sortedOrder_.erase (iter->second);
        iter->second = sortedOrder_.insert (links[i]);
      }
    }
  }
}



void
WeightedBp::calcFactorToVarMsg (BpLink* _link)
{
  WeightedLink* link = static_cast<WeightedLink*> (_link);
  FacNode* src = link->facNode();
  const VarNode* dst = link->varNode();
  const BpLinks& links = getLinks (src);
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned reps = 1;
  unsigned msgSize = Util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
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
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
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
  assert (msgProduct.size() == src->factor().size());
  if (Globals::logDomain) {
    result.params() += src->factor().params();
  } else {
    result.params() *= src->factor().params();
  }
  if (Constants::showBpCalcs) {
    std::cout << "    message product: " ;
    std::cout << msgProduct << std::endl;
    std::cout << "    original factor: " ;
    std::cout << src->factor().params() << std::endl;
    std::cout << "    factor product:  " ;
    std::cout << result.params() << std::endl;
  }
  result.sumOutAllExceptIndex (link->index());
  if (Constants::showBpCalcs) {
    std::cout << "    marginalized:    " ;
    std::cout << result.params() << std::endl;
  }
  link->nextMessage() = result.params();
  LogAware::normalize (link->nextMessage());
  if (Constants::showBpCalcs) {
    std::cout << "    curr msg:        " ;
    std::cout << link->message() << std::endl;
    std::cout << "    next msg:        " ;
    std::cout << link->nextMessage() << std::endl;
  }
}



Params
WeightedBp::getVarToFactorMsg (const BpLink* _link)
{
  const WeightedLink* link = static_cast<const WeightedLink*> (_link);
  const VarNode* src = link->varNode();
  const FacNode* dst = link->facNode();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), LogAware::noEvidence());
    double value = link->message()[src->getEvidence()];
    if (Constants::showBpCalcs) {
      msg[src->getEvidence()] = value;
      std::cout << msg << "^" << link->weight() << "-1" ;
    }
    msg[src->getEvidence()] = LogAware::pow (value, link->weight() - 1);
  } else {
    msg = link->message();
    if (Constants::showBpCalcs) {
      std::cout << msg << "^" << link->weight() << "-1" ;
    }
    LogAware::pow (msg, link->weight() - 1);
  }
  const BpLinks& links = getLinks (src);
  if (Globals::logDomain) {
    for (size_t i = 0; i < links.size(); i++) {
      WeightedLink* l = static_cast<WeightedLink*> (links[i]);
      if ( ! (l->facNode() == dst && l->index() == link->index())) {
        msg += l->powMessage();
      }
    }
  } else {
    for (size_t i = 0; i < links.size(); i++) {
      WeightedLink* l = static_cast<WeightedLink*> (links[i]);
      if ( ! (l->facNode() == dst && l->index() == link->index())) {
        msg *= l->powMessage();
        if (Constants::showBpCalcs) {
          std::cout << " x " << l->nextMessage() << "^" << link->weight();
        }
      }
    }
  }
  if (Constants::showBpCalcs) {
    std::cout << " = " << msg;
  }
  return msg;
}



void
WeightedBp::printLinkInformation() const
{
  using std::cout;
  using std::endl;
  for (size_t i = 0; i < links_.size(); i++) {
    WeightedLink* l = static_cast<WeightedLink*> (links_[i]);
    cout << l->toString() << ":" << endl;
    cout << "    curr msg = " << l->message() << endl;
    cout << "    next msg = " << l->nextMessage() << endl;
    cout << "    pow msg  = " << l->powMessage() << endl;
    cout << "    index    = " << l->index() << endl;
    cout << "    weight   = " << l->weight() << endl;
    cout << "    residual = " << l->residual() << endl;
  }
}

}  // namespace Horus

