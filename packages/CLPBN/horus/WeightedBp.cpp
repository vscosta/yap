#include <cassert>

#include <iostream>

#include "WeightedBp.h"

namespace horus {

WeightedBp::~WeightedBp (void)
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
    probs.resize (var->range(), log_aware::noEvidence());
    probs[var->getEvidence()] = log_aware::withEvidence();
  } else {
    probs.resize (var->range(), log_aware::multIdenty());
    const BpLinks& links = ninf(var)->getLinks();
    if (globals::logDomain) {
      for (size_t i = 0; i < links.size(); i++) {
        WeightedLink* l = static_cast<WeightedLink*> (links[i]);
        probs += l->powMessage();
      }
      log_aware::normalize (probs);
      util::exp (probs);
    } else {
      for (size_t i = 0; i < links.size(); i++) {
        WeightedLink* l = static_cast<WeightedLink*> (links[i]);
        probs *= l->powMessage();
      }
      log_aware::normalize (probs);
    }
  }
  return probs;
}



void
WeightedBp::createLinks (void)
{
  using std::cout;
  using std::endl;
  if (globals::verbosity > 0) {
    cout << "compressed factor graph contains " ;
    cout << fg.nrVarNodes() << " variables and " ;
    cout << fg.nrFacNodes() << " factors " << endl;
    cout << endl;
  }
  const FacNodes& facNodes = fg.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const VarNodes& neighs = facNodes[i]->neighbors();
    for (size_t j = 0; j < neighs.size(); j++) {
      if (globals::verbosity > 1) {
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
  if (globals::verbosity > 1) {
    cout << endl;
  }
}



void
WeightedBp::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (globals::verbosity >= 1) {
        std::cout << "calculating " << links_[i]->toString() << std::endl;
      }
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (globals::verbosity > 1) {
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
    if (globals::verbosity >= 1) {
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
      const BpLinks& links = ninf(factorNeighbors[i])->getLinks();
      for (size_t j = 0; j < links.size(); j++) {
        if (links[j]->varNode() != link->varNode()) {
          if (globals::verbosity > 1) {
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
    const BpLinks& links = ninf(link->facNode())->getLinks();
    for (size_t i = 0; i < links.size(); i++) {
      if (links[i]->varNode() != link->varNode()) {
        if (globals::verbosity > 1) {
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
  const BpLinks& links = ninf(src)->getLinks();
  // calculate the product of messages that were sent
  // to factor `src', except from var `dst'
  unsigned reps = 1;
  unsigned msgSize = util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, log_aware::multIdenty());
  if (globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
        if (constants::SHOW_BP_CALCS) {
          std::cout << "    message from " << links[i]->varNode()->label();
          std::cout << ": " ;
        }
        util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::plus<double>());
        if (constants::SHOW_BP_CALCS) {
          std::cout << std::endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  } else {
    for (size_t i = links.size(); i-- > 0; ) {
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
        if (constants::SHOW_BP_CALCS) {
          std::cout << "    message from " << links[i]->varNode()->label();
          std::cout << ": " ;
        }
        util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::multiplies<double>());
        if (constants::SHOW_BP_CALCS) {
          std::cout << std::endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  }
  Factor result (src->factor().arguments(),
      src->factor().ranges(), msgProduct);
  assert (msgProduct.size() == src->factor().size());
  if (globals::logDomain) {
    result.params() += src->factor().params();
  } else {
    result.params() *= src->factor().params();
  }
  if (constants::SHOW_BP_CALCS) {
    std::cout << "    message product: " ;
    std::cout << msgProduct << std::endl;
    std::cout << "    original factor: " ;
    std::cout << src->factor().params() << std::endl;
    std::cout << "    factor product:  " ;
    std::cout << result.params() << std::endl;
  }
  result.sumOutAllExceptIndex (link->index());
  if (constants::SHOW_BP_CALCS) {
    std::cout << "    marginalized:    " ;
    std::cout << result.params() << std::endl;
  }
  link->nextMessage() = result.params();
  log_aware::normalize (link->nextMessage());
  if (constants::SHOW_BP_CALCS) {
    std::cout << "    curr msg:        " ;
    std::cout << link->message() << std::endl;
    std::cout << "    next msg:        " ;
    std::cout << link->nextMessage() << std::endl;
  }
}



Params
WeightedBp::getVarToFactorMsg (const BpLink* _link) const
{
  const WeightedLink* link = static_cast<const WeightedLink*> (_link);
  const VarNode* src = link->varNode();
  const FacNode* dst = link->facNode();
  Params msg;
  if (src->hasEvidence()) {
    msg.resize (src->range(), log_aware::noEvidence());
    double value = link->message()[src->getEvidence()];
    if (constants::SHOW_BP_CALCS) {
      msg[src->getEvidence()] = value;
      std::cout << msg << "^" << link->weight() << "-1" ;
    }
    msg[src->getEvidence()] = log_aware::pow (value, link->weight() - 1);
  } else {
    msg = link->message();
    if (constants::SHOW_BP_CALCS) {
      std::cout << msg << "^" << link->weight() << "-1" ;
    }
    log_aware::pow (msg, link->weight() - 1);
  }
  const BpLinks& links = ninf(src)->getLinks();
  if (globals::logDomain) {
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
        if (constants::SHOW_BP_CALCS) {
          std::cout << " x " << l->nextMessage() << "^" << link->weight();
        }
      }
    }
  }
  if (constants::SHOW_BP_CALCS) {
    std::cout << " = " << msg;
  }
  return msg;
}



void
WeightedBp::printLinkInformation (void) const
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

}  // namespace horus

