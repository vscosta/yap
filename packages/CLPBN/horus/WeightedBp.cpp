#include "WeightedBp.h"


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
    probs.resize (var->range(), LogAware::noEvidence());
    probs[var->getEvidence()] = LogAware::withEvidence();
  } else {
    probs.resize (var->range(), LogAware::multIdenty());
    const BpLinks& links = ninf(var)->getLinks();
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



void
WeightedBp::createLinks (void)
{
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
WeightedBp::maxResidualSchedule (void)
{
  if (nIters_ == 1) {
    for (size_t i = 0; i < links_.size(); i++) {
      calculateMessage (links_[i]);
      SortedOrder::iterator it = sortedOrder_.insert (links_[i]);
      linkMap_.insert (make_pair (links_[i], it));
      if (Globals::verbosity >= 1) {
        cout << "calculating " << links_[i]->toString() << endl;
      }
    }
    return;
  }

  for (size_t c = 0; c < links_.size(); c++) {
    if (Globals::verbosity > 1) {
      cout << endl << "current residuals:" << endl;
      for (SortedOrder::iterator it = sortedOrder_.begin();
          it != sortedOrder_.end(); ++it) {
        cout << "    " << setw (30) << left << (*it)->toString();
        cout << "residual = " << (*it)->residual() << endl;
      }
    }

    SortedOrder::iterator it = sortedOrder_.begin();
    BpLink* link = *it;
    if (Globals::verbosity >= 1) {
      cout << "updating " << (*sortedOrder_.begin())->toString() << endl;
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
          if (Globals::verbosity > 1) {
            cout << "    calculating " << links[j]->toString() << endl;
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
        if (Globals::verbosity > 1) {
          cout << "    calculating " << links[i]->toString() << endl;
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
  unsigned msgSize = Util::sizeExpected (src->factor().ranges());
  Params msgProduct (msgSize, LogAware::multIdenty());
  if (Globals::logDomain) {
    for (size_t i = links.size(); i-- > 0; ) {
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->varNode()->label();
          cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::plus<double>());
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
        }
      }
      reps *= links[i]->varNode()->range();
    }
  } else {
    for (size_t i = links.size(); i-- > 0; ) {
      const WeightedLink* l = static_cast<const WeightedLink*> (links[i]);
      if ( ! (l->varNode() == dst && l->index() == link->index())) {
        if (Constants::SHOW_BP_CALCS) {
          cout << "    message from " << links[i]->varNode()->label();
          cout << ": " ;
        }
        Util::apply_n_times (msgProduct, getVarToFactorMsg (links[i]),
            reps, std::multiplies<double>());
        if (Constants::SHOW_BP_CALCS) {
          cout << endl;
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
  if (Constants::SHOW_BP_CALCS) {
    cout << "    message product:  " << msgProduct << endl;
    cout << "    original factor:  " << src->factor().params() << endl;
    cout << "    factor product:   " << result.params() << endl;
  }
  result.sumOutAllExceptIndex (link->index());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    marginalized:     " << result.params() << endl;
  }
  link->nextMessage() = result.params();
  LogAware::normalize (link->nextMessage());
  if (Constants::SHOW_BP_CALCS) {
    cout << "    curr msg:         " << link->message() << endl;
    cout << "    next msg:         " << link->nextMessage() << endl;
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
    msg.resize (src->range(), LogAware::noEvidence());
    double value = link->message()[src->getEvidence()];
    if (Constants::SHOW_BP_CALCS) {
      msg[src->getEvidence()] = value;
      cout << msg << "^" << link->weight() << "-1" ;
    }
    msg[src->getEvidence()] = LogAware::pow (value, link->weight() - 1);
  } else {
    msg = link->message();
    if (Constants::SHOW_BP_CALCS) {
      cout << msg << "^" << link->weight() << "-1" ;
    }
    LogAware::pow (msg, link->weight() - 1);
  }
  const BpLinks& links = ninf(src)->getLinks();
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
        if (Constants::SHOW_BP_CALCS) {
          cout << " x " << l->nextMessage() << "^" << link->weight();
        }
      }
    }
  }
  if (Constants::SHOW_BP_CALCS) {
    cout << " = " << msg;
  }
  return msg;
}



void
WeightedBp::printLinkInformation (void) const
{
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

