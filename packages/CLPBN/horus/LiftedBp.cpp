#include "LiftedBp.h"
#include "WeightedBp.h"
#include "FactorGraph.h"
#include "LiftedVe.h"


LiftedBp::LiftedBp (const ParfactorList& pfList)
    : pfList_(pfList)
{
  refineParfactors();
  createFactorGraph();
  solver_ = new WeightedBp (*fg_, getWeights());
}



LiftedBp::~LiftedBp (void)
{
  delete solver_;
  delete fg_;
}



Params
LiftedBp::solveQuery (const Grounds& query)
{
  assert (query.empty() == false);
  Params res;
  vector<PrvGroup> groups = getQueryGroups (query);
  if (query.size() == 1) {
    res = solver_->getPosterioriOf (groups[0]);
  } else {
    ParfactorList::iterator it = pfList_.begin();
    size_t idx = pfList_.size();
    size_t count = 0;
    while (it != pfList_.end()) {
      if ((*it)->containsGrounds (query)) {
        idx = count;
        break;
      }
      ++ it;
      ++ count;
    }
    if (idx == pfList_.size()) {
      res = getJointByConditioning (pfList_, query);
    } else {
      VarIds queryVids;
      for (unsigned i = 0; i < groups.size(); i++) {
        queryVids.push_back (groups[i]);
      }
      res = solver_->getFactorJoint (idx, queryVids);
    }
  }
  return res;
}



void
LiftedBp::printSolverFlags (void) const
{
  stringstream ss;
  ss << "lifted bp [" ;
  ss << "schedule=" ;
  typedef BpOptions::Schedule Sch;
  switch (BpOptions::schedule) {
    case Sch::SEQ_FIXED:    ss << "seq_fixed";    break;
    case Sch::SEQ_RANDOM:   ss << "seq_random";   break;
    case Sch::PARALLEL:     ss << "parallel";     break;
    case Sch::MAX_RESIDUAL: ss << "max_residual"; break;
  }
  ss << ",max_iter=" << BpOptions::maxIter;
  ss << ",accuracy=" << BpOptions::accuracy;
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}



void
LiftedBp::refineParfactors (void)
{
  while (iterate() == false);

  if (Globals::verbosity > 2) {
    Util::printHeader ("AFTER REFINEMENT");
    pfList_.print();
  }
}



bool
LiftedBp::iterate (void)
{
  ParfactorList::iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    const ProbFormulas& args = (*it)->arguments();
    for (size_t i = 0; i < args.size(); i++) {
      LogVarSet lvs = (*it)->logVarSet() - args[i].logVars();
      if ((*it)->constr()->isCountNormalized (lvs) == false) {
        Parfactors pfs = LiftedVe::countNormalize (*it, lvs);
        it = pfList_.removeAndDelete (it);
        pfList_.add (pfs);
        return false;
      }
    }
    ++ it;
  }
  return true;
}



vector<PrvGroup>
LiftedBp::getQueryGroups (const Grounds& query)
{
  vector<PrvGroup> queryGroups;
  for (unsigned i = 0; i < query.size(); i++) {
    ParfactorList::const_iterator it = pfList_.begin();
    for (; it != pfList_.end(); ++it) {
      if ((*it)->containsGround (query[i])) {
        queryGroups.push_back ((*it)->findGroup (query[i]));
        break;
      }
    }
  }
  assert (queryGroups.size() == query.size());
  return queryGroups;
}



void
LiftedBp::createFactorGraph (void)
{
  fg_ = new FactorGraph();
  ParfactorList::const_iterator it = pfList_.begin();
  for (; it != pfList_.end(); ++it) {
    vector<PrvGroup> groups = (*it)->getAllGroups();
    VarIds varIds;
    for (size_t i = 0; i < groups.size(); i++) {
      varIds.push_back (groups[i]);
    }
    fg_->addFactor (Factor (varIds, (*it)->ranges(), (*it)->params()));
  }
}



vector<vector<unsigned>>
LiftedBp::getWeights (void) const
{
  vector<vector<unsigned>> weights;
  weights.reserve (pfList_.size());
  ParfactorList::const_iterator it = pfList_.begin();
  for (; it != pfList_.end(); ++it) {
    const ProbFormulas& args = (*it)->arguments();
    weights.push_back ({ });
    weights.back().reserve (args.size());
    for (size_t i = 0; i < args.size(); i++) {
      LogVarSet lvs = (*it)->logVarSet() - args[i].logVars();
      weights.back().push_back ((*it)->constr()->getConditionalCount (lvs));
    }
  }
  return weights;
}



unsigned
LiftedBp::rangeOfGround (const Ground& gr)
{
  ParfactorList::iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    if ((*it)->containsGround (gr)) {
      PrvGroup prvGroup = (*it)->findGroup (gr);
      return (*it)->range ((*it)->indexOfGroup (prvGroup));
    }
    ++ it;
  }
  return std::numeric_limits<unsigned>::max();
}

	

Params
LiftedBp::getJointByConditioning (
    const ParfactorList& pfList,
    const Grounds& grounds)
{
  LiftedBp solver (pfList);
  Params prevBeliefs = solver.solveQuery ({grounds[0]});
  Grounds obsGrounds = {grounds[0]};
  for (size_t i = 1; i < grounds.size(); i++) {
    Params newBeliefs;
    vector<ObservedFormula> obsFs;
    Ranges obsRanges;
    for (size_t j = 0; j < obsGrounds.size(); j++) {
      obsFs.push_back (ObservedFormula (
          obsGrounds[j].functor(), 0, obsGrounds[j].args()));
      obsRanges.push_back (rangeOfGround (obsGrounds[j]));
    }
    Indexer indexer (obsRanges, false);
    while (indexer.valid()) {
      for (size_t j = 0; j < obsFs.size(); j++) {
        obsFs[j].setEvidence (indexer[j]);
      }
      ParfactorList tempPfList (pfList);
      LiftedVe::absorveEvidence (tempPfList, obsFs);
      LiftedBp solver (tempPfList);
      Params beliefs = solver.solveQuery ({grounds[i]});
      for (size_t k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
      ++ indexer;
    }
    int count = -1;
    unsigned range = rangeOfGround (grounds[i]);
    for (size_t j = 0; j < newBeliefs.size(); j++) {
      if (j % range == 0) {
        count ++;
      }
      newBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = newBeliefs;
    obsGrounds.push_back (grounds[i]);
  }
  return prevBeliefs;
}

