#include "LiftedBp.h"
#include "WeightedBp.h"
#include "FactorGraph.h"
#include "LiftedVe.h"


LiftedBp::LiftedBp (const ParfactorList& pfList)
    : pfList_(pfList)
{
  refineParfactors();
  solver_ = new WeightedBp (*getFactorGraph(), getWeights());
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
    VarIds queryVids;
    for (unsigned i = 0; i < groups.size(); i++) {
      queryVids.push_back (groups[i]);
    }
    res = solver_->getJointDistributionOf (queryVids);
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



FactorGraph*
LiftedBp::getFactorGraph (void)
{
  FactorGraph* fg = new FactorGraph();
  ParfactorList::const_iterator it = pfList_.begin();
  for (; it != pfList_.end(); ++it) {
    vector<PrvGroup> groups = (*it)->getAllGroups();
    VarIds varIds;
    for (size_t i = 0; i < groups.size(); i++) {
      varIds.push_back (groups[i]);
    }
    fg->addFactor (Factor (varIds, (*it)->ranges(), (*it)->params()));
  }
  return fg;
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


