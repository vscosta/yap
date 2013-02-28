#include <cassert>

#include <sstream>

#include "LiftedBp.h"
#include "LiftedOperations.h"
#include "WeightedBp.h"
#include "FactorGraph.h"


namespace Horus {

LiftedBp::LiftedBp (const ParfactorList& parfactorList)
    : LiftedSolver (parfactorList)
{
  refineParfactors();
  createFactorGraph();
  solver_ = new WeightedBp (*fg_, getWeights());
}



LiftedBp::~LiftedBp()
{
  delete solver_;
  delete fg_;
}



Params
LiftedBp::solveQuery (const Grounds& query)
{
  assert (query.empty() == false);
  Params res;
  std::vector<PrvGroup> groups = getQueryGroups (query);
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
      res = solver_->getFactorJoint (fg_->facNodes()[idx], queryVids);
    }
  }
  return res;
}



void
LiftedBp::printSolverFlags() const
{
  std::stringstream ss;
  ss << "lifted bp [" ;
  ss << "bp_msg_schedule=" ;
  typedef WeightedBp::MsgSchedule MsgSchedule;
  switch (WeightedBp::msgSchedule()) {
    case MsgSchedule::seqFixedSch:    ss << "seq_fixed";    break;
    case MsgSchedule::seqRandomSch:   ss << "seq_random";   break;
    case MsgSchedule::parallelSch:    ss << "parallel";     break;
    case MsgSchedule::maxResidualSch: ss << "max_residual"; break;
  }
  ss << ",bp_max_iter=" << WeightedBp::maxIterations();
  ss << ",bp_accuracy=" << WeightedBp::accuracy();
  ss << ",log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  std::cout << ss.str() << std::endl;
}



void
LiftedBp::refineParfactors()
{
  pfList_ = parfactorList;
  while (iterate() == false);

  if (Globals::verbosity > 2) {
    Util::printHeader ("AFTER REFINEMENT");
    pfList_.print();
  }
}



bool
LiftedBp::iterate()
{
  ParfactorList::iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    const ProbFormulas& args = (*it)->arguments();
    for (size_t i = 0; i < args.size(); i++) {
      LogVarSet lvs = (*it)->logVarSet() - args[i].logVars();
      if ((*it)->constr()->isCountNormalized (lvs) == false) {
        Parfactors pfs = LiftedOperations::countNormalize (*it, lvs);
        it = pfList_.removeAndDelete (it);
        pfList_.add (pfs);
        return false;
      }
    }
    ++ it;
  }
  return true;
}



std::vector<PrvGroup>
LiftedBp::getQueryGroups (const Grounds& query)
{
  std::vector<PrvGroup> queryGroups;
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
LiftedBp::createFactorGraph()
{
  fg_ = new FactorGraph();
  ParfactorList::const_iterator it = pfList_.begin();
  for (; it != pfList_.end(); ++it) {
    std::vector<PrvGroup> groups = (*it)->getAllGroups();
    VarIds varIds;
    for (size_t i = 0; i < groups.size(); i++) {
      varIds.push_back (groups[i]);
    }
    fg_->addFactor (Factor (varIds, (*it)->ranges(), (*it)->params()));
  }
}



std::vector<std::vector<unsigned>>
LiftedBp::getWeights() const
{
  std::vector<std::vector<unsigned>> weights;
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
  return Util::maxUnsigned();
}



Params
LiftedBp::getJointByConditioning (
    const ParfactorList& pfList,
    const Grounds& query)
{
  LiftedBp solver (pfList);
  Params prevBeliefs = solver.solveQuery ({query[0]});
  Grounds obsGrounds = {query[0]};
  for (size_t i = 1; i < query.size(); i++) {
    Params newBeliefs;
    std::vector<ObservedFormula> obsFs;
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
      LiftedOperations::absorveEvidence (tempPfList, obsFs);
      LiftedBp solver (tempPfList);
      Params beliefs = solver.solveQuery ({query[i]});
      for (size_t k = 0; k < beliefs.size(); k++) {
        newBeliefs.push_back (beliefs[k]);
      }
      ++ indexer;
    }
    int count = -1;
    unsigned range = rangeOfGround (query[i]);
    for (size_t j = 0; j < newBeliefs.size(); j++) {
      if (j % range == 0) {
        count ++;
      }
      newBeliefs[j] *= prevBeliefs[count];
    }
    prevBeliefs = newBeliefs;
    obsGrounds.push_back (query[i]);
  }
  return prevBeliefs;
}

}  // namespace Horus

