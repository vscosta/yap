#include "LiftedKc.h"
#include "LiftedWCNF.h"
#include "LiftedCircuit.h"
#include "LiftedOperations.h"
#include "Indexer.h"


LiftedKc::LiftedKc (const ParfactorList& pfList)
    : pfList_(pfList)
{

}



LiftedKc::~LiftedKc (void)
{
  delete lwcnf_;
  delete circuit_;
}



Params
LiftedKc::solveQuery (const Grounds& query)
{
  LiftedOperations::shatterAgainstQuery (pfList_, query);
  lwcnf_ = new LiftedWCNF (pfList_);
  circuit_ = new LiftedCircuit (lwcnf_);
  vector<PrvGroup> groups;
  Ranges ranges;
  for (size_t i = 0; i < query.size(); i++) {
    ParfactorList::const_iterator it = pfList_.begin();
    while (it != pfList_.end()) {
      size_t idx = (*it)->indexOfGround (query[i]);
      if (idx != (*it)->nrArguments()) {
        groups.push_back ((*it)->argument (idx).group());
        ranges.push_back ((*it)->range (idx));
        break;
      }
      ++ it;
    }
  }
  assert (groups.size() == query.size());
  cout << "groups: " << groups << endl;
  cout << "ranges: " << ranges << endl;
  Params params;
  Indexer indexer (ranges);
  while (indexer.valid()) {
    for (size_t i = 0; i < groups.size(); i++) {
      vector<LiteralId> litIds = lwcnf_->prvGroupLiterals (groups[i]);
      for (size_t j = 0; j < litIds.size(); j++) {
        if (indexer[i] == j) {
          lwcnf_->addWeight (litIds[j], LogAware::one(),
              LogAware::one());
        } else {
          lwcnf_->addWeight (litIds[j], LogAware::zero(),
              LogAware::one());
        }
      }
    }
    // cout << "new weights ----- ----- -----" << endl;
    // lwcnf_->printWeights();
    // circuit_->exportToGraphViz ("ccircuit.dot");
    params.push_back (circuit_->getWeightedModelCount());
    ++ indexer;
  }
  LogAware::normalize (params);
  if (Globals::logDomain) {
    Util::exp (params);
  }
  return params;
}



void
LiftedKc::printSolverFlags (void) const
{
  stringstream ss;
  ss << "lifted kc [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
}

