#include "LiftedKc.h"
#include "LiftedWCNF.h"
#include "LiftedCircuit.h"
#include "Indexer.h"


LiftedKc::LiftedKc (const ParfactorList& pfList)
    : pfList_(pfList)
{
  lwcnf_ = new LiftedWCNF (pfList);
  circuit_ = new LiftedCircuit (lwcnf_);
}



LiftedKc::~LiftedKc (void)
{
}



Params
LiftedKc::solveQuery (const Grounds& query)
{
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
  cout << "groups: " << groups << endl;
  cout << "ranges: " << ranges << endl;
  Params params;
  Indexer indexer (ranges);
  while (indexer.valid()) {
    for (size_t i = 0; i < groups.size(); i++) {
      vector<LiteralId> litIds = lwcnf_->prvGroupLiterals (groups[i]);
      for (size_t j = 0; j < litIds.size(); j++) {
        if (indexer[i] == j) {
          lwcnf_->addWeight (litIds[j], 1.0, 1.0); // TODO not log aware
        } else {
          lwcnf_->addWeight (litIds[j], 0.0, 1.0); // TODO not log aware
        }
      }
    }
    // cout << "new weights ----- ----- -----" << endl;
    // lwcnf_->printWeights();
    // circuit_->exportToGraphViz ("ccircuit.dot");
    params.push_back (circuit_->getWeightedModelCount());
    ++ indexer;
  }
  cout << "params: " << params << endl;
  LogAware::normalize (params);
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

