
#include <algorithm>
#include <set>

#include "FoveSolver.h"
#include "Histogram.h"
#include "Util.h"


vector<LiftedOperator*>
LiftedOperator::getValidOps (
    ParfactorList& pfList,
    const Grounds& query)
{
  vector<LiftedOperator*>   validOps;
  vector<SumOutOperator*>   sumOutOps;
  vector<CountingOperator*> countOps;
  vector<GroundOperator*>   groundOps;

  sumOutOps = SumOutOperator::getValidOps (pfList, query);
  countOps  = CountingOperator::getValidOps (pfList);
  groundOps = GroundOperator::getValidOps (pfList);

  validOps.insert (validOps.end(), sumOutOps.begin(), sumOutOps.end());
  validOps.insert (validOps.end(), countOps.begin(),  countOps.end());
  validOps.insert (validOps.end(), groundOps.begin(), groundOps.end());
  return validOps;
}



void
LiftedOperator::printValidOps (
    ParfactorList& pfList,
    const Grounds& query)
{
  vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList, query);
  for (unsigned i = 0; i < validOps.size(); i++) {
    cout << "-> " << validOps[i]->toString() << endl;
    delete validOps[i];
  }
}



unsigned
SumOutOperator::getCost (void)
{
  TinySet<unsigned> groupSet;
  ParfactorList::const_iterator pfIter = pfList_.begin();
  while (pfIter != pfList_.end()) {
    if ((*pfIter)->containsGroup (group_)) {
      vector<unsigned> groups = (*pfIter)->getAllGroups();
      groupSet |= TinySet<unsigned> (groups);
    }
    ++ pfIter;
  }
  unsigned cost = 1;
  for (unsigned i = 0; i < groupSet.size(); i++) {
    pfIter = pfList_.begin();
    while (pfIter != pfList_.end()) {
      if ((*pfIter)->containsGroup (groupSet[i])) {
        int idx = (*pfIter)->indexOfGroup (groupSet[i]);
        cost *= (*pfIter)->range (idx);
        break;
      }
      ++ pfIter;
    }
  }
  return cost;
}



void
SumOutOperator::apply (void)
{
  vector<ParfactorList::iterator> iters 
      = parfactorsWithGroup (pfList_, group_);
  Parfactor* product = *(iters[0]);
  pfList_.remove (iters[0]);
  for (unsigned i = 1; i < iters.size(); i++) {
    product->multiply (**(iters[i]));
    pfList_.removeAndDelete (iters[i]);
  }
  if (product->nrArguments() == 1) {
    delete product;
    return;
  }
  int fIdx = product->indexOfGroup (group_);
  LogVarSet excl = product->exclusiveLogVars (fIdx);
  if (product->constr()->isCountNormalized (excl)) {
    product->sumOut (fIdx);
    pfList_.addShattered (product);
  } else {
    Parfactors pfs = FoveSolver::countNormalize (product, excl);
    for (unsigned i = 0; i < pfs.size(); i++) {
      pfs[i]->sumOut (fIdx);
      pfList_.add (pfs[i]);
    }
    delete product;
  }
}



vector<SumOutOperator*>
SumOutOperator::getValidOps (
    ParfactorList& pfList,
    const Grounds& query)
{
  vector<SumOutOperator*> validOps;
  set<unsigned> allGroups;
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (unsigned i = 0; i < formulas.size(); i++) {
      allGroups.insert (formulas[i].group());
    }
    ++ it;
  }
  set<unsigned>::const_iterator groupIt = allGroups.begin();
  while (groupIt != allGroups.end()) {
    if (validOp (*groupIt, pfList, query)) {
      validOps.push_back (new SumOutOperator (*groupIt, pfList));
    }
    ++ groupIt;
  }
  return validOps;
}



string
SumOutOperator::toString (void)
{
  stringstream ss;
  vector<ParfactorList::iterator> pfIters;
  pfIters = parfactorsWithGroup (pfList_, group_);
  int idx = (*pfIters[0])->indexOfGroup (group_);
  ProbFormula f = (*pfIters[0])->argument (idx);
  TupleSet tupleSet = (*pfIters[0])->constr()->tupleSet (f.logVars());
  ss << "sum out " << f.functor() << "/" << f.arity();
  ss << "|" << tupleSet << " (group " << group_ << ")";
  ss << " [cost=" << getCost() << "]" << endl;
  return ss.str();
}



bool
SumOutOperator::validOp (
    unsigned group,
    ParfactorList& pfList,
    const Grounds& query)
{
  vector<ParfactorList::iterator> pfIters;
  pfIters = parfactorsWithGroup (pfList, group);
  if (isToEliminate (*pfIters[0], group, query) == false) {
    return false;
  }
  unordered_map<unsigned, unsigned> groupToRange;
  for (unsigned i = 0; i < pfIters.size(); i++) {
    int fIdx = (*pfIters[i])->indexOfGroup (group);
    if ((*pfIters[i])->argument (fIdx).contains (
            (*pfIters[i])->elimLogVars()) == false) {
      return false;
    }
    vector<unsigned> ranges = (*pfIters[i])->ranges();
    vector<unsigned> groups = (*pfIters[i])->getAllGroups();
    for (unsigned i = 0; i < groups.size(); i++) {
      unordered_map<unsigned, unsigned>::iterator it;
      it = groupToRange.find (groups[i]); 
      if (it == groupToRange.end()) {
        groupToRange.insert (make_pair (groups[i], ranges[i]));
      } else {
        if (it->second != ranges[i]) {
          return false;
        }
      }
    }
  }
  return true;
}



vector<ParfactorList::iterator>
SumOutOperator::parfactorsWithGroup (
    ParfactorList& pfList,
    unsigned group)
{
  vector<ParfactorList::iterator> iters;
  ParfactorList::iterator pflIt = pfList.begin();
  while (pflIt != pfList.end()) {
    if ((*pflIt)->containsGroup (group)) {
      iters.push_back (pflIt);
    }
    ++ pflIt;
  }
  return iters;
}



bool
SumOutOperator::isToEliminate (
   Parfactor* g,
   unsigned group,
   const Grounds& query)
{
  int fIdx = g->indexOfGroup (group);
  const ProbFormula& formula = g->argument (fIdx);
  bool toElim = true;
  for (unsigned i = 0; i < query.size(); i++) {
    if (formula.functor() == query[i].functor() && 
        formula.arity()   == query[i].arity()) {
      g->constr()->moveToTop (formula.logVars());
      if (g->constr()->containsTuple (query[i].args())) {
        toElim = false;
        break;
      }
    }
  }
  return toElim;
}



unsigned
CountingOperator::getCost (void)
{
  unsigned cost = 0;
  int fIdx = (*pfIter_)->indexOfLogVar (X_);
  unsigned range = (*pfIter_)->range (fIdx);
  unsigned size  = (*pfIter_)->size() / range;
  TinySet<unsigned> counts;
  counts = (*pfIter_)->constr()->getConditionalCounts (X_);
  for (unsigned i = 0; i < counts.size(); i++) {
    cost += size * HistogramSet::nrHistograms (counts[i], range);
  }
  return cost;
}



void
CountingOperator::apply (void)
{
  if ((*pfIter_)->constr()->isCountNormalized (X_)) {
    (*pfIter_)->countConvert (X_);
  } else {
    Parfactor* pf = *pfIter_;
    pfList_.remove (pfIter_);
    Parfactors pfs = FoveSolver::countNormalize (pf, X_);
    for (unsigned i = 0; i  < pfs.size(); i++) {
      unsigned condCount = pfs[i]->constr()->getConditionalCount (X_);
      bool cartProduct   = pfs[i]->constr()->isCarteesianProduct (
          pfs[i]->countedLogVars() | X_);
      if (condCount > 1 && cartProduct) {
        pfs[i]->countConvert (X_);
      }
      pfList_.add (pfs[i]);
    }
    delete pf;
  }
}



vector<CountingOperator*>
CountingOperator::getValidOps (ParfactorList& pfList)
{
  vector<CountingOperator*> validOps;
  ParfactorList::iterator it = pfList.begin();
  while (it != pfList.end()) {
    LogVarSet candidates = (*it)->uncountedLogVars();
    for (unsigned i = 0; i < candidates.size(); i++) {
      if (validOp (*it, candidates[i])) {
        validOps.push_back (new CountingOperator (
            it, candidates[i], pfList));
      }
    }
    ++ it;
  }
  return validOps;
}



string
CountingOperator::toString (void)
{
  stringstream ss;
  ss << "count convert " << X_ << " in " ;
  ss << (*pfIter_)->getLabel();
  ss << " [cost=" << getCost() << "]" << endl;
  Parfactors pfs = FoveSolver::countNormalize (*pfIter_, X_);
  if ((*pfIter_)->constr()->isCountNormalized (X_) == false) {
    for (unsigned i = 0; i < pfs.size(); i++) {
      ss << "   º " << pfs[i]->getLabel() << endl;
    }
  }
  for (unsigned i = 0; i < pfs.size(); i++) {
    delete pfs[i];
  }
  return ss.str();
}



bool
CountingOperator::validOp (Parfactor* g, LogVar X)
{
  if (g->nrFormulas (X) != 1) {
    return false;
  }
  int fIdx = g->indexOfLogVar (X);
  if (g->argument (fIdx).isCounting()) {
    return false;
  }
  bool countNormalized = g->constr()->isCountNormalized (X);
  if (countNormalized) {
    unsigned condCount = g->constr()->getConditionalCount (X);
    bool cartProduct   = g->constr()->isCarteesianProduct (
        g->countedLogVars() | X);
    if (condCount == 1 || cartProduct == false) {
      return false;
    }
  }
  return true;
}



unsigned
GroundOperator::getCost (void)
{
  unsigned cost = 0;
  bool isCountingLv = (*pfIter_)->countedLogVars().contains (X_);
  if (isCountingLv) {
    int fIdx = (*pfIter_)->indexOfLogVar (X_);
    unsigned currSize  = (*pfIter_)->size();
    unsigned nrHists   = (*pfIter_)->range (fIdx);
    unsigned range     = (*pfIter_)->argument (fIdx).range();
    unsigned nrSymbols = (*pfIter_)->constr()->getConditionalCount (X_);
    cost = (currSize / nrHists) * (std::pow (range, nrSymbols));
  } else {
    cost = (*pfIter_)->constr()->nrSymbols (X_) * (*pfIter_)->size();
  }
  return cost;
}



void
GroundOperator::apply (void)
{
  bool countedLv = (*pfIter_)->countedLogVars().contains (X_);
  Parfactor* pf = *pfIter_;
  pfList_.remove (pfIter_);
  if (countedLv) {
    pf->fullExpand (X_);
    pfList_.add (pf);
  } else {
    ConstraintTrees cts = pf->constr()->ground (X_);
    for (unsigned i = 0; i < cts.size(); i++) {
      pfList_.add (new Parfactor (pf, cts[i]));
    }
    delete pf;
  }
}



vector<GroundOperator*>
GroundOperator::getValidOps (ParfactorList& pfList)
{
  vector<GroundOperator*> validOps;
  ParfactorList::iterator pfIter = pfList.begin();
  while (pfIter != pfList.end()) {
    LogVarSet set = (*pfIter)->logVarSet();
    for (unsigned i = 0; i < set.size(); i++) {
      if ((*pfIter)->constr()->isSingleton (set[i]) == false) {
        validOps.push_back (new GroundOperator (pfIter, set[i], pfList));
      }
    }
    ++ pfIter;
  }
  return validOps;
}



string
GroundOperator::toString (void)
{
  stringstream ss;
  ((*pfIter_)->countedLogVars().contains (X_)) 
      ? ss << "full expanding " 
      : ss << "grounding " ;
  ss << X_ << " in " << (*pfIter_)->getLabel();
  ss << " [cost=" << getCost() << "]" << endl;
  return ss.str();
}



Params
FoveSolver::getPosterioriOf (const Ground& query)
{
  return getJointDistributionOf ({query});
}



Params
FoveSolver::getJointDistributionOf (const Grounds& query)
{
  runSolver (query);
  (*pfList_.begin())->normalize();
  Params params = (*pfList_.begin())->params();
  if (Globals::logDomain) {
    Util::fromLog (params);
  }
  return params;
}



void
FoveSolver::absorveEvidence (
    ParfactorList& pfList,
    ObservedFormulas& obsFormulas)
{
  for (unsigned i = 0; i < obsFormulas.size(); i++) {
    Parfactors newPfs;
    ParfactorList::iterator it  = pfList.begin();
    while (it != pfList.end()) {
      Parfactor* pf = *it;
      it = pfList.remove (it);
      Parfactors absorvedPfs = absorve (obsFormulas[i], pf);
      if (absorvedPfs.empty() == false) {
        if (absorvedPfs.size() == 1 && absorvedPfs[0] == 0) {
          // just remove pf;
        } else {
          Util::addToVector (newPfs, absorvedPfs);
        }
        delete pf;
      } else {
        it = pfList.insertShattered (it, pf);
        ++ it;
      }
    }
    pfList.add (newPfs);
  }
  if (Constants::DEBUG > 1 && obsFormulas.empty() == false) {
    Util::printAsteriskLine();
    cout << "AFTER EVIDENCE ABSORVED" << endl;
    for (unsigned i = 0; i < obsFormulas.size(); i++) {
      cout << " -> " << obsFormulas[i] << endl;
    }
    Util::printAsteriskLine();
    pfList.print();
  }
}



Parfactors
FoveSolver::countNormalize (
    Parfactor* g,
    const LogVarSet& set)
{
  Parfactors normPfs;
  if (set.empty()) {
    normPfs.push_back (new Parfactor (*g));
  } else {
    ConstraintTrees normCts = g->constr()->countNormalize (set);
    for (unsigned i = 0; i < normCts.size(); i++) {
      normPfs.push_back (new Parfactor (g, normCts[i]));
    }
  }
  return normPfs;
}



void
FoveSolver::runSolver (const Grounds& query)
{
  shatterAgainstQuery (query);
  runWeakBayesBall (query);
  while (true) {
    if (Constants::DEBUG > 1) {
      Util::printDashedLine();
      pfList_.print();
      LiftedOperator::printValidOps (pfList_, query);
    }
    LiftedOperator* op = getBestOperation (query);
    if (op == 0) {
      break;
    }
    if (Constants::DEBUG > 1) {
      cout << "best operation: " << op->toString() << endl;
    }
    op->apply();
    delete op;
  }
  assert (pfList_.size() > 0);
  if (pfList_.size() > 1) {
    ParfactorList::iterator pfIter = pfList_.begin();
    pfIter ++;
    while (pfIter != pfList_.end()) {
      (*pfList_.begin())->multiply (**pfIter);
      ++ pfIter;
    }
  }
  (*pfList_.begin())->reorderAccordingGrounds (query);
}



LiftedOperator*
FoveSolver::getBestOperation (const Grounds& query)
{
  unsigned bestCost;
  LiftedOperator* bestOp = 0;
  vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList_, query);
  for (unsigned i = 0; i < validOps.size(); i++) {
    unsigned cost = validOps[i]->getCost();
    if ((bestOp == 0) || (cost < bestCost)) {
      bestOp   = validOps[i];
      bestCost = cost;
    } 
  }
  for (unsigned i = 0; i < validOps.size(); i++) {
    if (validOps[i] != bestOp) {
      delete validOps[i];
    }
  }
  return bestOp;
}



void
FoveSolver::runWeakBayesBall (const Grounds& query)
{
  queue<unsigned> todo; // groups to process
  set<unsigned> done;   // processed or in queue 
  for (unsigned i = 0; i < query.size(); i++) {
    ParfactorList::iterator it = pfList_.begin();
    while (it != pfList_.end()) {
      int group = (*it)->findGroup (query[i]);
      if (group != -1) {
        todo.push (group);
        done.insert (group);
        break;
      }
      ++ it;
    }
  }

  set<Parfactor*> requiredPfs;
  while (todo.empty() == false) {
    unsigned group = todo.front();
    ParfactorList::iterator it = pfList_.begin();
    while (it != pfList_.end()) {
      if (Util::contains (requiredPfs, *it) == false &&
          (*it)->containsGroup (group)) {
        vector<unsigned> groups = (*it)->getAllGroups();
        for (unsigned i = 0; i < groups.size(); i++) {
          if (Util::contains (done, groups[i]) == false) {
            todo.push (groups[i]);
            done.insert (groups[i]);
          }
        }
        requiredPfs.insert (*it);
      }
      ++ it;
    }
    todo.pop();
  }

  ParfactorList::iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    if (Util::contains (requiredPfs, *it) == false) {
      it = pfList_.removeAndDelete (it);
    } else {
      ++ it;
    }
  }

  if (Constants::DEBUG > 1) {
    Util::printHeader ("REQUIRED PARFACTORS");
    pfList_.print();
  }
}



void
FoveSolver::shatterAgainstQuery (const Grounds& query)
{
  return ;
  for (unsigned i = 0; i < query.size(); i++) {
    if (query[i].isAtom()) {
      continue;
    }
    Parfactors newPfs;
    ParfactorList::iterator it = pfList_.begin();
    while (it != pfList_.end()) {
      if ((*it)->containsGround (query[i])) {
        std::pair<ConstraintTree*, ConstraintTree*> split = 
            (*it)->constr()->split (query[i].args(), query[i].arity());
        ConstraintTree* commCt = split.first;
        ConstraintTree* exclCt = split.second;
        newPfs.push_back (new Parfactor (*it, commCt));
        if (exclCt->empty() == false) {
          newPfs.push_back (new Parfactor (*it, exclCt));
        } else {
          delete exclCt;
        }
        it = pfList_.removeAndDelete (it);
      } else {
        ++ it;
      }
    }
    pfList_.add (newPfs);
  }
  if (Constants::DEBUG > 1) {
    cout << endl;
    Util::printAsteriskLine();
    cout << "SHATTERED AGAINST THE QUERY" << endl;
    for (unsigned i = 0; i < query.size(); i++) {
      cout << " -> " << query[i] << endl;
    }
    Util::printAsteriskLine();
    pfList_.print();
  }
}



Parfactors
FoveSolver::absorve (
    ObservedFormula& obsFormula,
    Parfactor* g)
{
  Parfactors absorvedPfs;
  const ProbFormulas& formulas = g->arguments();
  for (unsigned i = 0; i < formulas.size(); i++) {
    if (obsFormula.functor() == formulas[i].functor() &&
        obsFormula.arity()   == formulas[i].arity()) {

      if (obsFormula.isAtom()) {
        if (formulas.size() > 1) {
          g->absorveEvidence (formulas[i], obsFormula.evidence());
        } else {
          // hack to erase parfactor g
          absorvedPfs.push_back (0);
        }
        break;
      } 

      g->constr()->moveToTop (formulas[i].logVars());
      std::pair<ConstraintTree*, ConstraintTree*> res
          = g->constr()->split (&(obsFormula.constr()), formulas[i].arity());
      ConstraintTree* commCt = res.first;
      ConstraintTree* exclCt = res.second;

      if (commCt->empty() == false) {
        if (formulas.size() > 1) {
          LogVarSet excl = g->exclusiveLogVars (i);
          Parfactors countNormPfs = countNormalize (g, excl);
          for (unsigned j = 0; j < countNormPfs.size(); j++) {
            countNormPfs[j]->absorveEvidence (
                formulas[i], obsFormula.evidence());
            absorvedPfs.push_back (countNormPfs[j]);
          }
        } else {
          delete commCt;
        }
        if (exclCt->empty() == false) {
          absorvedPfs.push_back (new Parfactor (g, exclCt));
        } else {
          delete exclCt;
        }
        if (absorvedPfs.empty()) {
          // hack to erase parfactor g
          absorvedPfs.push_back (0);
        }
        break;
      } else {
        delete commCt;
        delete exclCt;
      }
    }
  }
  return absorvedPfs;
}

