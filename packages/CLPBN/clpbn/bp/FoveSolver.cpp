
#include <algorithm>
#include <set>

#include "FoveSolver.h"
#include "Histogram.h"
#include "Util.h"


vector<LiftedOperator*>
LiftedOperator::getValidOps (ParfactorList& pfList, const Grounds& query)
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
LiftedOperator::printValidOps (ParfactorList& pfList, const Grounds& query)
{
  vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList, query);
  for (unsigned i = 0; i < validOps.size(); i++) {
    cout << "-> " << validOps[i]->toString() << endl;
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
        int idx = (*pfIter)->indexOfFormulaWithGroup (groupSet[i]);
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
    delete *(iters[i]);
    pfList_.remove (iters[i]);
  }
  if (product->nrFormulas() == 1) {
    delete product;
    return;
  }
  int fIdx = product->indexOfFormulaWithGroup (group_);
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
    pfList_.shatter();
  }
}



vector<SumOutOperator*>
SumOutOperator::getValidOps (ParfactorList& pfList, const Grounds& query)
{
  vector<SumOutOperator*> validOps;
  set<unsigned> allGroups;
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    assert (*it);
    const ProbFormulas& formulas = (*it)->formulas();
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
  int idx = (*pfIters[0])->indexOfFormulaWithGroup (group_);
  ProbFormula f = (*pfIters[0])->formula (idx);
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
    int fIdx = (*pfIters[i])->indexOfFormulaWithGroup (group);
    if ((*pfIters[i])->formulas()[fIdx].contains (
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
  int fIdx = g->indexOfFormulaWithGroup (group);
  const ProbFormula& formula = g->formula (fIdx);
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
  int fIdx = (*pfIter_)->indexOfFormulaWithLogVar (X_);
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
    Parfactors pfs = FoveSolver::countNormalize (*pfIter_, X_);
    for (unsigned i = 0; i  < pfs.size(); i++) {
      unsigned condCount = pfs[i]->constr()->getConditionalCount (X_);
      bool cartProduct   = pfs[i]->constr()->isCarteesianProduct (
          (*pfIter_)->countedLogVars() | X_);
      if (condCount > 1 && cartProduct) {
        pfs[i]->countConvert (X_);
      }
      pfList_.add (pfs[i]);
    }
    pfList_.deleteAndRemove (pfIter_);
    pfList_.shatter();
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
  ss << (*pfIter_)->getHeaderString();
  ss << " [cost=" << getCost() << "]" << endl;
  Parfactors pfs = FoveSolver::countNormalize (*pfIter_, X_);
  if ((*pfIter_)->constr()->isCountNormalized (X_) == false) {
    for (unsigned i = 0; i < pfs.size(); i++) {
      ss << "   º " << pfs[i]->getHeaderString() << endl;
    }
  }
  return ss.str();
}



bool
CountingOperator::validOp (Parfactor* g, LogVar X)
{
  if (g->nrFormulas (X) != 1) {
    return false;
  }
  int fIdx = g->indexOfFormulaWithLogVar (X);
  if (g->formulas()[fIdx].isCounting()) {
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
    int fIdx = (*pfIter_)->indexOfFormulaWithLogVar (X_);
    unsigned currSize  = (*pfIter_)->size();
    unsigned nrHists   = (*pfIter_)->range (fIdx);
    unsigned range     = (*pfIter_)->formula(fIdx).range();
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
  if (countedLv) {
    (*pfIter_)->fullExpand (X_);
    (*pfIter_)->setNewGroups();
    pfList_.shatter();
  } else {
    ConstraintTrees cts = (*pfIter_)->constr()->ground (X_);
    for (unsigned i = 0; i < cts.size(); i++) {
      Parfactor* newPf = new Parfactor (*pfIter_, cts[i]);
      pfList_.add (newPf);
    }
    pfList_.deleteAndRemove (pfIter_);
    pfList_.shatter();
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
  ss << X_ << " in " << (*pfIter_)->getHeaderString();
  ss << " [cost=" << getCost() << "]" << endl;
  return ss.str();
}



FoveSolver::FoveSolver (const ParfactorList* pfList)
{
  for (ParfactorList::const_iterator it = pfList->begin();
       it != pfList->end();
       it ++) {
    pfList_.addShattered (new Parfactor (**it));
  }
}



Params
FoveSolver::getPosterioriOf (const Ground& query)
{
  return getJointDistributionOf ({query});
}



Params
FoveSolver::getJointDistributionOf (const Grounds& query)
{
  shatterAgainstQuery (query);
  runSolver (query);
  (*pfList_.begin())->normalize();
  Params params = (*pfList_.begin())->params();
  if (Globals::logDomain) {
    Util::fromLog (params);
  }
  delete *pfList_.begin();
  return params;
}



void
FoveSolver::absorveEvidence (
    ParfactorList& pfList,
    const ObservedFormulas& obsFormulas)
{
  ParfactorList::iterator it = pfList.begin();
  while (it != pfList.end()) {
    bool increment = true;
    for (unsigned i = 0; i < obsFormulas.size(); i++) {
      if (absorved (pfList, it, obsFormulas[i])) {
        it = pfList.deleteAndRemove (it);
        increment = false;
        break;
      }      
    }
    if (increment) {
      ++ it;
    }
  }
  pfList.shatter();
  if (obsFormulas.empty() == false) {
    cout << "*******************************************************" << endl;
    cout << "AFTER EVIDENCE ABSORVED" << endl;
    for (unsigned i = 0; i < obsFormulas.size(); i++) {
      cout << " -> " << *obsFormulas[i] << endl;
    }
    cout << "*******************************************************" << endl;
  }
  pfList.print();
}



Parfactors
FoveSolver::countNormalize (
    Parfactor* g,
    const LogVarSet& set)
{
  if (set.empty()) {
    assert (false); // TODO
    return {};
  }
  Parfactors normPfs;
  ConstraintTrees normCts = g->constr()->countNormalize (set);
  for (unsigned i = 0; i < normCts.size(); i++) {
    normPfs.push_back (new Parfactor (g, normCts[i]));
  }
  return normPfs;
}



void
FoveSolver::runSolver (const Grounds& query)
{
  while (true) {
    cout << "---------------------------------------------------" << endl;
    pfList_.print();
    LiftedOperator::printValidOps (pfList_, query);
    LiftedOperator* op = getBestOperation (query);
    if (op == 0) {
      break;
    }
    cout << "best operation: " << op->toString() << endl;
    op->apply();
  }
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



bool
FoveSolver::allEliminated (const Grounds&)
{
  ParfactorList::iterator pfIter = pfList_.begin();
  while (pfIter != pfList_.end()) {
    const ProbFormulas formulas = (*pfIter)->formulas();
    for (unsigned i = 0; i < formulas.size(); i++) {
      //bool toElim = false; 
      //for (unsigned j = 0; j < queries.size(); j++) {
      //  if ((*pfIter)->containsGround (queries[i]) == false) {
      //   return
      //  }
    }
    ++ pfIter;
  }
  return false;
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
  return bestOp;
}



void
FoveSolver::shatterAgainstQuery (const Grounds& query)
{
  // return;
  for (unsigned i = 0; i < query.size(); i++) {
    if (query[i].isAtom()) {
      continue;
    }
    ParfactorList pfListCopy = pfList_;
    pfList_.clear();
    for (ParfactorList::iterator it = pfListCopy.begin(); 
        it != pfListCopy.end(); ++ it) {
      Parfactor* pf = *it; 
      if (pf->containsGround (query[i])) {
        std::pair<ConstraintTree*, ConstraintTree*> split = 
            pf->constr()->split (query[i].args(), query[i].arity());
        ConstraintTree* commCt = split.first;
        ConstraintTree* exclCt = split.second;
        pfList_.add (new Parfactor (pf, commCt));
        if (exclCt->empty() == false) {
          pfList_.add (new Parfactor (pf, exclCt));
        } else {
          delete exclCt;
        }
        delete pf;
      } else {
        pfList_.add (pf);
      }
    }
    pfList_.shatter();
  }
  cout << endl;
  cout << "*******************************************************" << endl;
  cout << "SHATTERED AGAINST THE QUERY" << endl;
  for (unsigned i = 0; i < query.size(); i++) {
    cout << " -> " << query[i] << endl;
  }
  cout << "*******************************************************" << endl;
  pfList_.print();
}



bool
FoveSolver::absorved (
    ParfactorList& pfList,
    ParfactorList::iterator pfIter,
    const ObservedFormula* obsFormula)
{
  Parfactors absorvedPfs;
  Parfactor* g = *pfIter;
  const ProbFormulas& formulas = g->formulas();
  for (unsigned i = 0; i < formulas.size(); i++) {
    if (obsFormula->functor() == formulas[i].functor() &&
        obsFormula->arity()   == formulas[i].arity()) {

      if (obsFormula->isAtom()) {
        if (formulas.size() > 1) {
          g->absorveEvidence (i, obsFormula->evidence());
        } else {
          return true;
        }
      } 

      g->constr()->moveToTop (formulas[i].logVars());
      std::pair<ConstraintTree*, ConstraintTree*> res
          = g->constr()->split (obsFormula->constr(), formulas[i].arity());
      ConstraintTree* commCt = res.first;
      ConstraintTree* exclCt = res.second;

      if (commCt->empty()) {
        delete commCt;
        delete exclCt;
        continue;
      }

      if (exclCt->empty() == false) {
        pfList.add (new Parfactor (g, exclCt));
      } else {
        delete exclCt;
      }

      if (formulas.size() > 1) {
        LogVarSet excl = g->exclusiveLogVars (i);
        Parfactors countNormPfs = countNormalize (g, excl);
        for (unsigned j = 0; j < countNormPfs.size(); j++) {
          countNormPfs[j]->absorveEvidence (i, obsFormula->evidence());
          absorvedPfs.push_back (countNormPfs[j]);
        }
      } else {
        delete commCt;
      }
      return true;

    }
  }
  return false;
}



bool
FoveSolver::proper (
    const ProbFormula&  f1,
    ConstraintTree*     c1,
    const ProbFormula&  f2,
    ConstraintTree*     c2)
{
  return disjoint  (f1, c1, f2, c2)
      || identical (f1, c1, f2, c2);
}



bool
FoveSolver::identical (
    const ProbFormula&  f1,
    ConstraintTree*     c1,
    const ProbFormula&  f2,
    ConstraintTree*     c2)
{
  if (f1.sameSkeletonAs (f2) == false) {
    return false;
  }
  c1->moveToTop (f1.logVars());
  c2->moveToTop (f2.logVars());
  return ConstraintTree::identical (
      c1, c2, f1.logVars().size());
}



bool
FoveSolver::disjoint (
    const ProbFormula&  f1,
    ConstraintTree*     c1,
    const ProbFormula&  f2,
    ConstraintTree*     c2)
{
  if (f1.sameSkeletonAs (f2) == false) {
    return true;
  }
  c1->moveToTop (f1.logVars());
  c2->moveToTop (f2.logVars());
  return ConstraintTree::overlap (
      c1, c2, f1.arity()) == false;
}

