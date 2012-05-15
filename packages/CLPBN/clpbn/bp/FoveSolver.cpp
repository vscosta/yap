
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
  vector<ProductOperator*> multOps;

  multOps = ProductOperator::getValidOps (pfList);
  validOps.insert (validOps.end(), multOps.begin(), multOps.end());

  if (Globals::verbosity > 1 || multOps.empty()) {
    vector<SumOutOperator*>   sumOutOps;
    vector<CountingOperator*> countOps;
    vector<GroundOperator*>   groundOps;
    sumOutOps = SumOutOperator::getValidOps (pfList, query);
    countOps  = CountingOperator::getValidOps (pfList);
    groundOps = GroundOperator::getValidOps (pfList);
    validOps.insert (validOps.end(), sumOutOps.begin(), sumOutOps.end());
    validOps.insert (validOps.end(), countOps.begin(),  countOps.end());
    validOps.insert (validOps.end(), groundOps.begin(), groundOps.end());
  }

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
    cout << "-> " << validOps[i]->toString();
    delete validOps[i];
  }
}



vector<ParfactorList::iterator>
LiftedOperator::getParfactorsWithGroup (
    ParfactorList& pfList, unsigned group)
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



double
ProductOperator::getLogCost (void)
{
  return std::log (0.0);
}



void
ProductOperator::apply (void)
{
  Parfactor* g1 = *g1_;
  Parfactor* g2 = *g2_;
  g1->multiply (*g2);
  pfList_.remove (g1_);
  pfList_.removeAndDelete (g2_);
  pfList_.addShattered (g1);
}



vector<ProductOperator*>
ProductOperator::getValidOps (ParfactorList& pfList)
{
  vector<ProductOperator*> validOps;
  ParfactorList::iterator it1 = pfList.begin();
  ParfactorList::iterator penultimate = -- pfList.end();
  set<Parfactor*> pfs;
  while (it1 != penultimate) {
    if (Util::contains (pfs, *it1)) {
      ++ it1;
      continue;
    }
    ParfactorList::iterator it2 = it1;
    ++ it2;
    while (it2 != pfList.end()) {
      if (Util::contains (pfs, *it2)) {
        ++ it2;
        continue;
      } else {
        if (validOp (*it1, *it2)) {
          pfs.insert (*it1);
          pfs.insert (*it2);
          validOps.push_back (new ProductOperator (
              it1, it2, pfList));
          if (Globals::verbosity < 2) {
            return validOps;
          }
          break;
        }
      }
      ++ it2;
    }
    ++ it1;
  }
  return validOps;
}



string
ProductOperator::toString (void)
{
  stringstream ss;
  ss << "just multiplicate " ;
  ss << (*g1_)->getAllGroups();
  ss << " x " ; 
  ss << (*g2_)->getAllGroups();
  ss << " [cost=" << std::exp (getLogCost()) << "]" << endl;
  return ss.str();
}



bool
ProductOperator::validOp (Parfactor* g1, Parfactor* g2)
{
  TinySet<unsigned> g1_gs (g1->getAllGroups());
  TinySet<unsigned> g2_gs (g2->getAllGroups());
  if (g1_gs.contains (g2_gs) || g2_gs.contains (g1_gs)) {
    TinySet<unsigned> intersect = g1_gs & g2_gs;
    for (unsigned i = 0; i < intersect.size(); i++) {
      if (g1->nrFormulasWithGroup (intersect[i]) != 1 ||
          g2->nrFormulasWithGroup (intersect[i]) != 1) {
        return false;
      }
      int idx1 = g1->indexOfGroup (intersect[i]);
      int idx2 = g2->indexOfGroup (intersect[i]);
      if (g1->range (idx1) != g2->range (idx2)) { 
        return false;
      }
    }
    return Parfactor::canMultiply (g1, g2);
  }
  return false;
}



double
SumOutOperator::getLogCost (void)
{
  TinySet<unsigned> groupSet;
  ParfactorList::const_iterator pfIter = pfList_.begin();
  unsigned nrProdFactors = 0;
  while (pfIter != pfList_.end()) {
    if ((*pfIter)->containsGroup (group_)) {
      vector<unsigned> groups = (*pfIter)->getAllGroups();
      groupSet |= TinySet<unsigned> (groups);
      ++ nrProdFactors;
    }
    ++ pfIter;
  }
  if (nrProdFactors == 1) {
    // best possible case
    return std::log (0.0);
  }
  double cost = 1.0;
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
  return std::log (cost);
}



void
SumOutOperator::apply (void)
{
  vector<ParfactorList::iterator> iters;
  iters = getParfactorsWithGroup (pfList_, group_);
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
  pfIters = getParfactorsWithGroup (pfList_, group_);
  int idx = (*pfIters[0])->indexOfGroup (group_);
  ProbFormula f = (*pfIters[0])->argument (idx);
  TupleSet tupleSet = (*pfIters[0])->constr()->tupleSet (f.logVars());
  ss << "sum out " << f.functor() << "/" << f.arity();
  ss << "|" << tupleSet << " (group " << group_ << ")";
  ss << " [cost=" << std::exp (getLogCost()) << "]" << endl;
  return ss.str();
}



bool
SumOutOperator::validOp (
    unsigned group,
    ParfactorList& pfList,
    const Grounds& query)
{
  vector<ParfactorList::iterator> pfIters;
  pfIters = getParfactorsWithGroup (pfList, group);
  if (isToEliminate (*pfIters[0], group, query) == false) {
    return false;
  }
  int range = -1;
  for (unsigned i = 0; i < pfIters.size(); i++) {
    if ((*pfIters[i])->nrFormulasWithGroup (group) > 1) {
      return false;
    }
    int fIdx = (*pfIters[i])->indexOfGroup (group);
    if ((*pfIters[i])->argument (fIdx).contains (
            (*pfIters[i])->elimLogVars()) == false) {
      return false;
    }
    if (range == -1) {
      range = (*pfIters[i])->range (fIdx);
    } else if ((int)(*pfIters[i])->range (fIdx) != range) {
      return false;
    }
  }
  return true;
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



double
CountingOperator::getLogCost (void)
{
  double cost = 0.0;
  int fIdx = (*pfIter_)->indexOfLogVar (X_);
  unsigned range = (*pfIter_)->range (fIdx);
  unsigned size  = (*pfIter_)->size() / range;
  TinySet<unsigned> counts;
  counts = (*pfIter_)->constr()->getConditionalCounts (X_);
  for (unsigned i = 0; i < counts.size(); i++) {
    cost += size * HistogramSet::nrHistograms (counts[i], range);
  }
  unsigned group = (*pfIter_)->argument (fIdx).group();
  int lvIndex = Util::vectorIndex (
      (*pfIter_)->argument (fIdx).logVars(), X_);
  assert (lvIndex != -1);
  ParfactorList::iterator pfIter = pfList_.begin();
  while (pfIter != pfList_.end()) {
    if (pfIter != pfIter_) {
      int fIdx2 = (*pfIter)->indexOfGroup (group);
      if (fIdx2 != -1) {
        LogVar Y = ((*pfIter)->argument (fIdx2).logVars()[lvIndex]);
        if ((*pfIter)->canCountConvert (Y) == false) {
          // the real cost should be the cost of grounding Y
          cost *= 10.0;
        }
      }
    }
    ++ pfIter;
  }
  return std::log (cost);
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
      } else {
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
  ss << " [cost=" << std::exp (getLogCost()) << "]" << endl;
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
    return g->canCountConvert (X);
  }
  return true;
}



double
GroundOperator::getLogCost (void)
{
  vector<pair<unsigned, unsigned>> affectedFormulas;
  affectedFormulas = getAffectedFormulas();
  // cout << "affected formulas: " ;
  // for (unsigned i = 0; i < affectedFormulas.size(); i++) {
  //  cout << affectedFormulas[i].first  << ":" ;
  //  cout << affectedFormulas[i].second << " " ;
  // }
  // cout << "cost =" ;
  double totalCost = std::log (0.0);
  ParfactorList::iterator pflIt = pfList_.begin();
  while (pflIt != pfList_.end()) {
    Parfactor* pf = *pflIt;
    double reps   = 0.0;
    double pfSize = std::log (pf->size());
    bool willBeAffected = false;
    LogVarSet lvsToGround;
    for (unsigned i = 0; i < affectedFormulas.size(); i++) {
      int fIdx = pf->indexOfGroup (affectedFormulas[i].first);
      if (fIdx != -1) {
        ProbFormula f = pf->argument (fIdx);
        LogVar X      = f.logVars()[affectedFormulas[i].second];
        bool isCountingLv = pf->countedLogVars().contains (X);
        if (isCountingLv) {
          unsigned nrHists   = pf->range (fIdx);
          unsigned nrSymbols = pf->constr()->getConditionalCount (X);
          unsigned range     = pf->argument (fIdx).range();
          double power       = std::log (range) * nrSymbols;
          pfSize = (pfSize - std::log (nrHists)) + power;
        } else {
          if (lvsToGround.contains (X) == false) {
            reps += std::log (pf->constr()->nrSymbols (X));
            lvsToGround.insert (X);
          }
        }
        willBeAffected = true;
      }
    }
    if (willBeAffected) {
      // cout << " + " << std::exp (reps) << "x" << std::exp (pfSize);
      double pfCost = reps + pfSize;
      totalCost = Util::logSum (totalCost, pfCost);
    }
    ++ pflIt;
  }
  // cout << endl;
  return totalCost;
}



void
GroundOperator::apply (void)
{
  // TODO if we update the correct groups  
  // we can skip shattering
  ParfactorList::iterator pfIter;
  pfIter = getParfactorsWithGroup (pfList_, group_).front();
  Parfactor* pf = *pfIter;
  int idx = pf->indexOfGroup (group_);
  ProbFormula f = pf->argument (idx);
  LogVar X      = f.logVars()[lvIndex_];
  bool countedLv = pf->countedLogVars().contains (X);
  pfList_.remove (pfIter);
  if (countedLv) {
    pf->fullExpand (X);
    pfList_.add (pf);
  } else {
    ConstraintTrees cts = pf->constr()->ground (X);
    for (unsigned i = 0; i < cts.size(); i++) {
      pfList_.add (new Parfactor (pf, cts[i]));
    }
    delete pf;
  }
  ParfactorList::iterator pflIt = pfList_.begin();
  while (pflIt != pfList_.end()) {
    (*pflIt)->simplifyGrounds();
    ++ pflIt;
  }
}



vector<GroundOperator*>
GroundOperator::getValidOps (ParfactorList& pfList)
{
  vector<GroundOperator*> validOps;
  set<unsigned> allGroups;
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (unsigned i = 0; i < formulas.size(); i++) {
      if (Util::contains (allGroups, formulas[i].group()) == false) {
        const LogVars& lvs = formulas[i].logVars();
        for (unsigned j = 0; j < lvs.size(); j++) {
          if ((*it)->constr()->isSingleton (lvs[j]) == false) {
            validOps.push_back (new GroundOperator (
                formulas[i].group(), j, pfList));
          }
        }
        allGroups.insert (formulas[i].group());
      }
    }
    ++ it;
  }
  return validOps;
}



string
GroundOperator::toString (void)
{
  stringstream ss;
  vector<ParfactorList::iterator> pfIters;
  pfIters = getParfactorsWithGroup (pfList_, group_);
  Parfactor* pf = *(getParfactorsWithGroup (pfList_, group_).front());
  int idx = pf->indexOfGroup (group_);
  ProbFormula f = pf->argument (idx);
  LogVar lv     = f.logVars()[lvIndex_];
  TupleSet tupleSet = pf->constr()->tupleSet ({lv});
  string pos = "th";
  if (lvIndex_ == 0) {
    pos = "st" ;
  } else if (lvIndex_ == 1) {
    pos = "nd" ;
  } else if (lvIndex_ == 2) {
    pos = "rd" ;
  }
  ss << "grounding " << lvIndex_ + 1 << pos << " log var in " ;
  ss << f.functor() << "/" << f.arity();
  ss << "|" << tupleSet << " (group " << group_ << ")";
  ss << " [cost=" << std::exp (getLogCost()) << "]" << endl;
  return ss.str();
}



vector<pair<unsigned, unsigned>>
GroundOperator::getAffectedFormulas (void)
{
  vector<pair<unsigned, unsigned>> affectedFormulas;
  affectedFormulas.push_back (make_pair (group_, lvIndex_));
  queue<pair<unsigned, unsigned>> q;
  q.push (make_pair (group_, lvIndex_));
  while (q.empty() == false) {
    pair<unsigned, unsigned> front = q.front();
    ParfactorList::iterator pflIt = pfList_.begin();
    while (pflIt != pfList_.end()) {
      int idx = (*pflIt)->indexOfGroup (front.first);
      if (idx != -1) {
        ProbFormula f = (*pflIt)->argument (idx);
        LogVar X      = f.logVars()[front.second];
        const ProbFormulas& fs = (*pflIt)->arguments();
        for (unsigned i = 0; i < fs.size(); i++) {
          if ((int)i != idx && fs[i].contains (X)) {
            pair<unsigned, unsigned> pair = make_pair (
                fs[i].group(), fs[i].indexOf (X));
            if (Util::contains (affectedFormulas, pair) == false) {
              q.push (pair);
              affectedFormulas.push_back (pair);
            }
          }
        }
      }
      ++ pflIt;
    }
    q.pop();
  }
  return affectedFormulas;
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
FoveSolver::printSolverFlags (void) const
{
  stringstream ss;
  ss << "fove [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  cout << ss.str() << endl;
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
  if (Globals::verbosity > 2 && obsFormulas.empty() == false) {
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



Parfactor
FoveSolver::calcGroundMultiplication (Parfactor pf)
{
  LogVarSet lvs = pf.constr()->logVarSet();
  lvs -= pf.constr()->singletons();
  Parfactors newPfs = {new Parfactor (pf)};
  for (unsigned i = 0; i < lvs.size(); i++) {
    Parfactors pfs = newPfs;
    newPfs.clear();
    for (unsigned j = 0; j < pfs.size(); j++) {
      bool countedLv = pfs[j]->countedLogVars().contains (lvs[i]);
      if (countedLv) {
        pfs[j]->fullExpand (lvs[i]);
        newPfs.push_back (pfs[j]);
      } else {
        ConstraintTrees cts = pfs[j]->constr()->ground (lvs[i]);
        for (unsigned k = 0; k < cts.size(); k++) {
          newPfs.push_back (new Parfactor (pfs[j], cts[k]));
        }
        delete pfs[j];
      }
    }
  }
  ParfactorList pfList (newPfs);
  Parfactors groundShatteredPfs (pfList.begin(),pfList.end());
  for (unsigned i = 1; i < groundShatteredPfs.size(); i++) {
     groundShatteredPfs[0]->multiply (*groundShatteredPfs[i]);
  }
  return Parfactor (*groundShatteredPfs[0]);
}



void
FoveSolver::runSolver (const Grounds& query)
{
  largestCost_ = std::log (0);
  shatterAgainstQuery (query);
  runWeakBayesBall (query);
  while (true) {
    if (Globals::verbosity > 2) {
      Util::printDashedLine();
      pfList_.print();
      if (Globals::verbosity > 3) {
        LiftedOperator::printValidOps (pfList_, query);
      }
    }
    LiftedOperator* op = getBestOperation (query);
    if (op == 0) {
      break;
    }
    if (Globals::verbosity > 1) {
      cout << "best operation: " << op->toString();
      if (Globals::verbosity > 2) {
        cout << endl;
      }
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
  if (Globals::verbosity > 0) {
    cout << "largest cost = " << std::exp (largestCost_) << endl;
    cout << endl;
  }
  (*pfList_.begin())->simplifyGrounds();
  (*pfList_.begin())->reorderAccordingGrounds (query);
}



LiftedOperator*
FoveSolver::getBestOperation (const Grounds& query)
{
  double bestCost = 0.0;
  LiftedOperator* bestOp = 0;
  vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList_, query);
  for (unsigned i = 0; i < validOps.size(); i++) {
    double cost = validOps[i]->getLogCost();
    if ((bestOp == 0) || (cost < bestCost)) {
      bestOp   = validOps[i];
      bestCost = cost;
    } 
  }
  if (bestCost > largestCost_) {
    largestCost_ = bestCost;
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
  bool foundNotRequired = false;
  while (it != pfList_.end()) {
    if (Util::contains (requiredPfs, *it) == false) {
      if (Globals::verbosity > 2) {
        if (foundNotRequired == false) {
          Util::printHeader ("PARFACTORS TO DISCARD");
          foundNotRequired = true;
        }
        (*it)->print();
      }
      it = pfList_.removeAndDelete (it);
    } else {
      ++ it;
    }
  }
}



void
FoveSolver::shatterAgainstQuery (const Grounds& query)
{
  for (unsigned i = 0; i < query.size(); i++) {
    if (query[i].isAtom()) {
      continue;
    }
    bool found = false;
    Parfactors newPfs;
    ParfactorList::iterator it = pfList_.begin();
    while (it != pfList_.end()) {
      if ((*it)->containsGround (query[i])) {
        found = true;
        std::pair<ConstraintTree*, ConstraintTree*> split;
        LogVars queryLvs (
            (*it)->constr()->logVars().begin(),
            (*it)->constr()->logVars().begin() + query[i].arity());
        split = (*it)->constr()->split (query[i].args());
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
    if (found == false) {
      cerr << "error: could not find a parfactor with ground " ;
      cerr << "`" << query[i] << "'" << endl;
      exit (0);	
    }
    pfList_.add (newPfs);
  }
  if (Globals::verbosity > 2) {
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
      std::pair<ConstraintTree*, ConstraintTree*> res;
      res = g->constr()->split (
          formulas[i].logVars(),
          &(obsFormula.constr()),
          obsFormula.constr().logVars());
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

