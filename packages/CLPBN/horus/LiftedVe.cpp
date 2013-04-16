#include <cassert>

#include <vector>
#include <set>
#include <queue>
#include <algorithm>
#include <string>
#include <iostream>
#include <sstream>

#include "LiftedVe.h"
#include "LiftedOperations.h"
#include "Histogram.h"
#include "Util.h"


namespace Horus {

class LiftedOperator {
  public:
    virtual ~LiftedOperator() { }

    virtual double getLogCost() = 0;

    virtual void apply() = 0;

    virtual std::string toString() = 0;

    static std::vector<LiftedOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    static void printValidOps (ParfactorList&, const Grounds&);

    static std::vector<ParfactorList::iterator> getParfactorsWithGroup (
        ParfactorList&, PrvGroup group);

  private:
    DISALLOW_ASSIGN (LiftedOperator);
};



class ProductOperator : public LiftedOperator {
  public:
    ProductOperator (
        ParfactorList::iterator g1,
        ParfactorList::iterator g2,
        ParfactorList& pfList)
        : g1_(g1), g2_(g2), pfList_(pfList) { }

    double getLogCost();

    void apply();

    static std::vector<ProductOperator*> getValidOps (ParfactorList&);

    std::string toString();

  private:
    static bool validOp (Parfactor*, Parfactor*);

    ParfactorList::iterator  g1_;
    ParfactorList::iterator  g2_;
    ParfactorList&           pfList_;

    DISALLOW_COPY_AND_ASSIGN (ProductOperator);
};



class SumOutOperator : public LiftedOperator {
  public:
    SumOutOperator (PrvGroup group, ParfactorList& pfList)
        : group_(group), pfList_(pfList) { }

    double getLogCost();

    void apply();

    static std::vector<SumOutOperator*> getValidOps (
        ParfactorList&, const Grounds&);

    std::string toString();

  private:
    static bool validOp (PrvGroup, ParfactorList&, const Grounds&);

    static bool isToEliminate (Parfactor*, PrvGroup, const Grounds&);

    PrvGroup        group_;
    ParfactorList&  pfList_;

    DISALLOW_COPY_AND_ASSIGN (SumOutOperator);
};



class CountingOperator : public LiftedOperator {
  public:
    CountingOperator (
        ParfactorList::iterator pfIter,
        LogVar X,
        ParfactorList& pfList)
        : pfIter_(pfIter), X_(X), pfList_(pfList) { }

    double getLogCost();

    void apply();

    static std::vector<CountingOperator*> getValidOps (ParfactorList&);

    std::string toString();

  private:
    static bool validOp (Parfactor*, LogVar);

    ParfactorList::iterator  pfIter_;
    LogVar                   X_;
    ParfactorList&           pfList_;

    DISALLOW_COPY_AND_ASSIGN (CountingOperator);
};



class GroundOperator : public LiftedOperator {
  public:
    GroundOperator (
        PrvGroup group,
        unsigned lvIndex,
        ParfactorList& pfList)
        : group_(group), lvIndex_(lvIndex), pfList_(pfList) { }

    double getLogCost();

    void apply();

    static std::vector<GroundOperator*> getValidOps (ParfactorList&);

    std::string toString();

  private:
    std::vector<std::pair<PrvGroup, unsigned>> getAffectedFormulas();

    PrvGroup        group_;
    unsigned        lvIndex_;
    ParfactorList&  pfList_;

    DISALLOW_COPY_AND_ASSIGN (GroundOperator);
};



std::vector<LiftedOperator*>
LiftedOperator::getValidOps (
    ParfactorList& pfList,
    const Grounds& query)
{
  std::vector<LiftedOperator*>  validOps;
  std::vector<ProductOperator*> multOps;

  multOps = ProductOperator::getValidOps (pfList);
  validOps.insert (validOps.end(), multOps.begin(), multOps.end());

  if (Globals::verbosity > 1 || multOps.empty()) {
    std::vector<SumOutOperator*>   sumOutOps;
    std::vector<CountingOperator*> countOps;
    std::vector<GroundOperator*>   groundOps;
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
  std::vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList, query);
  for (size_t i = 0; i < validOps.size(); i++) {
    std::cout << "-> " << validOps[i]->toString();
    delete validOps[i];
  }
}



std::vector<ParfactorList::iterator>
LiftedOperator::getParfactorsWithGroup (
    ParfactorList& pfList, PrvGroup group)
{
  std::vector<ParfactorList::iterator> iters;
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
ProductOperator::getLogCost()
{
  return std::log (0.0);
}



void
ProductOperator::apply()
{
  Parfactor* g1 = *g1_;
  Parfactor* g2 = *g2_;
  g1->multiply (*g2);
  pfList_.remove (g1_);
  pfList_.removeAndDelete (g2_);
  pfList_.addShattered (g1);
}



std::vector<ProductOperator*>
ProductOperator::getValidOps (ParfactorList& pfList)
{
  std::vector<ProductOperator*> validOps;
  ParfactorList::iterator it1 = pfList.begin();
  ParfactorList::iterator penultimate = -- pfList.end();
  std::set<Parfactor*> pfs;
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



std::string
ProductOperator::toString()
{
  std::stringstream ss;
  ss << "just multiplicate " ;
  ss << (*g1_)->getAllGroups();
  ss << " x " ;
  ss << (*g2_)->getAllGroups();
  ss << " [cost=" << std::exp (getLogCost()) << "]" << std::endl;
  return ss.str();
}



bool
ProductOperator::validOp (Parfactor* g1, Parfactor* g2)
{
  TinySet<PrvGroup> g1_gs (g1->getAllGroups());
  TinySet<PrvGroup> g2_gs (g2->getAllGroups());
  if (g1_gs.contains (g2_gs) || g2_gs.contains (g1_gs)) {
    TinySet<PrvGroup> intersect = g1_gs & g2_gs;
    for (size_t i = 0; i < intersect.size(); i++) {
      if (g1->nrFormulasWithGroup (intersect[i]) != 1 ||
          g2->nrFormulasWithGroup (intersect[i]) != 1) {
        return false;
      }
      size_t idx1 = g1->indexOfGroup (intersect[i]);
      size_t idx2 = g2->indexOfGroup (intersect[i]);
      if (g1->range (idx1) != g2->range (idx2)) {
        return false;
      }
    }
    return Parfactor::canMultiply (g1, g2);
  }
  return false;
}



double
SumOutOperator::getLogCost()
{
  TinySet<PrvGroup> groupSet;
  ParfactorList::const_iterator pfIter = pfList_.begin();
  unsigned nrProdFactors = 0;
  while (pfIter != pfList_.end()) {
    if ((*pfIter)->containsGroup (group_)) {
      std::vector<PrvGroup> groups = (*pfIter)->getAllGroups();
      groupSet |= TinySet<PrvGroup> (groups);
      ++ nrProdFactors;
    }
    ++ pfIter;
  }
  if (nrProdFactors == 1) {
    // best possible case
    return std::log (0.0);
  }
  double cost = 1.0;
  for (size_t i = 0; i < groupSet.size(); i++) {
    pfIter = pfList_.begin();
    while (pfIter != pfList_.end()) {
      if ((*pfIter)->containsGroup (groupSet[i])) {
        size_t idx = (*pfIter)->indexOfGroup (groupSet[i]);
        cost *= (*pfIter)->range (idx);
        break;
      }
      ++ pfIter;
    }
  }
  return std::log (cost);
}



void
SumOutOperator::apply()
{
  std::vector<ParfactorList::iterator> iters;
  iters = getParfactorsWithGroup (pfList_, group_);
  Parfactor* product = *(iters[0]);
  pfList_.remove (iters[0]);
  for (size_t i = 1; i < iters.size(); i++) {
    product->multiply (**(iters[i]));
    pfList_.removeAndDelete (iters[i]);
  }
  if (product->nrArguments() == 1) {
    delete product;
    return;
  }
  size_t fIdx = product->indexOfGroup (group_);
  LogVarSet excl = product->exclusiveLogVars (fIdx);
  if (product->constr()->isCountNormalized (excl)) {
    product->sumOutIndex (fIdx);
    pfList_.addShattered (product);
  } else {
    Parfactors pfs = LiftedOperations::countNormalize (product, excl);
    for (size_t i = 0; i < pfs.size(); i++) {
      pfs[i]->sumOutIndex (fIdx);
      pfList_.add (pfs[i]);
    }
    delete product;
  }
}



std::vector<SumOutOperator*>
SumOutOperator::getValidOps (
    ParfactorList& pfList,
    const Grounds& query)
{
  std::vector<SumOutOperator*> validOps;
  std::set<PrvGroup> allGroups;
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      allGroups.insert (formulas[i].group());
    }
    ++ it;
  }
  std::set<PrvGroup>::const_iterator groupIt = allGroups.begin();
  while (groupIt != allGroups.end()) {
    if (validOp (*groupIt, pfList, query)) {
      validOps.push_back (new SumOutOperator (*groupIt, pfList));
    }
    ++ groupIt;
  }
  return validOps;
}



std::string
SumOutOperator::toString()
{
  std::stringstream ss;
  std::vector<ParfactorList::iterator> pfIters;
  pfIters = getParfactorsWithGroup (pfList_, group_);
  size_t idx = (*pfIters[0])->indexOfGroup (group_);
  ProbFormula f = (*pfIters[0])->argument (idx);
  TupleSet tupleSet = (*pfIters[0])->constr()->tupleSet (f.logVars());
  ss << "sum out " << f.functor() << "/" << f.arity();
  ss << "|" << tupleSet << " (group " << group_ << ")";
  ss << " [cost=" << std::exp (getLogCost()) << "]" << std::endl;
  return ss.str();
}



bool
SumOutOperator::validOp (
    PrvGroup group,
    ParfactorList& pfList,
    const Grounds& query)
{
  std::vector<ParfactorList::iterator> pfIters;
  pfIters = getParfactorsWithGroup (pfList, group);
  if (isToEliminate (*pfIters[0], group, query) == false) {
    return false;
  }
  int range = -1;
  for (size_t i = 0; i < pfIters.size(); i++) {
    if ((*pfIters[i])->nrFormulasWithGroup (group) > 1) {
      return false;
    }
    size_t fIdx = (*pfIters[i])->indexOfGroup (group);
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
   PrvGroup group,
   const Grounds& query)
{
  size_t fIdx = g->indexOfGroup (group);
  const ProbFormula& formula = g->argument (fIdx);
  bool toElim = true;
  for (size_t i = 0; i < query.size(); i++) {
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
CountingOperator::getLogCost()
{
  double cost = 0.0;
  size_t fIdx = (*pfIter_)->indexOfLogVar (X_);
  unsigned range = (*pfIter_)->range (fIdx);
  unsigned size  = (*pfIter_)->size() / range;
  TinySet<unsigned> counts;
  counts = (*pfIter_)->constr()->getConditionalCounts (X_);
  for (size_t i = 0; i < counts.size(); i++) {
    cost += size * HistogramSet::nrHistograms (counts[i], range);
  }
  PrvGroup group = (*pfIter_)->argument (fIdx).group();
  size_t lvIndex = Util::indexOf (
      (*pfIter_)->argument (fIdx).logVars(), X_);
  assert (lvIndex != (*pfIter_)->argument (fIdx).logVars().size());
  ParfactorList::iterator pfIter = pfList_.begin();
  while (pfIter != pfList_.end()) {
    if (pfIter != pfIter_) {
      size_t fIdx2 = (*pfIter)->indexOfGroup (group);
      if (fIdx2 != (*pfIter)->nrArguments()) {
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
CountingOperator::apply()
{
  if ((*pfIter_)->constr()->isCountNormalized (X_)) {
    (*pfIter_)->countConvert (X_);
  } else {
    Parfactor* pf = *pfIter_;
    pfList_.remove (pfIter_);
    Parfactors pfs = LiftedOperations::countNormalize (pf, X_);
    for (size_t i = 0; i  < pfs.size(); i++) {
      unsigned condCount = pfs[i]->constr()->getConditionalCount (X_);
      bool cartProduct   = pfs[i]->constr()->isCartesianProduct (
          pfs[i]->countedLogVars() | X_);
      if (condCount > 1 && cartProduct) {
        pfs[i]->countConvert (X_);
      }
      pfList_.add (pfs[i]);
    }
    delete pf;
  }
}



std::vector<CountingOperator*>
CountingOperator::getValidOps (ParfactorList& pfList)
{
  std::vector<CountingOperator*> validOps;
  ParfactorList::iterator it = pfList.begin();
  while (it != pfList.end()) {
    LogVarSet candidates = (*it)->uncountedLogVars();
    for (size_t i = 0; i < candidates.size(); i++) {
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



std::string
CountingOperator::toString()
{
  std::stringstream ss;
  ss << "count convert " << X_ << " in " ;
  ss << (*pfIter_)->getLabel();
  ss << " [cost=" << std::exp (getLogCost()) << "]" << std::endl;
  Parfactors pfs = LiftedOperations::countNormalize (*pfIter_, X_);
  if ((*pfIter_)->constr()->isCountNormalized (X_) == false) {
    for (size_t i = 0; i < pfs.size(); i++) {
      ss << "   º " << pfs[i]->getLabel() << std::endl;
    }
  }
  for (size_t i = 0; i < pfs.size(); i++) {
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
  size_t fIdx = g->indexOfLogVar (X);
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
GroundOperator::getLogCost()
{
  std::vector<std::pair<PrvGroup, unsigned>> affectedFormulas;
  affectedFormulas = getAffectedFormulas();
  // std::cout << "affected formulas: " ;
  // for (size_t i = 0; i < affectedFormulas.size(); i++) {
  //  std::cout << affectedFormulas[i].first  << ":" ;
  //  std::cout << affectedFormulas[i].second << " " ;
  // }
  // std::cout << "cost =" ;
  double totalCost = std::log (0.0);
  ParfactorList::iterator pflIt = pfList_.begin();
  while (pflIt != pfList_.end()) {
    Parfactor* pf = *pflIt;
    double reps   = 0.0;
    double pfSize = std::log (pf->size());
    bool willBeAffected = false;
    LogVarSet lvsToGround;
    for (size_t i = 0; i < affectedFormulas.size(); i++) {
      size_t fIdx = pf->indexOfGroup (affectedFormulas[i].first);
      if (fIdx != pf->nrArguments()) {
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
      // std::cout << " + " << std::exp (reps) << "x" << std::exp (pfSize);
      double pfCost = reps + pfSize;
      totalCost = Util::logSum (totalCost, pfCost);
    }
    ++ pflIt;
  }
  // std::cout << std::endl;
  return totalCost + 3;
}



void
GroundOperator::apply()
{
  ParfactorList::iterator pfIter;
  pfIter = getParfactorsWithGroup (pfList_, group_).front();
  Parfactor* pf = *pfIter;
  size_t idx = pf->indexOfGroup (group_);
  ProbFormula f = pf->argument (idx);
  LogVar X      = f.logVars()[lvIndex_];
  bool countedLv = pf->countedLogVars().contains (X);
  pfList_.remove (pfIter);
  if (countedLv) {
    pf->fullExpand (X);
    pfList_.add (pf);
  } else {
    ConstraintTrees cts = pf->constr()->ground (X);
    for (size_t i = 0; i < cts.size(); i++) {
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



std::vector<GroundOperator*>
GroundOperator::getValidOps (ParfactorList& pfList)
{
  std::vector<GroundOperator*> validOps;
  std::set<PrvGroup> allGroups;
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      if (Util::contains (allGroups, formulas[i].group()) == false) {
        const LogVars& lvs = formulas[i].logVars();
        for (size_t j = 0; j < lvs.size(); j++) {
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



std::string
GroundOperator::toString()
{
  std::stringstream ss;
  std::vector<ParfactorList::iterator> pfIters;
  pfIters = getParfactorsWithGroup (pfList_, group_);
  Parfactor* pf = *(getParfactorsWithGroup (pfList_, group_).front());
  size_t idx = pf->indexOfGroup (group_);
  ProbFormula f = pf->argument (idx);
  LogVar lv     = f.logVars()[lvIndex_];
  TupleSet tupleSet = pf->constr()->tupleSet ({lv});
  std::string pos = "th";
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
  ss << " [cost=" << std::exp (getLogCost()) << "]" << std::endl;
  return ss.str();
}



std::vector<std::pair<PrvGroup, unsigned>>
GroundOperator::getAffectedFormulas()
{
  std::vector<std::pair<PrvGroup, unsigned>> affectedFormulas;
  affectedFormulas.push_back (std::make_pair (group_, lvIndex_));
  std::queue<std::pair<PrvGroup, unsigned>> q;
  q.push (std::make_pair (group_, lvIndex_));
  while (q.empty() == false) {
    std::pair<PrvGroup, unsigned> front = q.front();
    ParfactorList::iterator pflIt = pfList_.begin();
    while (pflIt != pfList_.end()) {
      size_t idx = (*pflIt)->indexOfGroup (front.first);
      if (idx != (*pflIt)->nrArguments()) {
        ProbFormula f = (*pflIt)->argument (idx);
        LogVar X      = f.logVars()[front.second];
        const ProbFormulas& fs = (*pflIt)->arguments();
        for (size_t i = 0; i < fs.size(); i++) {
          if (i != idx && fs[i].contains (X)) {
            std::pair<PrvGroup, unsigned> pair = std::make_pair (
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
LiftedVe::solveQuery (const Grounds& query)
{
  assert (query.empty() == false);
  pfList_ = parfactorList;
  runSolver (query);
  (*pfList_.begin())->normalize();
  Params params = (*pfList_.begin())->params();
  if (Globals::logDomain) {
    Util::exp (params);
  }
  return params;
}



void
LiftedVe::printSolverFlags() const
{
  std::stringstream ss;
  ss << "lve [" ;
  ss << "log_domain=" << Util::toString (Globals::logDomain);
  ss << "]" ;
  std::cout << ss.str() << std::endl;
}



void
LiftedVe::runSolver (const Grounds& query)
{
  largestCost_ = std::log (0);
  LiftedOperations::shatterAgainstQuery (pfList_, query);
  LiftedOperations::runWeakBayesBall (pfList_, query);
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
      std::cout << "best operation: " << op->toString();
      if (Globals::verbosity > 2) {
        std::cout << std::endl;
      }
    }
    op->apply();
    delete op;
  }
  assert (pfList_.size() > 0);
  if (pfList_.size() > 1) {
    ParfactorList::iterator pfIter = pfList_.begin();
    ++ pfIter;
    while (pfIter != pfList_.end()) {
      (*pfList_.begin())->multiply (**pfIter);
      ++ pfIter;
    }
  }
  if (Globals::verbosity > 0) {
    std::cout << "largest cost = " << std::exp (largestCost_);
    std::cout << std::endl;
    std::cout << std::endl;
  }
  (*pfList_.begin())->simplifyGrounds();
  (*pfList_.begin())->reorderAccordingGrounds (query);
}



LiftedOperator*
LiftedVe::getBestOperation (const Grounds& query)
{
  double bestCost = 0.0;
  LiftedOperator* bestOp = 0;
  std::vector<LiftedOperator*> validOps;
  validOps = LiftedOperator::getValidOps (pfList_, query);
  for (size_t i = 0; i < validOps.size(); i++) {
    double cost = validOps[i]->getLogCost();
    if (!bestOp || cost < bestCost) {
      bestOp   = validOps[i];
      bestCost = cost;
    }
  }
  if (bestCost > largestCost_) {
    largestCost_ = bestCost;
  }
  for (size_t i = 0; i < validOps.size(); i++) {
    if (validOps[i] != bestOp) {
      delete validOps[i];
    }
  }
  return bestOp;
}

}  // namespace Horus

