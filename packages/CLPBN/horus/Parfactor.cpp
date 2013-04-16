#include <cassert>

#include <iostream>
#include <sstream>

#include "Parfactor.h"
#include "Histogram.h"
#include "Indexer.h"
#include "Util.h"
#include "Horus.h"


namespace Horus {

Parfactor::Parfactor (
    const ProbFormulas& formulas,
    const Params& params,
    const Tuples& tuples,
    unsigned distId)
{
  args_   = formulas;
  params_ = params;
  distId_ = distId;

  LogVars logVars;
  for (size_t i = 0; i < args_.size(); i++) {
    ranges_.push_back (args_[i].range());
    const LogVars& lvs = args_[i].logVars();
    for (size_t j = 0; j < lvs.size(); j++) {
      if (Util::contains (logVars, lvs[j]) == false) {
        logVars.push_back (lvs[j]);
      }
    }
  }
  LogVar newLv = logVars.size();
  constr_ = new ConstraintTree (logVars, tuples);
  // Change formulas like f(X,X), X in {(p1),(p2),...}
  // to be like f(X,Y), (X,Y) in {(p1,p1),(p2,p2),...}.
  // This will simplify shattering on the constraint tree.
  for (size_t i = 0; i < args_.size(); i++) {
    LogVarSet lvSet;
    LogVars& lvs = args_[i].logVars();
    for (size_t j = 0; j < lvs.size(); j++) {
      if (lvSet.contains (lvs[j]) == false) {
        lvSet |= lvs[j];
      } else {
        constr_->cloneLogVar (lvs[j], newLv);
        lvs[j] = newLv;
        ++ newLv;
      }
    }
  }
  assert (params_.size() == Util::sizeExpected (ranges_));
}



Parfactor::Parfactor (const Parfactor* g, const Tuple& tuple)
{
  args_    = g->arguments();
  params_  = g->params();
  ranges_  = g->ranges();
  distId_  = g->distId();
  constr_  = new ConstraintTree (g->logVars(), {tuple});
  assert (params_.size() == Util::sizeExpected (ranges_));
}



Parfactor::Parfactor (const Parfactor* g, ConstraintTree* constr)
{
  args_    = g->arguments();
  params_  = g->params();
  ranges_  = g->ranges();
  distId_  = g->distId();
  constr_  = constr;
  assert (params_.size() == Util::sizeExpected (ranges_));
}



Parfactor::Parfactor (const Parfactor& g)
{
  args_    = g.arguments();
  params_  = g.params();
  ranges_  = g.ranges();
  distId_  = g.distId();
  constr_  = new ConstraintTree (*g.constr());
  assert (params_.size() == Util::sizeExpected (ranges_));
}



Parfactor::~Parfactor()
{
  delete constr_;
}



LogVarSet
Parfactor::countedLogVars() const
{
  LogVarSet set;
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].isCounting()) {
      set.insert (args_[i].countedLogVar());
    }
  }
  return set;
}



LogVarSet
Parfactor::uncountedLogVars() const
{
  return constr_->logVarSet() - countedLogVars();
}



LogVarSet
Parfactor::elimLogVars() const
{
  LogVarSet requiredToElim = constr_->logVarSet();
  requiredToElim -= constr_->singletons();
  requiredToElim -= countedLogVars();
  return requiredToElim;
}



LogVarSet
Parfactor::exclusiveLogVars (size_t fIdx) const
{
  assert (fIdx < args_.size());
  LogVarSet remaining;
  for (size_t i = 0; i < args_.size(); i++) {
    if (i != fIdx) {
      remaining |= args_[i].logVarSet();
    }
  }
  return args_[fIdx].logVarSet() - remaining;
}



void
Parfactor::sumOutIndex (size_t fIdx)
{
  assert (fIdx < args_.size());
  assert (args_[fIdx].contains (elimLogVars()));

  if (args_[fIdx].isCounting()) {
    unsigned N = constr_->getConditionalCount (
        args_[fIdx].countedLogVar());
    unsigned R = args_[fIdx].range();
    std::vector<double> numAssigns = HistogramSet::getNumAssigns (N, R);
    Indexer indexer (ranges_, fIdx);
    while (indexer.valid()) {
      if (Globals::logDomain) {
        params_[indexer] += numAssigns[ indexer[fIdx] ];
      } else {
        params_[indexer] *= numAssigns[ indexer[fIdx] ];
      }
      ++ indexer;
    }
  }

  LogVarSet excl = exclusiveLogVars (fIdx);
  unsigned exp;
  if (args_[fIdx].isCounting()) {
    // counting log vars were already raised on counting conversion
    exp = constr_->getConditionalCount (excl - args_[fIdx].countedLogVar());
  } else {
    exp = constr_->getConditionalCount (excl);
  }
  constr_->remove (excl);

  GenericFactor<ProbFormula>::sumOutIndex (fIdx);
  LogAware::pow (params_, exp);
}



void
Parfactor::multiply (Parfactor& g)
{
  alignAndExponentiate (this, &g);
  GenericFactor<ProbFormula>::multiply (g);
  constr_->join (g.constr(), true);
  simplifyGrounds();
  assert (constr_->isCartesianProduct (countedLogVars()));
}



bool
Parfactor::canCountConvert (LogVar X)
{
  if (nrFormulas (X) != 1) {
    return false;
  }
  size_t fIdx = indexOfLogVar (X);
  if (args_[fIdx].isCounting()) {
    return false;
  }
  if (constr_->isCountNormalized (X) == false) {
    return false;
  }
  if (constr_->getConditionalCount (X) == 1) {
    return false;
  }
  if (constr_->isCartesianProduct (countedLogVars() | X) == false) {
    return false;
  }
  return true;
}



void
Parfactor::countConvert (LogVar X)
{
  size_t fIdx = indexOfLogVar (X);
  assert (constr_->isCountNormalized (X));
  assert (constr_->getConditionalCount (X) > 1);
  assert (canCountConvert (X));

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = ranges_[fIdx];
  unsigned H = HistogramSet::nrHistograms (N, R);
  std::vector<Histogram> histograms = HistogramSet::getHistograms (N, R);

  Indexer indexer (ranges_);
  std::vector<Params> sumout (params_.size() / R);
  unsigned count = 0;
  while (indexer.valid()) {
    sumout[count].reserve (R);
    for (unsigned r = 0; r < R; r++) {
      sumout[count].push_back (params_[indexer]);
      indexer.incrementDimension (fIdx);
    }
    count ++;
    indexer.resetDimension (fIdx);
    indexer.incrementExceptDimension (fIdx);
  }

  params_.clear();
  params_.reserve (sumout.size() * H);

  ranges_[fIdx] = H;
  MapIndexer mapIndexer (ranges_, fIdx);
  while (mapIndexer.valid()) {
    double prod = LogAware::multIdenty();
    size_t i   = mapIndexer;
    unsigned h = mapIndexer[fIdx];
    for (unsigned r = 0; r < R; r++) {
      if (Globals::logDomain) {
        prod += LogAware::pow (sumout[i][r], histograms[h][r]);
      } else {
        prod *= LogAware::pow (sumout[i][r], histograms[h][r]);
      }
    }
    params_.push_back (prod);
    ++ mapIndexer;
  }
  args_[fIdx].setCountedLogVar (X);
  simplifyCountingFormulas (fIdx);
}



void
Parfactor::expand (LogVar X, LogVar X_new1, LogVar X_new2)
{
  size_t fIdx = indexOfLogVar (X);
  assert (fIdx != args_.size());
  assert (args_[fIdx].isCounting());

  unsigned N1 = constr_->getConditionalCount (X_new1);
  unsigned N2 = constr_->getConditionalCount (X_new2);
  unsigned N  = N1 + N2;
  unsigned R  = args_[fIdx].range();
  unsigned H1 = HistogramSet::nrHistograms (N1, R);
  unsigned H2 = HistogramSet::nrHistograms (N2, R);

  std::vector<Histogram> histograms  = HistogramSet::getHistograms (N,  R);
  std::vector<Histogram> histograms1 = HistogramSet::getHistograms (N1, R);
  std::vector<Histogram> histograms2 = HistogramSet::getHistograms (N2, R);

  std::vector<unsigned> sumIndexes;
  sumIndexes.reserve (H1 * H2);
  for (unsigned i = 0; i < H1; i++) {
    for (unsigned j = 0; j < H2; j++) {
      Histogram hist = histograms1[i];
      hist += histograms2[j];
      sumIndexes.push_back (HistogramSet::findIndex (hist, histograms));
    }
  }

  expandPotential (fIdx, H1 * H2, sumIndexes);

  args_.insert (args_.begin() + fIdx + 1, args_[fIdx]);
  args_[fIdx].rename (X, X_new1);
  args_[fIdx + 1].rename (X, X_new2);
  if (H1 == 2) {
    args_[fIdx].clearCountedLogVar();
  }
  if (H2 == 2) {
    args_[fIdx + 1].clearCountedLogVar();
  }
  ranges_.insert (ranges_.begin() + fIdx + 1, H2);
  ranges_[fIdx] = H1;
}



void
Parfactor::fullExpand (LogVar X)
{
  size_t fIdx = indexOfLogVar (X);
  assert (fIdx != args_.size());
  assert (args_[fIdx].isCounting());

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = args_[fIdx].range();
  std::vector<Histogram> originHists = HistogramSet::getHistograms (N, R);
  std::vector<Histogram> expandHists = HistogramSet::getHistograms (1, R);
  assert (ranges_[fIdx] == originHists.size());
  std::vector<unsigned> sumIndexes;
  sumIndexes.reserve (N * R);

  Ranges expandRanges (N, R);
  Indexer indexer (expandRanges);
  while (indexer.valid()) {
    std::vector<unsigned> hist (R, 0);
    for (unsigned n = 0; n < N; n++) {
      hist += expandHists[indexer[n]];
    }
    sumIndexes.push_back (HistogramSet::findIndex (hist, originHists));
    ++ indexer;
  }

  expandPotential (fIdx, std::pow (R, N), sumIndexes);

  ProbFormula f = args_[fIdx];
  args_.erase (args_.begin() + fIdx);
  ranges_.erase (ranges_.begin() + fIdx);
  LogVars newLvs = constr_->expand (X);
  assert (newLvs.size() == N);
  for (unsigned i = 0 ; i < N; i++) {
    ProbFormula newFormula (f.functor(), f.logVars(), f.range());
    newFormula.rename (X, newLvs[i]);
    args_.insert (args_.begin() + fIdx + i, newFormula);
    ranges_.insert (ranges_.begin() + fIdx + i, R);
  }
}



void
Parfactor::reorderAccordingGrounds (const Grounds& grounds)
{
  ProbFormulas newFormulas;
  for (size_t i = 0; i < grounds.size(); i++) {
    for (size_t j = 0; j < args_.size(); j++) {
      if (grounds[i].functor() == args_[j].functor() &&
          grounds[i].arity()   == args_[j].arity()) {
        constr_->moveToTop (args_[j].logVars());
        if (constr_->containsTuple (grounds[i].args())) {
          newFormulas.push_back (args_[j]);
          break;
        }
      }
    }
    assert (newFormulas.size() == i + 1);
  }
  reorderArguments (newFormulas);
}



void
Parfactor::absorveEvidence (const ProbFormula& formula, unsigned evidence)
{
  size_t fIdx = indexOf (formula);
  assert (fIdx != args_.size());
  LogVarSet excl = exclusiveLogVars (fIdx);
  assert (args_[fIdx].isCounting() == false);
  assert (constr_->isCountNormalized (excl));
  LogAware::pow (params_, constr_->getConditionalCount (excl));
  GenericFactor<ProbFormula>::absorveEvidence (formula, evidence);
  constr_->remove (excl);
}



void
Parfactor::setNewGroups()
{
  for (size_t i = 0; i < args_.size(); i++) {
    args_[i].setGroup (ProbFormula::getNewGroup());
  }
}



void
Parfactor::applySubstitution (const Substitution& theta)
{
  for (size_t i = 0; i < args_.size(); i++) {
    LogVars& lvs = args_[i].logVars();
    for (size_t j = 0; j < lvs.size(); j++) {
      lvs[j] = theta.newNameFor (lvs[j]);
    }
    if (args_[i].isCounting()) {
      LogVar clv = args_[i].countedLogVar();
      args_[i].setCountedLogVar (theta.newNameFor (clv));
    }
  }
  constr_->applySubstitution (theta);
}



size_t
Parfactor::indexOfGround (const Ground& ground) const
{
  size_t idx = args_.size();
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].functor() == ground.functor() &&
        args_[i].arity()   == ground.arity()) {
      constr_->moveToTop (args_[i].logVars());
      if (constr_->containsTuple (ground.args())) {
        idx = i;
        break;
      }
    }
  }
  return idx;
}



PrvGroup
Parfactor::findGroup (const Ground& ground) const
{
  size_t idx = indexOfGround (ground);
  return idx == args_.size()
         ? std::numeric_limits<PrvGroup>::max()
         : args_[idx].group();
}



bool
Parfactor::containsGround (const Ground& ground) const
{
  return findGroup (ground) != std::numeric_limits<PrvGroup>::max();
}



bool
Parfactor::containsGrounds (const Grounds& grounds) const
{
  Tuple tuple;
  LogVars tupleLvs;
  for (size_t i = 0; i < grounds.size(); i++) {
    size_t idx = indexOfGround (grounds[i]);
    if (idx == args_.size()) {
      return false;
    }
    LogVars lvs = args_[idx].logVars();
    for (size_t j = 0; j < lvs.size(); j++) {
      if (Util::contains (tupleLvs, lvs[j]) == false) {
        tuple.push_back (grounds[i].args()[j]);
        tupleLvs.push_back (lvs[j]);
      }
    }
  }
  constr_->moveToTop (tupleLvs);
  return constr_->containsTuple (tuple);
}



bool
Parfactor::containsGroup (PrvGroup group) const
{
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].group() == group) {
      return true;
    }
  }
  return false;
}



bool
Parfactor::containsGroups (std::vector<PrvGroup> groups) const
{
  for (size_t i = 0; i < groups.size(); i++) {
    if (containsGroup (groups[i]) == false) {
      return false;
    }
  }
  return true;
}



unsigned
Parfactor::nrFormulas (LogVar X) const
{
  unsigned count = 0;
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].contains (X)) {
      count ++;
    }
  }
  return count;
}



int
Parfactor::indexOfLogVar (LogVar X) const
{
  size_t idx = args_.size();
  assert (nrFormulas (X) == 1);
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].contains (X)) {
      idx = i;
      break;
    }
  }
  return idx;
}



int
Parfactor::indexOfGroup (PrvGroup group) const
{
  size_t pos = args_.size();
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].group() == group) {
      pos = i;
      break;
    }
  }
  return pos;
}



unsigned
Parfactor::nrFormulasWithGroup (PrvGroup group) const
{
  unsigned count = 0;
  for (size_t i = 0; i < args_.size(); i++) {
    if (args_[i].group() == group) {
      count ++;
    }
  }
  return count;
}



std::vector<PrvGroup>
Parfactor::getAllGroups() const
{
  std::vector<PrvGroup> groups (args_.size());
  for (size_t i = 0; i < args_.size(); i++) {
    groups[i] = args_[i].group();
  }
  return groups;
}



std::string
Parfactor::getLabel() const
{
  std::stringstream ss;
  ss << "phi(" ;
  for (size_t i = 0; i < args_.size(); i++) {
    if (i != 0) ss << "," ;
    ss << args_[i];
  }
  ss << ")" ;
  ConstraintTree copy (*constr_);
  copy.moveToTop (copy.logVarSet().elements());
  ss << "|" << copy.tupleSet();
  return ss.str();
}



void
Parfactor::print (bool printParams) const
{
  using std::cout;
  using std::endl;
  cout << "Formulas:  " ;
  for (size_t i = 0; i < args_.size(); i++) {
    if (i != 0) cout << ", " ;
    cout << args_[i];
  }
  cout << endl;
  if (args_[0].group() != Util::maxUnsigned()) {
    std::vector<std::string> groups;
    for (size_t i = 0; i < args_.size(); i++) {
      groups.push_back (std::string ("g")
          + Util::toString (args_[i].group()));
    }
    cout << "Groups:    " << groups  << endl;
  }
  cout << "LogVars:   " << constr_->logVarSet()  << endl;
  cout << "Ranges:    " << ranges_ << endl;
  if (printParams == false) {
    cout << "Params:    " ;
    if (params_.size() <= 32) {
      cout.precision(10);
      cout << params_ << endl;
    } else {
      cout << "|" << params_.size() << "|" << endl;
    }
  }
  ConstraintTree copy (*constr_);
  copy.moveToTop (copy.logVarSet().elements());
  cout << "Tuples:    " << copy.tupleSet() << endl;
  if (printParams) {
    printParameters();
  }
}



void
Parfactor::printParameters() const
{
  std::vector<std::string> jointStrings;
  Indexer indexer (ranges_);
  while (indexer.valid()) {
    std::stringstream ss;
    for (size_t i = 0; i < args_.size(); i++) {
      if (i != 0) ss << ", " ;
      if (args_[i].isCounting()) {
        unsigned N = constr_->getConditionalCount (
            args_[i].countedLogVar());
        HistogramSet hs (N, args_[i].range());
        unsigned c = 0;
        while (c < indexer[i]) {
          hs.nextHistogram();
          c ++;
        }
        ss << hs;
      } else {
        ss << indexer[i];
      }
    }
    jointStrings.push_back (ss.str());
    ++ indexer;
  }
  for (size_t i = 0; i < params_.size(); i++) {
    std::cout << "f(" << jointStrings[i] << ")" ;
    std::cout << " = " << params_[i] << std::endl;
  }
}



void
Parfactor::printProjections() const
{
  ConstraintTree copy (*constr_);

  LogVarSet Xs = copy.logVarSet();
  for (size_t i = 0; i < Xs.size(); i++) {
    std::cout << "-> projection of " << Xs[i] << ": " ;
    std::cout << copy.tupleSet ({Xs[i]}) << std::endl;
  }
}



void
Parfactor::expandPotential (
    size_t fIdx,
    unsigned newRange,
    const std::vector<unsigned>& sumIndexes)
{
  ullong newSize = (params_.size() / ranges_[fIdx]) * newRange;
  if (newSize > params_.max_size()) {
    std::cerr << "Error: an overflow occurred when performing expansion." ;
    std::cerr << std::endl;
    exit (EXIT_FAILURE);
  }

  Params backup = params_;
  params_.clear();
  params_.reserve (newSize);

  size_t prod = 1;
  std::vector<size_t> offsets (ranges_.size());
  for (size_t i = ranges_.size(); i-- > 0; ) {
    offsets[i] = prod;
    prod *= ranges_[i];
  }

  size_t index = 0;
  ranges_[fIdx] = newRange;
  std::vector<unsigned> indices (ranges_.size(), 0);
  for (size_t k = 0; k < newSize; k++) {
    assert (index < backup.size());
    params_.push_back (backup[index]);
    for (size_t i = ranges_.size(); i-- > 0; ) {
      indices[i] ++;
      if (i == fIdx) {
        if (indices[i] != ranges_[i]) {
          int diff = sumIndexes[indices[i]] - sumIndexes[indices[i] - 1];
          index += diff * offsets[i];
          break;
        } else {
          // last index contains the old range minus 1
          index -= sumIndexes.back() * offsets[i];
          indices[i] = 0;
        }
      } else {
        if (indices[i] != ranges_[i]) {
          index += offsets[i];
          break;
        } else {
          index -= (ranges_[i] - 1) * offsets[i];
          indices[i] = 0;
        }
      }
    }
  }
}



void
Parfactor::simplifyCountingFormulas (size_t fIdx)
{
  // check if we can simplify the parfactor
  for (size_t i = 0; i < args_.size(); i++) {
    if (i != fIdx &&
        args_[i].isCounting() &&
        args_[i].group() == args_[fIdx].group()) {
      // if they only differ in the name of the counting log var
      if ((args_[i].logVarSet() - args_[i].countedLogVar()) ==
          (args_[fIdx].logVarSet()) - args_[fIdx].countedLogVar() &&
           ranges_[i] == ranges_[fIdx]) {
        simplifyParfactor (fIdx, i);
        break;
      }
    }
  }
}



void
Parfactor::simplifyGrounds()
{
  if (args_.size() == 1) {
    return;
  }
  LogVarSet singletons = constr_->singletons();
  for (long i = 0; i < (long)args_.size() - 1; i++) {
    for (size_t j = i + 1; j < args_.size(); j++) {
      if (args_[i].group() == args_[j].group() &&
          singletons.contains (args_[i].logVarSet()) &&
          singletons.contains (args_[j].logVarSet())) {
        simplifyParfactor (i, j);
        i --;
        break;
      }
    }
  }
}



bool
Parfactor::canMultiply (Parfactor* g1, Parfactor* g2)
{
  std::pair<LogVars, LogVars> res = getAlignLogVars (g1, g2);
  LogVarSet Xs_1 (res.first);
  LogVarSet Xs_2 (res.second);
  LogVarSet Y_1 = g1->logVarSet() - Xs_1;
  LogVarSet Y_2 = g2->logVarSet() - Xs_2;
  Y_1 -= g1->countedLogVars();
  Y_2 -= g2->countedLogVars();
  return g1->constr()->isCountNormalized (Y_1) &&
         g2->constr()->isCountNormalized (Y_2);
}



void
Parfactor::simplifyParfactor (size_t fIdx1, size_t fIdx2)
{
  Params backup = params_;
  params_.clear();
  Indexer indexer (ranges_);
  while (indexer.valid()) {
    if (indexer[fIdx1] == indexer[fIdx2]) {
      params_.push_back (backup[indexer]);
    }
    ++ indexer;
  }
  for (size_t i = 0; i < args_[fIdx2].logVars().size(); i++) {
    if (nrFormulas (args_[fIdx2].logVars()[i]) == 1) {
      constr_->remove ({ args_[fIdx2].logVars()[i] });
    }
  }
  args_.erase (args_.begin() + fIdx2);
  ranges_.erase (ranges_.begin() + fIdx2);
}



std::pair<LogVars, LogVars>
Parfactor::getAlignLogVars (Parfactor* g1, Parfactor* g2)
{
  g1->simplifyGrounds();
  g2->simplifyGrounds();
  LogVars Xs_1, Xs_2;
  TinySet<size_t> matchedI;
  TinySet<size_t> matchedJ;
  ProbFormulas& formulas1 = g1->arguments();
  ProbFormulas& formulas2 = g2->arguments();
  for (size_t i = 0; i < formulas1.size(); i++) {
    for (size_t j = 0; j < formulas2.size(); j++) {
      if (formulas1[i].group() == formulas2[j].group() &&
          g1->range (i) == g2->range (j) &&
          matchedI.contains (i) == false &&
          matchedJ.contains (j) == false) {
        Util::addToVector (Xs_1, formulas1[i].logVars());
        Util::addToVector (Xs_2, formulas2[j].logVars());
        matchedI.insert (i);
        matchedJ.insert (j);
      }
    }
  }
  return make_pair (Xs_1, Xs_2);
}



void
Parfactor::alignAndExponentiate (Parfactor* g1, Parfactor* g2)
{
  alignLogicalVars (g1, g2);
  LogVarSet comm = g1->logVarSet() & g2->logVarSet();
  LogVarSet Y_1 = g1->logVarSet() - comm;
  LogVarSet Y_2 = g2->logVarSet() - comm;
  Y_1 -= g1->countedLogVars();
  Y_2 -= g2->countedLogVars();
  assert (g1->constr()->isCountNormalized (Y_1));
  assert (g2->constr()->isCountNormalized (Y_2));
  unsigned condCount1 = g1->constr()->getConditionalCount (Y_1);
  unsigned condCount2 = g2->constr()->getConditionalCount (Y_2);
  LogAware::pow (g1->params(), 1.0 / condCount2);
  LogAware::pow (g2->params(), 1.0 / condCount1);
}



void
Parfactor::alignLogicalVars (Parfactor* g1, Parfactor* g2)
{
  std::pair<LogVars, LogVars> res = getAlignLogVars (g1, g2);
  const LogVars& alignLvs1 = res.first;
  const LogVars& alignLvs2 = res.second;
  // std::cout << "ALIGNING :::::::::::::::::" << std::endl;
  // g1->print();
  // cout << "AND" << endl;
  // g2->print();
  // std::cout << "-> align lvs1 = " << alignLvs1 << std::endl;
  // std::cout << "-> align lvs2 = " << alignLvs2 << std::endl;
  LogVar freeLogVar (0);
  Substitution theta1, theta2;
  for (size_t i = 0; i < alignLvs1.size(); i++) {
    bool b1 = theta1.containsReplacementFor (alignLvs1[i]);
    bool b2 = theta2.containsReplacementFor (alignLvs2[i]);
    if (b1 == false && b2 == false) {
      theta1.add (alignLvs1[i], freeLogVar);
      theta2.add (alignLvs2[i], freeLogVar);
      ++ freeLogVar;
    } else if (b1 == false && b2) {
      theta1.add (alignLvs1[i], theta2.newNameFor (alignLvs2[i]));
    } else if (b1 && b2 == false) {
      theta2.add (alignLvs2[i], theta1.newNameFor (alignLvs1[i]));
    }
  }

  const LogVarSet& allLvs1 = g1->logVarSet();
  for (size_t i = 0; i < allLvs1.size(); i++) {
    if (theta1.containsReplacementFor (allLvs1[i]) == false) {
      theta1.add (allLvs1[i], freeLogVar);
      ++ freeLogVar;
    }
  }
  const LogVarSet& allLvs2 = g2->logVarSet();
  for (size_t i = 0; i < allLvs2.size(); i++) {
    if (theta2.containsReplacementFor (allLvs2[i]) == false) {
      theta2.add (allLvs2[i], freeLogVar);
      ++ freeLogVar;
    }
  }

  // handle this type of situation:
  // g1 = p(X), q(X) ;  X    in {(p1),(p2)}
  // g2 = p(X), q(Y) ; (X,Y) in {(p1,p2),(p2,p1)}
  LogVars discardedLvs1 = theta1.getDiscardedLogVars();
  for (size_t i = 0; i < discardedLvs1.size(); i++) {
    if (g1->constr()->isSingleton (discardedLvs1[i]) &&
        g1->nrFormulas (discardedLvs1[i]) == 1) {
      g1->constr()->remove (discardedLvs1[i]);
    } else {
      LogVar X_new = ++ g1->constr()->logVarSet().back();
      theta1.rename (discardedLvs1[i], X_new);
    }
  }
  LogVars discardedLvs2 = theta2.getDiscardedLogVars();
  for (size_t i = 0; i < discardedLvs2.size(); i++) {
    if (g2->constr()->isSingleton (discardedLvs2[i]) &&
        g2->nrFormulas (discardedLvs2[i]) == 1) {
      g2->constr()->remove (discardedLvs2[i]);
    } else {
      LogVar X_new = ++ g2->constr()->logVarSet().back();
      theta2.rename (discardedLvs2[i], X_new);
    }
  }

  // std::cout << "theta1: " << theta1 << std::endl;
  // std::cout << "theta2: " << theta2 << std::endl;
  g1->applySubstitution (theta1);
  g2->applySubstitution (theta2);
}

}  // namespace Horus

