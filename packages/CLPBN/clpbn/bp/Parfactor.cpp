
#include "Parfactor.h"
#include "Histogram.h"
#include "Indexer.h"
#include "Util.h"
#include "Horus.h"


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
  for (unsigned i = 0; i < args_.size(); i++) {
    ranges_.push_back (args_[i].range());
    const LogVars& lvs = args_[i].logVars();
    for (unsigned j = 0; j < lvs.size(); j++) {
      if (Util::contains (logVars, lvs[j]) == false) {
        logVars.push_back (lvs[j]);
      }
    }
  }
  constr_ = new ConstraintTree (logVars, tuples);
  assert (params_.size() == Util::expectedSize (ranges_));
}



Parfactor::Parfactor (const Parfactor* g, const Tuple& tuple)
{
  args_    = g->arguments();
  params_  = g->params();
  ranges_  = g->ranges();
  distId_  = g->distId();
  constr_  = new ConstraintTree (g->logVars(), {tuple});
  assert (params_.size() == Util::expectedSize (ranges_));
}



Parfactor::Parfactor (const Parfactor* g, ConstraintTree* constr)
{
  args_    = g->arguments();
  params_  = g->params();
  ranges_  = g->ranges();
  distId_  = g->distId();
  constr_  = constr;
  assert (params_.size() == Util::expectedSize (ranges_));
}



Parfactor::Parfactor (const Parfactor& g)
{
  args_    = g.arguments();
  params_  = g.params();
  ranges_  = g.ranges();
  distId_  = g.distId();
  constr_  = new ConstraintTree (*g.constr());
  assert (params_.size() == Util::expectedSize (ranges_));
}



Parfactor::~Parfactor (void)
{
  delete constr_;
}



LogVarSet
Parfactor::countedLogVars (void) const
{
  LogVarSet set;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].isCounting()) {
      set.insert (args_[i].countedLogVar());
    }
  }
  return set;
}



LogVarSet
Parfactor::uncountedLogVars (void) const
{
  return constr_->logVarSet() - countedLogVars();
}



LogVarSet
Parfactor::elimLogVars (void) const
{
  LogVarSet requiredToElim = constr_->logVarSet();
  requiredToElim -= constr_->singletons();
  requiredToElim -= countedLogVars();
  return requiredToElim;
}



LogVarSet
Parfactor::exclusiveLogVars (unsigned fIdx) const
{
  assert (fIdx < args_.size());
  LogVarSet remaining;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (i != fIdx) {
      remaining |= args_[i].logVarSet();
    }
  }
  return args_[fIdx].logVarSet() - remaining;
}



void
Parfactor::setConstraintTree (ConstraintTree* newTree)
{
  delete constr_;
  constr_ = newTree;
}



void
Parfactor::sumOut (unsigned fIdx)
{
  assert (fIdx < args_.size());
  assert (args_[fIdx].contains (elimLogVars()));

  LogVarSet excl = exclusiveLogVars (fIdx);
  if (args_[fIdx].isCounting()) {
    LogAware::pow (params_, constr_->getConditionalCount (
        excl - args_[fIdx].countedLogVar()));
  } else {
    LogAware::pow (params_, constr_->getConditionalCount (excl));
  }

  if (args_[fIdx].isCounting()) {
    unsigned N = constr_->getConditionalCount (
        args_[fIdx].countedLogVar());
    unsigned R = args_[fIdx].range();
    vector<double> numAssigns = HistogramSet::getNumAssigns (N, R);
    StatesIndexer sindexer (ranges_, fIdx);
    while (sindexer.valid()) {
      unsigned h = sindexer[fIdx];
      if (Globals::logDomain) {
        params_[sindexer] += numAssigns[h];
      } else {
        params_[sindexer] *= numAssigns[h];
      }
      ++ sindexer;
    }
  }

  Params copy = params_;
  params_.clear();
  params_.resize (copy.size() / ranges_[fIdx], LogAware::addIdenty());
  MapIndexer indexer (ranges_, fIdx);
  if (Globals::logDomain) {
    for (unsigned i = 0; i < copy.size(); i++) {
      params_[indexer] = Util::logSum (params_[indexer], copy[i]);
      ++ indexer;
    }
  } else {
    for (unsigned i = 0; i < copy.size(); i++) {
      params_[indexer] += copy[i];
      ++ indexer;
    }
  }

  args_.erase (args_.begin() + fIdx);
  ranges_.erase (ranges_.begin() + fIdx);
  constr_->remove (excl);
}



void
Parfactor::multiply (Parfactor& g)
{
  alignAndExponentiate (this, &g);
  TFactor<ProbFormula>::multiply (g);
  constr_->join (g.constr(), true);
}



void
Parfactor::countConvert (LogVar X)
{
  int fIdx = indexOfLogVar (X);
  assert (fIdx != -1);
  assert (constr_->isCountNormalized (X));
  assert (constr_->getConditionalCount (X) > 1);
  assert (constr_->isCarteesianProduct (countedLogVars() | X));

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = ranges_[fIdx];
  unsigned H = HistogramSet::nrHistograms (N, R);
  vector<Histogram> histograms = HistogramSet::getHistograms (N, R);

  StatesIndexer indexer (ranges_);
  vector<Params> sumout (params_.size() / R);
  unsigned count = 0;
  while (indexer.valid()) {
    sumout[count].reserve (R);
    for (unsigned r = 0; r < R; r++) {
      sumout[count].push_back (params_[indexer]);
      indexer.increment (fIdx);
    }
    count ++;
    indexer.reset (fIdx);
    indexer.incrementExcluding (fIdx);
  }

  params_.clear();
  params_.reserve (sumout.size() * H);

  ranges_[fIdx] = H;
  MapIndexer mapIndexer (ranges_, fIdx);
  while (mapIndexer.valid()) {
    double prod = LogAware::multIdenty();
    unsigned i = mapIndexer.mappedIndex();
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
}



void
Parfactor::expand (LogVar X, LogVar X_new1, LogVar X_new2)
{
  int fIdx = indexOfLogVar (X);
  assert (fIdx != -1);
  assert (args_[fIdx].isCounting());

  unsigned N1 = constr_->getConditionalCount (X_new1);
  unsigned N2 = constr_->getConditionalCount (X_new2);
  unsigned N  = N1 + N2;
  unsigned R  = args_[fIdx].range();
  unsigned H1 = HistogramSet::nrHistograms (N1, R);
  unsigned H2 = HistogramSet::nrHistograms (N2, R);

  vector<Histogram> histograms  = HistogramSet::getHistograms (N,  R);
  vector<Histogram> histograms1 = HistogramSet::getHistograms (N1, R);
  vector<Histogram> histograms2 = HistogramSet::getHistograms (N2, R);

  vector<unsigned> sumIndexes;
  sumIndexes.reserve (H1 * H2);
  for (unsigned i = 0; i < H1; i++) {
    for (unsigned j = 0; j < H2; j++) {
      Histogram hist = histograms1[i];
      std::transform (
          hist.begin(), hist.end(),
          histograms2[j].begin(),
          hist.begin(),
          plus<int>());
      sumIndexes.push_back (HistogramSet::findIndex (hist, histograms));
    }
  }

  expandPotential (fIdx, H1 * H2, sumIndexes);

  args_.insert (args_.begin() + fIdx + 1, args_[fIdx]);
  args_[fIdx].rename (X, X_new1);
  args_[fIdx + 1].rename (X, X_new2);
  ranges_.insert (ranges_.begin() + fIdx + 1, H2);
  ranges_[fIdx] = H1;
}



void
Parfactor::fullExpand (LogVar X)
{
  int fIdx = indexOfLogVar (X);
  assert (fIdx != -1);
  assert (args_[fIdx].isCounting());

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = args_[fIdx].range();

  vector<Histogram> originHists = HistogramSet::getHistograms (N, R);
  vector<Histogram> expandHists = HistogramSet::getHistograms (1, R);

  vector<unsigned> sumIndexes;
  sumIndexes.reserve (N * R);

  Ranges expandRanges (N, R);
  StatesIndexer indexer (expandRanges);
  while (indexer.valid()) {
    vector<unsigned> hist (R, 0);
    for (unsigned n = 0; n < N; n++) {
      std::transform (
          hist.begin(), hist.end(),
          expandHists[indexer[n]].begin(),
          hist.begin(),
          plus<int>());
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
  for (unsigned i = 0; i < grounds.size(); i++) {
    for (unsigned j = 0; j < args_.size(); j++) {
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
  int fIdx = indexOf (formula);
  assert (fIdx != -1);
  LogVarSet excl = exclusiveLogVars (fIdx);
  assert (args_[fIdx].isCounting() == false);
  assert (constr_->isCountNormalized (excl));
  LogAware::pow (params_, constr_->getConditionalCount (excl));
  TFactor<ProbFormula>::absorveEvidence (formula, evidence);
  constr_->remove (excl);
}



void
Parfactor::setNewGroups (void)
{
  for (unsigned i = 0; i < args_.size(); i++) {
    args_[i].setGroup (ProbFormula::getNewGroup());
  }
}



void
Parfactor::applySubstitution (const Substitution& theta)
{
  for (unsigned i = 0; i < args_.size(); i++) {
    LogVars& lvs = args_[i].logVars();
    for (unsigned j = 0; j < lvs.size(); j++) {
      lvs[j] = theta.newNameFor (lvs[j]);
    }
    if (args_[i].isCounting()) {
      LogVar clv = args_[i].countedLogVar();
      args_[i].setCountedLogVar (theta.newNameFor (clv));
    }
  }
  constr_->applySubstitution (theta);
}



int
Parfactor::findGroup (const Ground& ground) const
{
  int group = -1;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].functor() == ground.functor() && 
        args_[i].arity()   == ground.arity()) {
      constr_->moveToTop (args_[i].logVars());
      if (constr_->containsTuple (ground.args())) {
        group = args_[i].group();
        break;
      }
    }
  }
  return group;
}



bool
Parfactor::containsGround (const Ground& ground) const
{
  return findGroup (ground) != -1;
}



bool
Parfactor::containsGroup (unsigned group) const
{
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].group() == group) {
      return true;
    }
  }
  return false;
}



unsigned
Parfactor::nrFormulas (LogVar X) const
{
  unsigned count = 0;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].contains (X)) {
      count ++;
    }
  }
  return count;
}



int
Parfactor::indexOfLogVar (LogVar X) const
{
  int idx = -1;
  assert (nrFormulas (X) == 1);
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].contains (X)) {
      idx = i;
      break;
    }
  }
  return idx;
}



int
Parfactor::indexOfGroup (unsigned group) const
{
  int pos = -1;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (args_[i].group() == group) {
      pos = i;
      break;
    }
  }
  return pos;
}



vector<unsigned>
Parfactor::getAllGroups (void) const
{
  vector<unsigned> groups (args_.size());
  for (unsigned i = 0; i < args_.size(); i++) {
    groups[i] = args_[i].group();
  }
  return groups;
}



string
Parfactor::getLabel (void) const
{
  stringstream ss;
  ss << "phi(" ;
  for (unsigned i = 0; i < args_.size(); i++) {
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
  cout << "Formulas:  " ;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (i != 0) cout << ", " ;
    cout << args_[i];
  }
  cout << endl;
  if (args_[0].group() != Util::maxUnsigned()) {
    vector<string> groups;
    for (unsigned i = 0; i < args_.size(); i++) {
      groups.push_back (string ("g") + Util::toString (args_[i].group()));
    }
    cout << "Groups:    " << groups  << endl;
  }
  cout << "LogVars:   " << constr_->logVarSet()  << endl;
  cout << "Ranges:    " << ranges_ << endl;
  if (printParams == false) {
    cout << "Params:    " << params_ << endl;
  }
  ConstraintTree copy (*constr_);
  copy.moveToTop (copy.logVarSet().elements());
  cout << "Tuples:    " << copy.tupleSet() << endl;
  if (printParams) {
    vector<string> jointStrings;
    StatesIndexer indexer (ranges_);
    while (indexer.valid()) {
      stringstream ss;
      for (unsigned i = 0; i < args_.size(); i++) {
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
    for (unsigned i = 0; i < params_.size(); i++) {
      cout << "f(" << jointStrings[i] << ")" ;
      cout << " = " << params_[i] << endl;
    }
  }
  cout << endl;
}



void
Parfactor::expandPotential (
    int fIdx,
    unsigned newRange,
    const vector<unsigned>& sumIndexes)
{
  unsigned size = (params_.size() / ranges_[fIdx]) * newRange;
  Params copy = params_;
  params_.clear();
  params_.reserve (size);

  unsigned prod = 1;
  vector<unsigned> offsets_ (ranges_.size());
  for (int i = ranges_.size() - 1; i >= 0; i--) {
    offsets_[i] = prod;
    prod *= ranges_[i];
  }

  unsigned index = 0;
  ranges_[fIdx] = newRange;
  vector<unsigned> indices (ranges_.size(), 0);
  for (unsigned k = 0; k < size; k++) {
    params_.push_back (copy[index]);
    for (int i = ranges_.size() - 1; i >= 0; i--) {
      indices[i] ++;
      if (i == fIdx) {
        assert (indices[i] - 1 < sumIndexes.size());
        int diff = sumIndexes[indices[i]] - sumIndexes[indices[i] - 1];
        index += diff * offsets_[i];
      } else {
        index += offsets_[i];
      }
      if (indices[i] != ranges_[i]) {
        break;
      } else {
        if (i == fIdx) {
          int diff = sumIndexes[0] - sumIndexes[indices[i]];
          index += diff * offsets_[i];
        } else {
          index -= offsets_[i] * ranges_[i];
        }
        indices[i] = 0;
      }
    }
  }
}



void
Parfactor::alignAndExponentiate (Parfactor* g1, Parfactor* g2)
{
  LogVars X_1, X_2;
  const ProbFormulas& formulas1 = g1->arguments();
  const ProbFormulas& formulas2 = g2->arguments();
  for (unsigned i = 0; i < formulas1.size(); i++) {
    for (unsigned j = 0; j < formulas2.size(); j++) {
      if (formulas1[i].group() == formulas2[j].group()) {
        Util::addToVector (X_1, formulas1[i].logVars());
        Util::addToVector (X_2, formulas2[j].logVars());
      }
    }
  }
  LogVarSet Y_1 = g1->logVarSet() - LogVarSet (X_1);
  LogVarSet Y_2 = g2->logVarSet() - LogVarSet (X_2);
  assert (g1->constr()->isCountNormalized (Y_1));
  assert (g2->constr()->isCountNormalized (Y_2));
  unsigned condCount1 = g1->constr()->getConditionalCount (Y_1);
  unsigned condCount2 = g2->constr()->getConditionalCount (Y_2);
  LogAware::pow (g1->params(), 1.0 / condCount2);
  LogAware::pow (g2->params(), 1.0 / condCount1);
  // this must be done in the end or else X_1 and X_2 
  // will refer the old log var names in the code above
  align (g1, X_1, g2, X_2);
}



void
Parfactor::align (
    Parfactor* g1, const LogVars& alignLvs1,
    Parfactor* g2, const LogVars& alignLvs2)
{
  LogVar freeLogVar = 0;
  Substitution theta1;
  Substitution theta2;
  const LogVarSet& allLvs1 = g1->logVarSet();
  for (unsigned i = 0; i < allLvs1.size(); i++) {
    theta1.add (allLvs1[i], freeLogVar);
    ++ freeLogVar;
  }

  const LogVarSet& allLvs2 = g2->logVarSet();
  for (unsigned i = 0; i < allLvs2.size(); i++) {
    theta2.add (allLvs2[i], freeLogVar);
    ++ freeLogVar;
  }

  assert (alignLvs1.size() == alignLvs2.size());
  for (unsigned i = 0; i < alignLvs1.size(); i++) {
    theta1.rename (alignLvs1[i], theta2.newNameFor (alignLvs2[i]));
  }
  g1->applySubstitution (theta1);
  g2->applySubstitution (theta2);
}

