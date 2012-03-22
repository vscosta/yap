
#include "Parfactor.h"
#include "Histogram.h"
#include "Indexer.h"
#include "Horus.h"


Parfactor::Parfactor (
    const ProbFormulas& formulas,
    const Params& params, 
    const Tuples& tuples,
    unsigned distId)
{
  formulas_ = formulas;
  params_   = params;
  distId_   = distId;

  LogVars logVars;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    ranges_.push_back (formulas_[i].range());
    const LogVars& lvs = formulas_[i].logVars();
    for (unsigned j = 0; j < lvs.size(); j++) {
      if (std::find (logVars.begin(), logVars.end(), lvs[j]) == 
          logVars.end()) {
        logVars.push_back (lvs[j]);
      }
    }
  }
  constr_ = new ConstraintTree (logVars, tuples);
}



Parfactor::Parfactor (const Parfactor* g, const Tuple& tuple)
{
  formulas_ = g->formulas();
  params_   = g->params();
  ranges_   = g->ranges();
  distId_   = g->distId();
  constr_   = new ConstraintTree (g->logVars(), {tuple});
}



Parfactor::Parfactor (const Parfactor* g, ConstraintTree* constr)
{
  formulas_ = g->formulas();
  params_   = g->params();
  ranges_   = g->ranges();
  distId_   = g->distId();
  constr_   = constr;
}



Parfactor::Parfactor (const Parfactor& g)
{
  formulas_ = g.formulas();
  params_   = g.params();
  ranges_   = g.ranges();
  distId_   = g.distId();
  constr_   = new ConstraintTree (*g.constr());
}



Parfactor::~Parfactor (void)
{
  delete constr_;
}



LogVarSet
Parfactor::countedLogVars (void) const
{
  LogVarSet set;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].isCounting()) {
      set.insert (formulas_[i].countedLogVar());
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
  assert (fIdx < formulas_.size());
  LogVarSet remaining;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (i != fIdx) {
      remaining |= formulas_[i].logVarSet();
    }
  }
  return formulas_[fIdx].logVarSet() - remaining;
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
  assert (fIdx < formulas_.size());
  assert (formulas_[fIdx].contains (elimLogVars()));

  LogVarSet excl = exclusiveLogVars (fIdx);
  unsigned condCount = constr_->getConditionalCount (excl);
  Util::pow (params_, condCount);

  vector<unsigned> numAssigns (ranges_[fIdx], 1);
  if (formulas_[fIdx].isCounting()) {
    unsigned N = constr_->getConditionalCount (
        formulas_[fIdx].countedLogVar());
    unsigned R = formulas_[fIdx].range();
    unsigned H = ranges_[fIdx];
    HistogramSet hs (N, R);
    unsigned N_factorial = Util::factorial (N);
    for (unsigned h = 0; h < H; h++) {
      unsigned prod = 1;
      for (unsigned r = 0; r < R; r++) {
        prod *= Util::factorial (hs[r]);
      }
      numAssigns[h] = N_factorial / prod;
      hs.nextHistogram();
    }
    cout << endl;
  }

  Params copy = params_;
  params_.clear();
  params_.resize (copy.size() / ranges_[fIdx], 0.0);

  MapIndexer indexer (ranges_, fIdx);
  for (unsigned i = 0; i < copy.size(); i++) {
    unsigned h = indexer[fIdx];
    // TODO NOT LOG DOMAIN AWARE :(
    params_[indexer] += numAssigns[h] * copy[i];
    ++ indexer;
  }
  formulas_.erase (formulas_.begin() + fIdx);
  ranges_.erase (ranges_.begin() + fIdx);
  constr_->remove (excl);
}



void
Parfactor::multiply (Parfactor& g)
{
  alignAndExponentiate (this, &g);
  bool sharedVars = false;
  vector<unsigned> g_varpos;
  const ProbFormulas& g_formulas = g.formulas();
  const Params&  g_params  = g.params();
  const Ranges&  g_ranges  = g.ranges();

  for (unsigned i = 0; i < g_formulas.size(); i++) {
    int group = g_formulas[i].group();
    if (indexOfFormulaWithGroup (group) == -1) {
      insertDimension (g.ranges()[i]);
      formulas_.push_back (g_formulas[i]);
      g_varpos.push_back (formulas_.size() - 1);
    } else {
      sharedVars = true;
      g_varpos.push_back (indexOfFormulaWithGroup (group));
    }
  }
 
  if (sharedVars == false) {
    unsigned count = 0;
    for (unsigned i = 0; i < params_.size(); i++) {
      if (Globals::logDomain) {
        params_[i] += g_params[count];
      } else {
        params_[i] *= g_params[count];
      }
      count ++;
      if (count >= g_params.size()) {
        count = 0;
      }
    }
  } else {
    StatesIndexer indexer (ranges_, false);
    while (indexer.valid()) {
      unsigned g_li = 0;
      unsigned prod = 1;
      for (int j = g_varpos.size() - 1; j >= 0; j--) {
        g_li += indexer[g_varpos[j]] * prod;
        prod *= g_ranges[j];
      }
      if (Globals::logDomain) {
        params_[indexer] += g_params[g_li];
      } else {
        params_[indexer] *= g_params[g_li];
      }
      ++ indexer;
    }
  }

  constr_->join (g.constr(), true);
}



void
Parfactor::countConvert (LogVar X)
{
  int fIdx = indexOfFormulaWithLogVar (X);
  assert (fIdx != -1);
  assert (constr_->isCountNormalized (X));
  assert (constr_->getConditionalCount (X) > 1);
  assert (constr_->isCarteesianProduct (countedLogVars() | X));

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = ranges_[fIdx];
  unsigned H = HistogramSet::nrHistograms (N, R);
  vector<Histogram> histograms = HistogramSet::getHistograms (N, R);

  StatesIndexer indexer (ranges_);
  vector<Params> summout (params_.size() / R);
  unsigned count = 0;
  while (indexer.valid()) {
    summout[count].reserve (R);
    for (unsigned r = 0; r < R; r++) {
      summout[count].push_back (params_[indexer]);
      indexer.increment (fIdx);
    }
    count ++;
    indexer.reset (fIdx);
    indexer.incrementExcluding (fIdx);
  }

  params_.clear();
  params_.reserve (summout.size() * H);

  vector<bool> mapDims (ranges_.size(), true);
  ranges_[fIdx] = H;
  mapDims[fIdx] = false;
  MapIndexer mapIndexer (ranges_, mapDims);
  while (mapIndexer.valid()) {
    double prod = 1.0;
    unsigned i = mapIndexer.mappedIndex();
    unsigned h = mapIndexer[fIdx];
    for (unsigned r = 0; r < R; r++) {
      // TODO not log domain aware
      prod *= Util::pow (summout[i][r], histograms[h][r]);
    }
    params_.push_back (prod);
    ++ mapIndexer;
  }
  formulas_[fIdx].setCountedLogVar (X);
}



void
Parfactor::expandPotential (
    LogVar X,
    LogVar X_new1,
    LogVar X_new2)
{
  int fIdx = indexOfFormulaWithLogVar (X);
  assert (fIdx != -1);
  assert (formulas_[fIdx].isCounting());

  unsigned N1 = constr_->getConditionalCount (X_new1);
  unsigned N2 = constr_->getConditionalCount (X_new2);
  unsigned N  = N1 + N2;
  unsigned R  = formulas_[fIdx].range();
  unsigned H1 = HistogramSet::nrHistograms (N1, R);
  unsigned H2 = HistogramSet::nrHistograms (N2, R);
  unsigned H  = ranges_[fIdx];

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

  unsigned size = (params_.size() / H) * H1 * H2;
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
  ranges_[fIdx] = H1 * H2;
  vector<unsigned> indices (ranges_.size(), 0);
  for (unsigned k = 0; k < size; k++) {
    params_.push_back (copy[index]);
    for (int i = ranges_.size() - 1; i >= 0; i--) {
      indices[i] ++;
      if (i == fIdx) {
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

  formulas_.insert (formulas_.begin() + fIdx + 1, formulas_[fIdx]);
  formulas_[fIdx].rename (X, X_new1);
  formulas_[fIdx + 1].rename (X, X_new2);
  ranges_.insert (ranges_.begin() + fIdx + 1, H2);
  ranges_[fIdx] = H1;
}



void
Parfactor::fullExpand (LogVar X)
{
  int fIdx = indexOfFormulaWithLogVar (X);
  assert (fIdx != -1);
  assert (formulas_[fIdx].isCounting());

  unsigned N = constr_->getConditionalCount (X);
  unsigned R = formulas_[fIdx].range();
  unsigned H = ranges_[fIdx];

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
  
  unsigned size = (params_.size() / H) * std::pow (R, N);
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
  ranges_[fIdx] = std::pow (R, N);
  vector<unsigned> indices (ranges_.size(), 0);
  for (unsigned k = 0; k < size; k++) {
    params_.push_back (copy[index]);
    for (int i = ranges_.size() - 1; i >= 0; i--) {
      indices[i] ++;
      if (i == fIdx) {
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

  ProbFormula f = formulas_[fIdx];
  formulas_.erase (formulas_.begin() + fIdx);
  ranges_.erase (ranges_.begin() + fIdx);
  LogVars newLvs = constr_->expand (X);
  assert (newLvs.size() == N);
  for (unsigned i = 0 ; i < N; i++) {
    ProbFormula newFormula (f.functor(), f.logVars(), f.range());
    newFormula.rename (X, newLvs[i]);
    formulas_.insert (formulas_.begin() + fIdx + i, newFormula);
    ranges_.insert (ranges_.begin() + fIdx + i, R);
  }
}



void
Parfactor::reorderAccordingGrounds (const Grounds& grounds)
{
  ProbFormulas newFormulas;
  for (unsigned i = 0; i < grounds.size(); i++) {
    for (unsigned j = 0; j < formulas_.size(); j++) {
      if (grounds[i].functor() == formulas_[j].functor() && 
          grounds[i].arity()   == formulas_[j].arity()) {
        constr_->moveToTop (formulas_[j].logVars());
        if (constr_->containsTuple (grounds[i].args())) {
          newFormulas.push_back (formulas_[j]);
          break;
        }
      }
    }
    assert (newFormulas.size() == i + 1);
  }
  reorderFormulas (newFormulas);
}



void
Parfactor::reorderFormulas (const ProbFormulas& newFormulas)
{
  assert (newFormulas.size() == formulas_.size());
  if (newFormulas == formulas_) {
    return;
  }

  Ranges newRanges;
  vector<unsigned> positions;
  for (unsigned i = 0; i < newFormulas.size(); i++) {
    unsigned idx = indexOf (newFormulas[i]);
    newRanges.push_back (ranges_[idx]);
    positions.push_back (idx);
  }
      
  unsigned N = ranges_.size();
  Params newParams (params_.size());
  for (unsigned i = 0; i < params_.size(); i++) {
    unsigned li = i;
    // calculate vector index corresponding to linear index
    vector<unsigned> vi (N);
    for (int k = N-1; k >= 0; k--) {
      vi[k] = li % ranges_[k];
      li   /= ranges_[k];
    }
    // convert permuted vector index to corresponding linear index
    unsigned prod = 1;
    unsigned new_li = 0;
    for (int k = N - 1; k >= 0; k--) {
      new_li += vi[positions[k]] * prod;
      prod   *= ranges_[positions[k]];
    }
    newParams[new_li] = params_[i];
  }
  formulas_ = newFormulas;
  ranges_   = newRanges;
  params_   = newParams;
}



void
Parfactor::absorveEvidence (unsigned fIdx, unsigned evidence)
{
  LogVarSet excl = exclusiveLogVars (fIdx);
  assert (fIdx < formulas_.size());
  assert (evidence < formulas_[fIdx].range());
  assert (formulas_[fIdx].isCounting() == false);
  assert (constr_->isCountNormalized (excl));

  Util::pow (params_, constr_->getConditionalCount (excl));
  
  Params copy = params_;
  params_.clear();
  params_.reserve (copy.size() / formulas_[fIdx].range());

  StatesIndexer indexer (ranges_);
  for (unsigned i = 0; i < evidence; i++) {
    indexer.increment (fIdx);
  }
  while (indexer.valid()) {
    params_.push_back (copy[indexer]);
    indexer.incrementExcluding (fIdx);
  }
  formulas_.erase (formulas_.begin() + fIdx);
  ranges_.erase (ranges_.begin() + fIdx);
  constr_->remove (excl);
}



void
Parfactor::normalize (void)
{
  Util::normalize (params_);
}



void
Parfactor::setFormulaGroup (const ProbFormula& f, int group)
{
  assert (indexOf (f) != -1);
  formulas_[indexOf (f)].setGroup (group);
}



void
Parfactor::setNewGroups (void)
{
  for (unsigned i = 0; i < formulas_.size(); i++) {
    formulas_[i].setGroup (ProbFormula::getNewGroup());
  }
}



void
Parfactor::applySubstitution (const Substitution& theta)
{
  for (unsigned i = 0; i < formulas_.size(); i++) {
    LogVars& lvs = formulas_[i].logVars();
    for (unsigned j = 0; j < lvs.size(); j++) {
      lvs[j] = theta.newNameFor (lvs[j]);
    }
    if (formulas_[i].isCounting()) {
      LogVar clv = formulas_[i].countedLogVar();
      formulas_[i].setCountedLogVar (theta.newNameFor (clv));
    }
  }
  constr_->applySubstitution (theta);
}



bool
Parfactor::containsGround (const Ground& ground) const
{
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].functor() == ground.functor() && 
        formulas_[i].arity()   == ground.arity()) {
      constr_->moveToTop (formulas_[i].logVars());
      if (constr_->containsTuple (ground.args())) {
        return true;
      }
    }
  }
  return false;
}



bool
Parfactor::containsGroup (unsigned group) const
{
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].group() == group) {
      return true;
    }
  }
  return false;
}



const ProbFormula&
Parfactor::formula (unsigned fIdx) const
{
  assert (fIdx < formulas_.size());
  return formulas_[fIdx];
}



unsigned
Parfactor::range (unsigned fIdx) const
{
  assert (fIdx < ranges_.size());
  return ranges_[fIdx];
}



unsigned
Parfactor::nrFormulas (LogVar X) const
{
  unsigned count = 0;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].contains (X)) {
      count ++;
    }
  }
  return count;
}



int
Parfactor::indexOf (const ProbFormula& f) const
{
  int idx = -1;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (f == formulas_[i]) {
      idx = i;
      break;
    }
  }
  return idx;
}



int
Parfactor::indexOfFormulaWithLogVar (LogVar X) const
{
  int idx = -1;
  assert (nrFormulas (X) == 1);
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].contains (X)) {
      idx = i;
      break;
    }
  }
  return idx;
}



int
Parfactor::indexOfFormulaWithGroup (unsigned group) const
{
  int pos = -1;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (formulas_[i].group() == group) {
      pos = i;
      break;
    }
  }
  return pos;
}



vector<unsigned>
Parfactor::getAllGroups (void) const
{
  vector<unsigned> groups (formulas_.size());
  for (unsigned i = 0; i < formulas_.size(); i++) {
    groups[i] = formulas_[i].group();
  }
  return groups;
}



string
Parfactor::getHeaderString (void) const
{
  stringstream ss;
  ss << "phi(" ;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (i != 0) ss << "," ;
    ss << formulas_[i];
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
  for (unsigned i = 0; i < formulas_.size(); i++) {
    if (i != 0) cout << ", " ;
    cout << formulas_[i];
  }
  cout << endl;
  vector<string> groups;
  for (unsigned i = 0; i < formulas_.size(); i++) {
    groups.push_back (string ("g") + Util::toString (formulas_[i].group()));
  }
  cout << "Groups:    " << groups  << endl;
  cout << "LogVars:   " << constr_->logVars()  << endl;
  cout << "Ranges:    " << ranges_ << endl;
  if (printParams == false) {
    cout << "Params:    " << params_ << endl;
  }
  cout << "Tuples:    " << constr_->tupleSet() << endl;
  if (printParams) {
    vector<string> jointStrings;
    StatesIndexer indexer (ranges_);
    while (indexer.valid()) {
      stringstream ss;
      for (unsigned i = 0; i < formulas_.size(); i++) {
        if (i != 0) ss << ", " ;
        if (formulas_[i].isCounting()) {
          unsigned N = constr_->getConditionalCount (formulas_[i].countedLogVar());
          HistogramSet hs (N, formulas_[i].range());
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
}



void
Parfactor::insertDimension (unsigned range)
{
  Params copy = params_;
  params_.clear();
  params_.reserve (copy.size() * range);
  for (unsigned i = 0; i < copy.size(); i++) {
    for (unsigned reps = 0; reps < range; reps++) {
      params_.push_back (copy[i]);
    }
  }
  ranges_.push_back (range);
}



void
Parfactor::alignAndExponentiate (Parfactor* g1, Parfactor* g2)
{
  LogVars X_1, X_2;
  const ProbFormulas& formulas1 = g1->formulas();
  const ProbFormulas& formulas2 = g2->formulas();
  for (unsigned i = 0; i < formulas1.size(); i++) {
    for (unsigned j = 0; j < formulas2.size(); j++) {
      if (formulas1[i].group() == formulas2[j].group()) {
        X_1.insert (X_1.end(),
                    formulas1[i].logVars().begin(),
                    formulas1[i].logVars().end());
        X_2.insert (X_2.end(),
                    formulas2[j].logVars().begin(),
                    formulas2[j].logVars().end());
      }
    }
  }
  align (g1, X_1, g2, X_2);
  LogVarSet Y_1 = g1->logVarSet() - LogVarSet (X_1);
  LogVarSet Y_2 = g2->logVarSet() - LogVarSet (X_2);
  assert (g1->constr()->isCountNormalized (Y_1));
  assert (g2->constr()->isCountNormalized (Y_2));
  unsigned condCount1 = g1->constr()->getConditionalCount (Y_1);
  unsigned condCount2 = g2->constr()->getConditionalCount (Y_2);
  Util::pow (g1->params(), 1.0 / condCount2);
  Util::pow (g2->params(), 1.0 / condCount1);
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

