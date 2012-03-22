#include <cstdlib>
#include <cassert>

#include <algorithm>

#include <iostream>
#include <sstream>

#include "Factor.h"
#include "Indexer.h"
#include "Util.h"


Factor::Factor (const Factor& g)
{
  copyFromFactor (g);
}



Factor::Factor (VarId vid, unsigned nStates)
{
  varids_.push_back (vid);
  ranges_.push_back (nStates);
  dist_ = new Distribution (Params (nStates, 1.0));
}



Factor::Factor (const VarNodes& vars)
{
  int nParams = 1;
  for (unsigned i = 0; i < vars.size(); i++) {
    varids_.push_back (vars[i]->varId());
    ranges_.push_back (vars[i]->nrStates());
    nParams *= vars[i]->nrStates();
  }
  // create a uniform distribution
  double val = 1.0 / nParams;
  dist_ = new Distribution (Params (nParams, val));
}



Factor::Factor (VarId vid, unsigned nStates, const Params& params)
{
  varids_.push_back (vid);
  ranges_.push_back (nStates);
  dist_ = new Distribution (params);
}



Factor::Factor (VarNodes& vars, Distribution* dist)
{
  for (unsigned i = 0; i < vars.size(); i++) {
    varids_.push_back (vars[i]->varId());
    ranges_.push_back (vars[i]->nrStates());
  }
  dist_ = dist;
}



Factor::Factor (const VarNodes& vars, const Params& params)
{
  for (unsigned i = 0; i < vars.size(); i++) {
    varids_.push_back (vars[i]->varId());
    ranges_.push_back (vars[i]->nrStates());
  }
  dist_ = new Distribution (params);
}



Factor::Factor (const VarIds& vids,
                const Ranges& ranges,
                const Params& params)
{
  varids_ = vids;
  ranges_ = ranges;
  dist_   = new Distribution (params);
}



Factor::~Factor (void)
{
  if (dist_->shared() == false) {
    delete dist_;
  }
}



void
Factor::setParameters (const Params& params)
{
  assert (dist_->params.size() == params.size());
  dist_->params = params;
}



void
Factor::copyFromFactor (const Factor& g)
{
  varids_ = g.getVarIds();
  ranges_ = g.getRanges();
  dist_   = new Distribution (g.getParameters());
}



void
Factor::multiply (const Factor& g)
{
  if (varids_.size() == 0) {
    copyFromFactor (g);
    return;
  }

  const VarIds&  g_varids = g.getVarIds();
  const Ranges&  g_ranges = g.getRanges();
  const Params&  g_params = g.getParameters();

  if (varids_ == g_varids) {
    // optimization: if the factors contain the same set of variables,
    // we can do a 1 to 1 operation on the parameters
    if (Globals::logDomain) {
      Util::add (dist_->params, g_params);
    } else {
      Util::multiply (dist_->params, g_params);
    }
  } else {
    bool sharedVars = false;
    vector<unsigned> gvarpos;
    for (unsigned i = 0; i < g_varids.size(); i++) {
      int idx = indexOf (g_varids[i]);
      if (idx == -1) {
        insertVariable (g_varids[i], g_ranges[i]);
        gvarpos.push_back (varids_.size() - 1);
      } else {
        sharedVars = true;
        gvarpos.push_back (idx);
      }
    }
    if (sharedVars == false) {
      // optimization: if the original factors doesn't have common variables,
      // we don't need to marry the states of the common variables
      unsigned count = 0;
      for (unsigned i = 0; i < dist_->params.size(); i++) {
        if (Globals::logDomain) {
          dist_->params[i] += g_params[count];
        } else {
          dist_->params[i] *= g_params[count];
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
        for (int j = gvarpos.size() - 1; j >= 0; j--) {
          g_li += indexer[gvarpos[j]] * prod;
          prod *= g_ranges[j];
        }
        if (Globals::logDomain) {
          dist_->params[indexer] += g_params[g_li];
        } else {
          dist_->params[indexer] *= g_params[g_li];
        }
        ++ indexer;
      }
    }
  }
}



void
Factor::insertVariable (VarId varId, unsigned nrStates)
{
  assert (indexOf (varId) == -1);
  Params oldParams = dist_->params;
  dist_->params.clear();
  dist_->params.reserve (oldParams.size() * nrStates);
  for (unsigned i = 0; i < oldParams.size(); i++) {
    for (unsigned reps = 0; reps < nrStates; reps++) {
      dist_->params.push_back (oldParams[i]);
    }
  }
  varids_.push_back (varId);
  ranges_.push_back (nrStates);
}



void
Factor::insertVariables (const VarIds& varIds, const Ranges& ranges)
{
  Params oldParams = dist_->params;
  unsigned nrStates = 1;
  for (unsigned i = 0; i < varIds.size(); i++) {
    assert (indexOf (varIds[i]) == -1);
    varids_.push_back (varIds[i]);
    ranges_.push_back (ranges[i]);
    nrStates *= ranges[i];
  }
  dist_->params.clear();
  dist_->params.reserve (oldParams.size() * nrStates);
  for (unsigned i = 0; i < oldParams.size(); i++) {
    for (unsigned reps = 0; reps < nrStates; reps++) {
      dist_->params.push_back (oldParams[i]);
    }
  }
}



void
Factor::sumOutAllExcept (VarId vid)
{
  assert (indexOf (vid) != -1);
  while (varids_.back() != vid) {
    sumOutLastVariable();
  }
  while (varids_.front() != vid) {
    sumOutFirstVariable();
  } 
}



void
Factor::sumOutAllExcept (const VarIds& vids)
{
  for (unsigned i = 0; i < varids_.size(); i++) {
    if (std::find (vids.begin(), vids.end(), varids_[i]) == vids.end()) {
      sumOut (varids_[i]);
    }
  }
}



void
Factor::sumOut (VarId vid)
{
  int idx = indexOf (vid);
  assert (idx != -1);

  if (vid == varids_.back()) {
    sumOutLastVariable();  // optimization
    return;
  } 
  if (vid == varids_.front()) {
    sumOutFirstVariable(); // optimization
    return;
  } 

  // number of parameters separating a different state of `var', 
  // with the states of the remaining variables fixed
  unsigned varOffset = 1;

  // number of parameters separating a different state of the variable
  // on the left of `var', with the states of the remaining vars fixed
  unsigned leftVarOffset = 1;

  for (int i = varids_.size() - 1; i > idx; i--) {
    varOffset     *= ranges_[i];
    leftVarOffset *= ranges_[i];
  }
  leftVarOffset *= ranges_[idx];

  unsigned offset    = 0;
  unsigned count1    = 0;
  unsigned count2    = 0;
  unsigned newpsSize = dist_->params.size() / ranges_[idx];

  Params newps;
  newps.reserve (newpsSize);
  Params& params = dist_->params;

  while (newps.size() < newpsSize) {
    double sum = Util::addIdenty();
    for (unsigned i = 0; i < ranges_[idx]; i++) {
      if (Globals::logDomain) {
        Util::logSum (sum, params[offset]);
      } else {
        sum += params[offset];
      }
      offset += varOffset;
    }
    newps.push_back (sum);
    count1 ++;
    if (idx == (int)varids_.size() - 1) {
      offset = count1 * ranges_[idx];
    } else {
      if (((offset - varOffset + 1) % leftVarOffset) == 0) {
        count1 = 0;
        count2 ++;
      }
      offset = (leftVarOffset * count2) + count1;
    }
  }
  varids_.erase (varids_.begin() + idx);
  ranges_.erase (ranges_.begin() + idx);
  dist_->params = newps;
}



void
Factor::sumOutFirstVariable (void)
{
  Params& params = dist_->params;
  unsigned nStates = ranges_.front();
  unsigned sep = params.size() / nStates;
  if (Globals::logDomain) {
    for (unsigned i = sep; i < params.size(); i++) {
      Util::logSum (params[i % sep], params[i]);
    }
  } else {
    for (unsigned i = sep; i < params.size(); i++) {
      params[i % sep] += params[i];
    }
  }
  params.resize (sep);
  varids_.erase (varids_.begin());
  ranges_.erase (ranges_.begin());
}



void
Factor::sumOutLastVariable (void)
{
  Params& params = dist_->params;
  unsigned nStates = ranges_.back();
  unsigned idx1 = 0;
  unsigned idx2 = 0;
  if (Globals::logDomain) {
    while (idx1 < params.size()) {
      params[idx2] = params[idx1];
      idx1 ++;
      for (unsigned j = 1; j < nStates; j++) {
        Util::logSum (params[idx2], params[idx1]);
        idx1 ++;
      }
      idx2 ++;
    }
  } else {
    while (idx1 < params.size()) {
      params[idx2] = params[idx1];
      idx1 ++;
      for (unsigned j = 1; j < nStates; j++) {
        params[idx2] += params[idx1];
        idx1 ++;
      }
      idx2 ++;
    }
  }
  params.resize (idx2);
  varids_.pop_back();
  ranges_.pop_back();
}



void
Factor::orderVariables (void)
{
  VarIds sortedVarIds = varids_;
  sort (sortedVarIds.begin(), sortedVarIds.end());
  reorderVariables (sortedVarIds);
}



void
Factor::reorderVariables (const VarIds& newVarIds)
{
  assert (newVarIds.size() == varids_.size());
  if (newVarIds == varids_) {
    return;
  }

  Ranges newRanges;
  vector<unsigned> positions;
  for (unsigned i = 0; i < newVarIds.size(); i++) {
    unsigned idx = indexOf (newVarIds[i]);
    newRanges.push_back (ranges_[idx]);
    positions.push_back (idx);
  }

  unsigned N = ranges_.size();
  Params newParams (dist_->params.size());
  for (unsigned i = 0; i < dist_->params.size(); i++) {
    unsigned li = i;
    // calculate vector index corresponding to linear index
    vector<unsigned> vi (N);
    for (int k = N-1; k >= 0; k--) {
      vi[k] = li % ranges_[k];
      li /= ranges_[k];
    }
    // convert permuted vector index to corresponding linear index
    unsigned prod = 1;
    unsigned new_li = 0;
    for (int k = N-1; k >= 0; k--) {
      new_li += vi[positions[k]] * prod;
      prod *= ranges_[positions[k]];
    }
    newParams[new_li] = dist_->params[i];
  }
  varids_ = newVarIds;
  ranges_ = newRanges;
  dist_->params = newParams;
}



void
Factor::absorveEvidence (VarId vid, unsigned evidence)
{
  int idx = indexOf (vid);
  assert (idx != -1);

  Params oldParams = dist_->params;
  dist_->params.clear();
  dist_->params.reserve (oldParams.size() / ranges_[idx]);
  StatesIndexer indexer (ranges_);
  for (unsigned i = 0; i < evidence; i++) {
    indexer.increment (idx);
  }
  while (indexer.valid()) {
    dist_->params.push_back (oldParams[indexer]);
    indexer.incrementExcluding (idx);
  }
  varids_.erase (varids_.begin() + idx);
  ranges_.erase (ranges_.begin() + idx);
}



void
Factor::normalize (void)
{
  Util::normalize (dist_->params);
}



bool
Factor::contains (const VarIds& vars) const
{
  for (unsigned i = 0; i < vars.size(); i++) {
    if (indexOf (vars[i]) == -1) {
      return false;
    }
  }
  return true;
}



int
Factor::indexOf (VarId vid) const
{
  for (unsigned i = 0; i < varids_.size(); i++) {
    if (varids_[i] == vid) {
      return i;
    }
  }
  return -1;
}



string
Factor::getLabel (void) const
{
  stringstream ss;
  ss << "f(" ;
  for (unsigned i = 0; i < varids_.size(); i++) {
    if (i != 0) ss << "," ;
    ss << VarNode (varids_[i], ranges_[i]).label();
  }
  ss << ")" ;
  return ss.str();
}



void
Factor::print (void) const
{
  VarNodes vars;
  for (unsigned i = 0; i < varids_.size(); i++) {
    vars.push_back (new VarNode (varids_[i], ranges_[i]));
  }
  vector<string> jointStrings = Util::getJointStateStrings (vars);
  for (unsigned i = 0; i < dist_->params.size(); i++) {
    cout << "f(" << jointStrings[i] << ")" ;
    cout << " = " << dist_->params[i] << endl;
  }
  cout << endl;
  for (unsigned i = 0; i < vars.size(); i++) {
    delete vars[i];
  }
}


