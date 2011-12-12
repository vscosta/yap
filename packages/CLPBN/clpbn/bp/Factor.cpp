#include <cstdlib>
#include <cassert>

#include <algorithm>

#include <iostream>
#include <sstream>

#include "Factor.h"
#include "StatesIndexer.h"


Factor::Factor (const Factor& g)
{
  copyFromFactor (g);
}



Factor::Factor (VarId vid, unsigned nStates)
{
  varids_.push_back (vid);
  ranges_.push_back (nStates);
  dist_ = new Distribution (ParamSet (nStates, 1.0));
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
  dist_ = new Distribution (ParamSet (nParams, val));
}



Factor::Factor (VarId vid, unsigned nStates, const ParamSet& params)
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



Factor::Factor (const VarNodes& vars, const ParamSet& params)
{
  for (unsigned i = 0; i < vars.size(); i++) {
    varids_.push_back (vars[i]->varId());
    ranges_.push_back (vars[i]->nrStates());
  }
  dist_ = new Distribution (params);
}



Factor::Factor (const VarIdSet& vids,
                const Ranges& ranges,
                const ParamSet& params)
{
  varids_ = vids;
  ranges_ = ranges;
  dist_   = new Distribution (params);
}



void
Factor::setParameters (const ParamSet& params)
{
  assert (dist_->params.size() == params.size());
  dist_->updateParameters (params);
}



void
Factor::copyFromFactor (const Factor& g)
{
  varids_ = g.getVarIds();
  ranges_ = g.getRanges();
  dist_ = new Distribution (g.getDistribution()->params);
}



void
Factor::multiplyByFactor (const Factor& g, const vector<CptEntry>* entries)
{
  if (varids_.size() == 0) {
    copyFromFactor (g);
    return;
  }

  const VarIdSet&  gvarids = g.getVarIds();
  const Ranges&    granges = g.getRanges();
  const ParamSet&  gparams = g.getParameters();

  if (varids_ == gvarids) {
    // optimization: if the factors contain the same set of variables,
    // we can do a 1 to 1 operation on the parameters
    switch (NSPACE) {
      case NumberSpace::NORMAL:
        Util::multiply (dist_->params, gparams);
        break;
      case NumberSpace::LOGARITHM:
        Util::add (dist_->params, gparams);
    }
  } else {
    bool hasCommonVars = false;
    vector<unsigned> gvarpos;
    for (unsigned i = 0; i < gvarids.size(); i++) {
      int pos = getPositionOf (gvarids[i]);
      if (pos == -1) {
        insertVariable (gvarids[i], granges[i]);
        gvarpos.push_back (varids_.size() - 1);
      } else {
        hasCommonVars = true;
        gvarpos.push_back (pos);
      }
    }
    if (hasCommonVars) {
      vector<unsigned> gvaroffsets (gvarids.size());
      gvaroffsets[gvarids.size() - 1] = 1;
      for (int i = gvarids.size() - 2; i >= 0; i--) {
        gvaroffsets[i] = gvaroffsets[i + 1] * granges[i + 1];
      }

      if (entries == 0) {
        entries = &getCptEntries();
      }

      for (unsigned i = 0; i < entries->size(); i++) {
        unsigned idx = 0;
        const DConf& conf = (*entries)[i].getDomainConfiguration();
        for (unsigned j = 0; j < gvarpos.size(); j++) {
          idx += gvaroffsets[j] * conf[ gvarpos[j] ];
        }
        switch (NSPACE) {
          case NumberSpace::NORMAL:
            dist_->params[i] *= gparams[idx];
            break;
          case NumberSpace::LOGARITHM:
            dist_->params[i] += gparams[idx];
        }
      }
    } else {
      // optimization: if the original factors doesn't have common variables,
      // we don't need to marry the states of the common variables
      unsigned count = 0;
      for (unsigned i = 0; i < dist_->params.size(); i++) {
        switch (NSPACE) {
          case NumberSpace::NORMAL:
            dist_->params[i] *= gparams[count];
            break;
          case NumberSpace::LOGARITHM:
            dist_->params[i] += gparams[count];
        }
        count ++;
        if (count >= gparams.size()) {
          count = 0;
        }
      }
    }
  }
  dist_->entries.clear();
}



void
Factor::insertVariable (VarId vid, unsigned nStates)
{
  assert (getPositionOf (vid) == -1);
  ParamSet newPs;
  newPs.reserve (dist_->params.size() * nStates);
  for (unsigned i = 0; i < dist_->params.size(); i++) {
    for (unsigned j = 0; j < nStates; j++) {
      newPs.push_back (dist_->params[i]);
    }
  }
  varids_.push_back (vid);
  ranges_.push_back (nStates);
  dist_->updateParameters (newPs);
  dist_->entries.clear();
}



void
Factor::removeAllVariablesExcept (VarId vid)
{
  assert (getPositionOf (vid) != -1);
  while (varids_.back() != vid) {
    removeLastVariable();
  }
  while (varids_.front() != vid) {
    removeFirstVariable();
  } 
}



void
Factor::removeVariable (VarId vid)
{
  int pos = getPositionOf (vid);
  assert (pos != -1);

  if (vid == varids_.back()) {
    removeLastVariable();  // optimization
    return;
  } 
  if (vid == varids_.front()) {
    removeFirstVariable(); // optimization
    return;
  } 

  // number of parameters separating a different state of `var', 
  // with the states of the remaining variables fixed
  unsigned varOffset = 1;

  // number of parameters separating a different state of the variable
  // on the left of `var', with the states of the remaining vars fixed
  unsigned leftVarOffset = 1;

  for (int i = varids_.size() - 1; i > pos; i--) {
    varOffset     *= ranges_[i];
    leftVarOffset *= ranges_[i];
  }
  leftVarOffset *= ranges_[pos];

  unsigned offset    = 0;
  unsigned count1    = 0;
  unsigned count2    = 0;
  unsigned newPsSize = dist_->params.size() / ranges_[pos];

  ParamSet newPs;
  newPs.reserve (newPsSize);

  while (newPs.size() < newPsSize) {
    double sum = Util::addIdenty();
    for (unsigned i = 0; i < ranges_[pos]; i++) {
      switch (NSPACE) {
        case NumberSpace::NORMAL:
          sum += dist_->params[offset];
          break;
        case NumberSpace::LOGARITHM:
          Util::logSum (sum, dist_->params[offset]);
      }
      offset += varOffset;
    }
    newPs.push_back (sum);
    count1 ++;
    if (pos == (int)varids_.size() - 1) {
      offset = count1 * ranges_[pos];
    } else {
      if (((offset - varOffset + 1) % leftVarOffset) == 0) {
        count1 = 0;
        count2 ++;
      }
      offset = (leftVarOffset * count2) + count1;
    }
  }
  varids_.erase (varids_.begin() + pos);
  ranges_.erase (ranges_.begin() + pos);
  dist_->updateParameters (newPs);
  dist_->entries.clear();
}



void
Factor::removeFirstVariable (void)
{
  ParamSet& params = dist_->params;
  unsigned nStates = ranges_.front();
  unsigned sep = params.size() / nStates;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned i = sep; i < params.size(); i++) {
        params[i % sep] += params[i];
      }
      break;
    case NumberSpace::LOGARITHM:
      for (unsigned i = sep; i < params.size(); i++) {
        Util::logSum (params[i % sep], params[i]);
      }
  }
  params.resize (sep);
  varids_.erase (varids_.begin());
  ranges_.erase (ranges_.begin());
  dist_->entries.clear();
}



void
Factor::removeLastVariable (void)
{
  ParamSet& params = dist_->params;
  unsigned nStates = ranges_.back();
  unsigned idx1 = 0;
  unsigned idx2 = 0;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      while (idx1 < params.size()) {
        params[idx2] = params[idx1];
        idx1 ++;
        for (unsigned j = 1; j < nStates; j++) {
          params[idx2] += params[idx1];
          idx1 ++;
        }
        idx2 ++;
      }
      break;
    case NumberSpace::LOGARITHM:
      while (idx1 < params.size()) {
        params[idx2] = params[idx1];
        idx1 ++;
        for (unsigned j = 1; j < nStates; j++) {
          Util::logSum (params[idx2], params[idx1]);
          idx1 ++;
        }
        idx2 ++;
      }
  }
  params.resize (idx2);
  varids_.pop_back();
  ranges_.pop_back();
  dist_->entries.clear();
}



void
Factor::orderVariables (void)
{
  VarIdSet sortedVarIds = varids_;
  sort (sortedVarIds.begin(), sortedVarIds.end());
  orderVariables (sortedVarIds);
}



void
Factor::orderVariables (const VarIdSet& newVarIdOrder)
{
  assert (newVarIdOrder.size() == varids_.size());
  if (newVarIdOrder == varids_) {
    return;
  }

  Ranges newRangeOrder;
  for (unsigned i = 0; i < newVarIdOrder.size(); i++) {
    unsigned pos = getPositionOf (newVarIdOrder[i]);
    newRangeOrder.push_back (ranges_[pos]);
  }

  vector<unsigned> positions;
  for (unsigned i = 0; i < newVarIdOrder.size(); i++) {
    positions.push_back (getPositionOf (newVarIdOrder[i]));
  }
      
  unsigned N = ranges_.size();
  ParamSet newPs (dist_->params.size());
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
    newPs[new_li] = dist_->params[i];
  }
  varids_ = newVarIdOrder;
  ranges_ = newRangeOrder;
  dist_->params = newPs;
  dist_->entries.clear(); 
}



void
Factor::removeInconsistentEntries (VarId vid, unsigned evidence)
{
  int pos = getPositionOf (vid);
  assert (pos != -1);
  ParamSet newPs;
  newPs.reserve (dist_->params.size() / ranges_[pos]);
  StatesIndexer idx (ranges_);
  for (unsigned i = 0; i < evidence; i++) {
    idx.incrementState (pos);
  }
  while (idx.valid()) {
    newPs.push_back (dist_->params[idx.getLinearIndex()]);
    idx.nextSameState (pos);
  }
  varids_.erase (varids_.begin() + pos);
  ranges_.erase (ranges_.begin() + pos);
  dist_->updateParameters (newPs);
  dist_->entries.clear();
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
Factor::printFactor (void) const
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
  for (unsigned i = 0; i < vars.size(); i++) {
    delete vars[i];
  }
}



int
Factor::getPositionOf (VarId vid) const
{
  for (unsigned i = 0; i < varids_.size(); i++) {
    if (varids_[i] == vid) {
      return i;
    }
  }
  return -1;
}



const vector<CptEntry>&
Factor::getCptEntries (void) const
{
  if (dist_->entries.size() == 0) {
    vector<DConf> confs (dist_->params.size());
    for (unsigned i = 0; i < dist_->params.size(); i++) {
      confs[i].resize (varids_.size());
    }
    unsigned nReps = 1;
    for (int i = varids_.size() - 1; i >= 0; i--) {
      unsigned index = 0;
      while (index < dist_->params.size()) {
        for (unsigned j = 0; j < ranges_[i]; j++) {
          for (unsigned r = 0; r < nReps; r++) {
            confs[index][i] = j;
            index++;
          }
        }
      }
      nReps *= ranges_[i];
    }
    dist_->entries.clear();
    dist_->entries.reserve (dist_->params.size());
    for (unsigned i = 0; i < dist_->params.size(); i++) {
      dist_->entries.push_back (CptEntry (i, confs[i]));
    }
  }
  return dist_->entries;
}

