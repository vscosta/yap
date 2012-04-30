#include <cstdlib>
#include <cassert>

#include <algorithm>

#include <iostream>
#include <sstream>

#include "Factor.h"
#include "Indexer.h"


Factor::Factor (const Factor& g)
{
  copyFromFactor (g);
}



Factor::Factor (
    const VarIds& vids,
    const Ranges& ranges,
    const Params& params,
    unsigned distId)
{
  args_   = vids;
  ranges_ = ranges;
  params_ = params;
  distId_ = distId;
  assert (params_.size() == Util::expectedSize (ranges_));
}



Factor::Factor (
    const Vars& vars,
    const Params& params,
    unsigned distId)
{
  for (unsigned i = 0; i < vars.size(); i++) {
    args_.push_back (vars[i]->varId());
    ranges_.push_back (vars[i]->range());
  }
  params_ = params;
  distId_ = distId;
  assert (params_.size() == Util::expectedSize (ranges_));
}



void
Factor::sumOut (VarId vid)
{
  int idx = indexOf (vid);
  assert (idx != -1);
  if (vid == args_.back()) {
    sumOutLastVariable();  // optimization
    return;
  } 
  if (vid == args_.front()) {
    sumOutFirstVariable(); // optimization
    return;
  } 
  sumOutIndex (idx);  
}



void
Factor::sumOutAllExcept (VarId vid)
{
  assert (indexOf (vid) != -1);
  while (args_.back() != vid) {
    sumOutLastVariable();
  }
  while (args_.front() != vid) {
    sumOutFirstVariable();
  } 
}



void
Factor::sumOutAllExcept (const VarIds& vids)
{
  for (int i = 0; i < (int)args_.size(); i++) {
    if (Util::contains (vids, args_[i]) == false) {
      sumOut (args_[i]);
      i --;
    }
  }
}



void
Factor::sumOutIndex (unsigned idx)
{
  assert (idx < args_.size());
  // number of parameters separating a different state of `var', 
  // with the states of the remaining variables fixed
  unsigned varOffset = 1;

  // number of parameters separating a different state of the variable
  // on the left of `var', with the states of the remaining vars fixed
  unsigned leftVarOffset = 1;

  for (int i = args_.size() - 1; i > (int)idx; i--) {
    varOffset     *= ranges_[i];
    leftVarOffset *= ranges_[i];
  }
  leftVarOffset *= ranges_[idx];

  unsigned offset    = 0;
  unsigned count1    = 0;
  unsigned count2    = 0;
  unsigned newpsSize = params_.size() / ranges_[idx];

  Params newps;
  newps.reserve (newpsSize);

  while (newps.size() < newpsSize) {
    double sum = LogAware::addIdenty();
    for (unsigned i = 0; i < ranges_[idx]; i++) {
      if (Globals::logDomain) {
        sum  = Util::logSum (sum, params_[offset]);
      } else {
        sum += params_[offset];
      }
      offset += varOffset;
    }
    newps.push_back (sum);
    count1 ++;
    if (idx == args_.size() - 1) {
      offset = count1 * ranges_[idx];
    } else {
      if (((offset - varOffset + 1) % leftVarOffset) == 0) {
        count1 = 0;
        count2 ++;
      }
      offset = (leftVarOffset * count2) + count1;
    }
  }
  args_.erase (args_.begin() + idx);
  ranges_.erase (ranges_.begin() + idx);
  params_ = newps;
}



void
Factor::sumOutAllExceptIndex (unsigned idx)
{
  assert (idx < args_.size());
  while (args_.size() > idx + 1) {
    sumOutLastVariable();
  }
  for (unsigned i = 0; i < idx; i++) {
    sumOutFirstVariable();
  }
}



void
Factor::sumOutFirstVariable (void)
{
  assert (args_.size() > 1);
  unsigned range = ranges_.front();
  unsigned sep = params_.size() / range;
  if (Globals::logDomain) {
    for (unsigned i = sep; i < params_.size(); i++) {
      params_[i % sep] = Util::logSum (params_[i % sep], params_[i]);
    }
  } else {
    for (unsigned i = sep; i < params_.size(); i++) {
      params_[i % sep] += params_[i];
    }
  }
  params_.resize (sep);
  args_.erase (args_.begin());
  ranges_.erase (ranges_.begin());
}



void
Factor::sumOutLastVariable (void)
{
  assert (args_.size() > 1);
  unsigned range = ranges_.back();
  unsigned idx1 = 0;
  unsigned idx2 = 0;
  if (Globals::logDomain) {
    while (idx1 < params_.size()) {
      params_[idx2] = params_[idx1];
      idx1 ++;
      for (unsigned j = 1; j < range; j++) {
        params_[idx2] = Util::logSum (params_[idx2], params_[idx1]);
        idx1 ++;
      }
      idx2 ++;
    }
  } else {
    while (idx1 < params_.size()) {
      params_[idx2] = params_[idx1];
      idx1 ++;
      for (unsigned j = 1; j < range; j++) {
        params_[idx2] += params_[idx1];
        idx1 ++;
      }
      idx2 ++;
    }
  }
  params_.resize (idx2);
  args_.pop_back();
  ranges_.pop_back();
}



void
Factor::multiply (Factor& g)
{
  if (args_.size() == 0) {
    copyFromFactor (g);
    return;
  }
  TFactor<VarId>::multiply (g);
}



void
Factor::reorderAccordingVarIds (void)
{
  VarIds sortedVarIds = args_;
  sort (sortedVarIds.begin(), sortedVarIds.end());
  reorderArguments (sortedVarIds);
}



string
Factor::getLabel (void) const
{
  stringstream ss;
  ss << "f(" ;
  for (unsigned i = 0; i < args_.size(); i++) {
    if (i != 0) ss << "," ;
    ss << Var (args_[i], ranges_[i]).label();
  }
  ss << ")" ;
  return ss.str();
}



void
Factor::print (void) const
{
  Vars vars;
  for (unsigned i = 0; i < args_.size(); i++) {
    vars.push_back (new Var (args_[i], ranges_[i]));
  }
  vector<string> jointStrings = Util::getStateLines (vars);
  for (unsigned i = 0; i < params_.size(); i++) {
    // cout << "[" << distId_ << "] " ;
    cout << "f(" << jointStrings[i] << ")" ;
    cout << " = " << params_[i] << endl;
  }
  cout << endl;
  for (unsigned i = 0; i < vars.size(); i++) {
    delete vars[i];
  }
}



void
Factor::copyFromFactor (const Factor& g)
{
  args_    = g.arguments();
  ranges_  = g.ranges();
  params_  = g.params();
  distId_  = g.distId();
}

