#include <algorithm>
#include <sstream>

#include "Var.h"

using namespace std;


unordered_map<VarId, VarInfo> Var::varsInfo_;


Var::Var (const Var* v)
{
  varId_    = v->varId();
  range_    = v->range();
  evidence_ = v->getEvidence();
  index_    = std::numeric_limits<unsigned>::max();
}



Var::Var (VarId varId, unsigned range, int evidence)
{
  assert (range != 0);
  assert (evidence < (int) range);
  varId_    = varId;
  range_    = range;
  evidence_ = evidence;
  index_    = std::numeric_limits<unsigned>::max();
}



bool
Var::isValidState (int stateIndex)
{
  return stateIndex >= 0 && stateIndex < (int) range_;
}



bool
Var::isValidState (const string& stateName)
{
  States states = Var::getVarInformation (varId_).states;
  return Util::contains (states, stateName);
}



void
Var::setEvidence (int ev) 
{
  assert (ev < (int) range_);
  evidence_ = ev;
}



void
Var::setEvidence (const string& ev) 
{ 
  States states = Var::getVarInformation (varId_).states;
  for (unsigned i = 0; i < states.size(); i++) {
    if (states[i] == ev) {
      evidence_ = i;
      return;
    }
  }
  assert (false);
}



string
Var::label (void) const
{
  if (Var::variablesHaveInformation()) {
    return Var::getVarInformation (varId_).label;
  }
  stringstream ss;
  ss << "x" << varId_;
  return ss.str();
}



States
Var::states (void) const
{
  if (Var::variablesHaveInformation()) {
    return Var::getVarInformation (varId_).states;
  }
  States states;
  for (unsigned i = 0; i < range_; i++) {
    stringstream ss;
    ss << i ;
    states.push_back (ss.str());
  }
  return states;
}

