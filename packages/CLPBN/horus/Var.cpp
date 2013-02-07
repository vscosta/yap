#include <sstream>

#include "Var.h"


std::unordered_map<VarId, VarInfo> Var::varsInfo_;


Var::Var (const Var* v)
{
  varId_    = v->varId();
  range_    = v->range();
  evidence_ = v->getEvidence();
  index_    = Util::maxUnsigned();
}



Var::Var (VarId varId, unsigned range, int evidence)
{
  assert (range != 0);
  assert (evidence < (int) range);
  varId_    = varId;
  range_    = range;
  evidence_ = evidence;
  index_    = Util::maxUnsigned();
}



bool
Var::isValidState (int stateIndex)
{
  return stateIndex >= 0 && stateIndex < (int) range_;
}



void
Var::setEvidence (int evidence)
{
  assert (evidence < (int) range_);
  evidence_ = evidence;
}



std::string
Var::label (void) const
{
  if (Var::varsHaveInfo()) {
    return Var::getVarInfo (varId_).label;
  }
  std::stringstream ss;
  ss << "x" << varId_;
  return ss.str();
}



States
Var::states (void) const
{
  if (Var::varsHaveInfo()) {
    return Var::getVarInfo (varId_).states;
  }
  States states;
  for (unsigned i = 0; i < range_; i++) {
    std::stringstream ss;
    ss << i ;
    states.push_back (ss.str());
  }
  return states;
}



inline void
Var::addVarInfo (
    VarId vid, std::string label, const States& states)
{
  assert (Util::contains (varsInfo_, vid) == false);
  varsInfo_.insert (std::make_pair (vid, VarInfo (label, states)));
}



inline VarInfo
Var::getVarInfo (VarId vid)
{
  assert (Util::contains (varsInfo_, vid));
  return varsInfo_.find (vid)->second;
}



inline bool
Var::varsHaveInfo (void)
{
  return varsInfo_.empty() == false;
}



inline void
Var::clearVarsInfo (void)
{
  varsInfo_.clear();
}

