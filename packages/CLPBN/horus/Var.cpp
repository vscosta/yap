#include <sstream>

#include "Var.h"


namespace Horus {

std::unordered_map<VarId, Var::VarInfo> Var::varsInfo_;


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
Var::label() const
{
  if (Var::varsHaveInfo()) {
    assert (Util::contains (varsInfo_, varId_));
    return varsInfo_.find (varId_)->second.first;
  }
  std::stringstream ss;
  ss << "x" << varId_;
  return ss.str();
}



States
Var::states() const
{
  if (Var::varsHaveInfo()) {
    assert (Util::contains (varsInfo_, varId_));
    return varsInfo_.find (varId_)->second.second;
  }
  States states;
  for (unsigned i = 0; i < range_; i++) {
    std::stringstream ss;
    ss << i ;
    states.push_back (ss.str());
  }
  return states;
}



void
Var::addVarInfo (
    VarId vid, std::string label, const States& states)
{
  assert (Util::contains (varsInfo_, vid) == false);
  varsInfo_.insert (std::make_pair (vid, VarInfo (label, states)));
}



bool
Var::varsHaveInfo()
{
  return varsInfo_.empty() == false;
}



void
Var::clearVarsInfo()
{
  varsInfo_.clear();
}

}  // namespace Horus

