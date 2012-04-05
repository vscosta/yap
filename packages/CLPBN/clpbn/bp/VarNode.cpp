#include <algorithm>
#include <sstream>

#include "VarNode.h"
#include "GraphicalModel.h"

using namespace std;


VarNode::VarNode (const VarNode* v)
{
  varId_    = v->varId();
  range_    = v->range();
  evidence_ = v->getEvidence();
  index_    = std::numeric_limits<unsigned>::max();
}



VarNode::VarNode (VarId varId, unsigned range, int evidence)
{
  assert (range != 0);
  assert (evidence < (int) range);
  varId_    = varId;
  range_    = range;
  evidence_ = evidence;
  index_    = std::numeric_limits<unsigned>::max();
}



bool
VarNode::isValidState (int stateIndex)
{
  return stateIndex >= 0 && stateIndex < (int) range_;
}



bool
VarNode::isValidState (const string& stateName)
{
  States states = GraphicalModel::getVarInformation (varId_).states;
  return Util::contains (states, stateName);
}



void
VarNode::setEvidence (int ev) 
{
  assert (ev < (int) range_);
  evidence_ = ev;
}



void
VarNode::setEvidence (const string& ev) 
{ 
  States states = GraphicalModel::getVarInformation (varId_).states;
  for (unsigned i = 0; i < states.size(); i++) {
    if (states[i] == ev) {
      evidence_ = i;
      return;
    }
  }
  assert (false);
}



string
VarNode::label (void) const
{
  if (GraphicalModel::variablesHaveInformation()) {
    return GraphicalModel::getVarInformation (varId_).label;
  }
  stringstream ss;
  ss << "x" << varId_;
  return ss.str();
}



States
VarNode::states (void) const
{
  if (GraphicalModel::variablesHaveInformation()) {
    return GraphicalModel::getVarInformation (varId_).states;
  }
  States states;
  for (unsigned i = 0; i < range_; i++) {
    stringstream ss;
    ss << i ;
    states.push_back (ss.str());
  }
  return states;
}

