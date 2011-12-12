#ifndef HORUS_GRAPHICALMODEL_H
#define HORUS_GRAPHICALMODEL_H

#include "VarNode.h"
#include "Shared.h"

using namespace std; 

struct VariableInfo
{
  VariableInfo (string l, const States& sts)
  {
    label  = l;
    states = sts;
  }
  string  label;
  States  states;
};


class GraphicalModel
{
  public:
    virtual ~GraphicalModel (void) {};
    virtual VarNode*   getVariableNode      (VarId)  const = 0;
    virtual VarNodes   getVariableNodes     (void) const = 0;
    virtual void       printGraphicalModel  (void) const = 0;
  
    static void addVariableInformation (VarId vid, string label,
                                        const States& states)
    {
      assert (varsInfo_.find (vid) == varsInfo_.end());
      varsInfo_.insert (make_pair (vid, VariableInfo (label, states)));
    }
    static VariableInfo getVariableInformation (VarId vid)
    {
      assert (varsInfo_.find (vid) != varsInfo_.end());
      return varsInfo_.find (vid)->second;
    }
    static bool variablesHaveInformation (void)
    {
      return varsInfo_.size() != 0;
    }
    static void clearVariablesInformation (void)
    {
      varsInfo_.clear();
    }

  private:
    static unordered_map<VarId,VariableInfo> varsInfo_;
};

#endif // HORUS_GRAPHICALMODEL_H

