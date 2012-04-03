#ifndef HORUS_GRAPHICALMODEL_H
#define HORUS_GRAPHICALMODEL_H

#include <cassert>

#include <unordered_map>

#include <sstream>

#include "VarNode.h"
#include "Util.h"
#include "Horus.h"

using namespace std; 


struct VarInfo
{
  VarInfo (string l, const States& sts) : label(l), states(sts) { }
  string label;
  States states;
};


class GraphicalModel
{
  public:
    virtual ~GraphicalModel (void) { };

    virtual VarNode* getVariableNode (VarId)  const = 0;

    virtual VarNodes getVariableNodes (void) const = 0;

    virtual void printGraphicalModel (void) const = 0;
  
    static void addVariableInformation (
        VarId vid, string label, const States& states)
    {
      assert (Util::contains (varsInfo_, vid) == false);
      varsInfo_.insert (make_pair (vid, VarInfo (label, states)));
    }

    static VarInfo getVarInformation (VarId vid)
    {
      assert (Util::contains (varsInfo_, vid));
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
    static unordered_map<VarId,VarInfo> varsInfo_;
};

#endif // HORUS_GRAPHICALMODEL_H

