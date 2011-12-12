#ifndef HORUS_BAYESNET_H
#define HORUS_BAYESNET_H

#include <vector>
#include <queue>
#include <list>
#include <map>

#include "GraphicalModel.h"
#include "BayesNode.h"
#include "Shared.h"


using namespace std;

class Distribution;

struct ScheduleInfo
{
  ScheduleInfo (BayesNode* n, bool vfp, bool vfc)
  {
    node               = n;
    visitedFromParent  = vfp;
    visitedFromChild   = vfc;
  }
  BayesNode*  node;
  bool        visitedFromParent;
  bool        visitedFromChild;
};


struct StateInfo
{
  StateInfo (void)
  {
    visited         = true;
    markedOnTop     = false;
    markedOnBottom  = false;
  }
  bool visited;
  bool markedOnTop;
  bool markedOnBottom;
};

typedef vector<Distribution*>  DistSet;
typedef queue<ScheduleInfo, list<ScheduleInfo> > Scheduling;


class BayesNet : public GraphicalModel
{
  public:
    BayesNet (void) {};
   ~BayesNet (void);

    void               readFromBifFormat (const char*);
    void               addNode (BayesNode*);
    BayesNode*         addNode (string, const States&);
    BayesNode*         addNode (VarId, unsigned, int, BnNodeSet&, Distribution*);
    BayesNode*         addNode (VarId, unsigned, int, Distribution*);
    BayesNode*         addNode (string, States, BnNodeSet&, ParamSet&);
    BayesNode*         getBayesNode (VarId) const;
    BayesNode*         getBayesNode (string) const;
    VarNode*           getVariableNode (VarId) const;
    VarNodes           getVariableNodes (void) const;
    void               addDistribution (Distribution*);
    Distribution*      getDistribution (unsigned) const;
    const BnNodeSet&   getBayesNodes (void) const;
    unsigned           nrNodes (void) const;
    BnNodeSet          getRootNodes (void) const;
    BnNodeSet          getLeafNodes (void) const;
    BayesNet*          getMinimalRequesiteNetwork (VarId) const;
    BayesNet*          getMinimalRequesiteNetwork (const VarIdSet&) const;
    void               constructGraph (
                           BayesNet*, const vector<StateInfo*>&) const;
    bool               isPolyTree (void) const;
    void               setIndexes (void);
    void               distributionsToLogs (void);
    void               freeDistributions (void);
    void               printGraphicalModel (void) const;
    void               exportToGraphViz (const char*, bool = true,
                           const VarIdSet& = VarIdSet()) const;
    void               exportToBifFormat (const char*) const;

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNet);

    bool               containsUndirectedCycle (void) const;
    bool               containsUndirectedCycle (int, int, vector<bool>&)const;
    vector<int>        getAdjacentNodes (int) const;
    ParamSet           reorderParameters (const ParamSet&, unsigned) const;
    ParamSet           revertParameterReorder (const ParamSet&, unsigned) const;
    void               scheduleParents (const BayesNode*, Scheduling&) const;
    void               scheduleChilds (const BayesNode*, Scheduling&) const;

    BnNodeSet          nodes_;
    DistSet            dists_;

    typedef unordered_map<unsigned, unsigned> IndexMap;
    IndexMap           indexMap_;
};



inline void
BayesNet::scheduleParents (const BayesNode* n, Scheduling& sch) const
{
  const BnNodeSet& ps = n->getParents();
  for (BnNodeSet::const_iterator it = ps.begin(); it != ps.end(); it++) {
    sch.push (ScheduleInfo (*it, false, true));
  }
}



inline void
BayesNet::scheduleChilds (const BayesNode* n, Scheduling& sch) const
{
  const BnNodeSet& cs = n->getChilds();
  for (BnNodeSet::const_iterator it = cs.begin(); it != cs.end(); it++) {
    sch.push (ScheduleInfo (*it, true, false));
  }
}

#endif // HORUS_BAYESNET_H

