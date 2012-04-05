#ifndef HORUS_BAYESBALL_H
#define HORUS_BAYESBALL_H

#include <vector>
#include <queue>
#include <list>
#include <map>

#include "GraphicalModel.h"
#include "Horus.h"

#include "FactorGraph.h"
#include "BayesNet.h"

using namespace std;


struct ScheduleInfo
{
  ScheduleInfo (DAGraphNode* n, bool vfp, bool vfc) : 
      node(n), visitedFromParent(vfp), visitedFromChild(vfc) { }

  DAGraphNode* node;
  bool visitedFromParent;
  bool visitedFromChild;
};


typedef queue<ScheduleInfo, list<ScheduleInfo>> Scheduling;


class BayesBall
{
  public:
   BayesBall (FactorGraph& fg) 
       : fg_(fg) , dag_(fg.getStructure())
   {
     dag_.clear();
   }

   FactorGraph* getMinimalFactorGraph (const VarIds&);

   static FactorGraph* getMinimalFactorGraph (FactorGraph& fg, VarIds vids)
   {
     BayesBall bb (fg);
     return bb.getMinimalFactorGraph (vids);
   }

  private:

    void constructGraph (FactorGraph* fg) const;

    void scheduleParents (const DAGraphNode* n, Scheduling& sch) const;

    void scheduleChilds (const DAGraphNode* n, Scheduling& sch) const;

    FactorGraph& fg_;

    DAGraph& dag_;
};



inline void
BayesBall::scheduleParents (const DAGraphNode* n, Scheduling& sch) const
{
  const vector<DAGraphNode*>& ps = n->parents();
  for (vector<DAGraphNode*>::const_iterator it = ps.begin(); 
      it != ps.end(); it++) {
    sch.push (ScheduleInfo (*it, false, true));
  }
}



inline void
BayesBall::scheduleChilds (const DAGraphNode* n, Scheduling& sch) const
{
  const vector<DAGraphNode*>& cs = n->childs();
  for (vector<DAGraphNode*>::const_iterator it = cs.begin();
      it != cs.end(); it++) {
    sch.push (ScheduleInfo (*it, true, false));
  }
}

#endif // HORUS_BAYESBALL_H

