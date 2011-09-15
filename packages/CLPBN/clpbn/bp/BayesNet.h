#ifndef BP_BAYES_NET_H
#define BP_BAYES_NET_H

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
typedef map<unsigned, unsigned> Histogram;
typedef map<unsigned, double>   Times;


class BayesNet : public GraphicalModel
{
  public:
    BayesNet (void) {};
    BayesNet (const char*);
   ~BayesNet (void);

    BayesNode*           addNode (unsigned);
    BayesNode*           addNode (unsigned, unsigned, int, BnNodeSet&,
                             Distribution*);
    BayesNode*           addNode (string, Domain, BnNodeSet&, ParamSet&);
    BayesNode*           getBayesNode (Vid) const;
    BayesNode*           getBayesNode (string) const;
    Variable*            getVariable (Vid) const;
    void                 addDistribution (Distribution*);
    Distribution*        getDistribution (unsigned) const;
    const BnNodeSet&     getBayesNodes (void) const;
    unsigned             getNumberOfNodes (void) const;
    BnNodeSet            getRootNodes (void) const;
    BnNodeSet            getLeafNodes (void) const;
    VarSet               getVariables (void) const;
    BayesNet*            getMinimalRequesiteNetwork (Vid) const;
    BayesNet*            getMinimalRequesiteNetwork (const VidSet&) const;
    void                 constructGraph (BayesNet*, 
                             const vector<StateInfo*>&) const;
    bool                 isSingleConnected (void) const;
    void                 setIndexes (void);
    void                 freeDistributions (void);
    void                 printGraphicalModel (void) const;
    void                 exportToDotFormat (const char*, bool = true,
                             CVidSet = VidSet()) const;
    void                 exportToBifFormat (const char*) const;

    static Histogram     histogram_;
    static Times         times_;

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNet);

    bool                 containsUndirectedCycle (void) const;
    bool                 containsUndirectedCycle (int, int,
                                                  vector<bool>&)const;
    vector<int>          getAdjacentNodes (int) const ;
    ParamSet             reorderParameters (CParamSet, unsigned) const;
    ParamSet             revertParameterReorder (CParamSet, unsigned) const;
    void                 scheduleParents (const BayesNode*, Scheduling&) const;
    void                 scheduleChilds (const BayesNode*, Scheduling&) const;

    BnNodeSet            nodes_;
    DistSet              dists_;
    IndexMap             indexMap_;
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

#endif //BP_BAYES_NET_H

