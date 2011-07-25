#ifndef BP_BAYES_NET_H
#define BP_BAYES_NET_H

#include <vector>
#include <queue>
#include <list>
#include <string>
#include <unordered_map>
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
typedef unordered_map<unsigned, unsigned> Histogram;
typedef unordered_map<unsigned, double>   Times;


class BayesNet : public GraphicalModel
{
  public:
    BayesNet (void);
    BayesNet (const char*);
   ~BayesNet (void);

    BayesNode*           addNode (unsigned);
    BayesNode*           addNode (unsigned, unsigned, int, NodeSet&, Distribution*);
    BayesNode*           addNode (string, Domain, NodeSet&, ParamSet&);
    BayesNode*           getNode (unsigned) const;
    BayesNode*           getNode (string) const;
    void                 addDistribution (Distribution*);
    Distribution*        getDistribution (unsigned) const;
    const NodeSet&       getNodes (void) const;
    int                  getNumberOfNodes (void) const;
    NodeSet              getRootNodes (void) const;
    NodeSet              getLeafNodes (void) const;
    VarSet               getVariables (void) const;
    BayesNet*            pruneNetwork (BayesNode*) const;
    BayesNet*            pruneNetwork (const NodeSet& queryNodes) const;
    void                 constructGraph (BayesNet*, const vector<StateInfo*>&) const;
    bool                 isSingleConnected (void) const;
    static vector<DomainConf> getDomainConfigurationsOf (const NodeSet&);
    static vector<string>     getInstantiations (const NodeSet& nodes);
    void                 setIndexes (void);
    void                 freeDistributions (void);
    void                 printNetwork (void) const;
    void                 printNetworkToFile (const char*) const;
    void                 exportToDotFile (const char*, bool = true,
                                          const NodeSet& = NodeSet()) const;
    void                 exportToBifFile (const char*) const;

    static Histogram     histogram_;
    static Times         times_;

  private:
    DISALLOW_COPY_AND_ASSIGN (BayesNet);

    bool                 containsUndirectedCycle (void) const;
    bool                 containsUndirectedCycle (int, int,
                                                  vector<bool>&)const;
    vector<int>          getAdjacentNodes (int) const ;
    ParamSet             reorderParameters (const ParamSet&, int) const;
    ParamSet             revertParameterReorder (const ParamSet&, int) const;
    void                 scheduleParents (const BayesNode*, Scheduling&) const;
    void                 scheduleChilds (const BayesNode*, Scheduling&) const;

    NodeSet              nodes_;
    DistSet              dists_;
    IndexMap             indexMap_;
};



inline void
BayesNet::scheduleParents (const BayesNode* n, Scheduling& sch) const
{
  const NodeSet& ps = n->getParents();
  for (NodeSet::const_iterator it = ps.begin(); it != ps.end(); it++) {
    sch.push (ScheduleInfo (*it, false, true));
  }
}



inline void
BayesNet::scheduleChilds (const BayesNode* n, Scheduling& sch) const
{
  const NodeSet& cs = n->getChilds();
  for (NodeSet::const_iterator it = cs.begin(); it != cs.end(); it++) {
    sch.push (ScheduleInfo (*it, true, false));
  }
}

#endif

