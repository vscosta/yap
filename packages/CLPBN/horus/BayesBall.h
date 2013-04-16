#ifndef YAP_PACKAGES_CLPBN_HORUS_BAYESBALL_H_
#define YAP_PACKAGES_CLPBN_HORUS_BAYESBALL_H_

#include <vector>
#include <queue>
#include <list>

#include "FactorGraph.h"
#include "BayesBallGraph.h"
#include "Horus.h"


namespace Horus {

class BayesBall {
  public:
    BayesBall (FactorGraph& fg);

    FactorGraph* getMinimalFactorGraph (const VarIds&);

    static FactorGraph* getMinimalFactorGraph (FactorGraph& fg, VarIds vids);

  private:
    struct ScheduleInfo {
      ScheduleInfo (BBNode* n, bool vfp, bool vfc)
        : node(n), visitedFromParent(vfp), visitedFromChild(vfc) { }

      BBNode*  node;
      bool     visitedFromParent;
      bool     visitedFromChild;
    };

    typedef std::queue<ScheduleInfo, std::list<ScheduleInfo>> Scheduling;

    void constructGraph (FactorGraph* fg) const;

    void scheduleParents (const BBNode* n, Scheduling& sch) const;

    void scheduleChilds (const BBNode* n, Scheduling& sch) const;

    FactorGraph&     fg_;
    BayesBallGraph&  dag_;
};



inline void
BayesBall::scheduleParents (const BBNode* n, Scheduling& sch) const
{
  const std::vector<BBNode*>& ps = n->parents();
  for (std::vector<BBNode*>::const_iterator it = ps.begin();
      it != ps.end(); ++it) {
    sch.push (ScheduleInfo (*it, false, true));
  }
}



inline void
BayesBall::scheduleChilds (const BBNode* n, Scheduling& sch) const
{
  const std::vector<BBNode*>& cs = n->childs();
  for (std::vector<BBNode*>::const_iterator it = cs.begin();
      it != cs.end(); ++it) {
    sch.push (ScheduleInfo (*it, true, false));
  }
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_BAYESBALL_H_

