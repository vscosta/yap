#ifndef YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_
#define YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_

#include <cassert>

#include <vector>
#include <algorithm>
#include <string>

#include "TinySet.h"
#include "LiftedUtils.h"


namespace Horus {

class CTNode;
class ConstraintTree;


typedef std::vector<CTNode*>          CTNodes;
typedef std::vector<ConstraintTree*>  ConstraintTrees;


struct CmpSymbol {
  bool operator() (const CTNode* n1, const CTNode* n2) const;
};


typedef TinySet<CTNode*, CmpSymbol>   CTChilds;


class ConstraintTree {
  public:
    ConstraintTree (unsigned);

    ConstraintTree (const LogVars&);

    ConstraintTree (const LogVars&, const Tuples&);

    ConstraintTree (std::vector<std::vector<std::string>> names);

    ConstraintTree (const ConstraintTree&);

    ConstraintTree (const CTChilds& rootChilds, const LogVars& logVars);

   ~ConstraintTree();

    CTNode* root() const { return root_; }

    bool empty() const;

    const LogVars& logVars() const;

    const LogVarSet& logVarSet() const;

    size_t nrLogVars() const;

    void addTuple (const Tuple&);

    bool containsTuple (const Tuple&);

    void moveToTop (const LogVars&);

    void moveToBottom (const LogVars&);

    void join (ConstraintTree*, bool oneTwoOne = false);

    unsigned getLevel (LogVar) const;

    void rename (LogVar, LogVar);

    void applySubstitution (const Substitution&);

    void project (const LogVarSet&);

    ConstraintTree projectedCopy (const LogVarSet&);

    void remove (const LogVarSet&);

    bool isSingleton (LogVar);

    LogVarSet singletons();

    TupleSet tupleSet (unsigned = 0) const;

    TupleSet tupleSet (const LogVars&);

    unsigned size() const;

    unsigned nrSymbols (LogVar);

    void exportToGraphViz (const char*, bool = false) const;

    bool isCountNormalized (const LogVarSet&);

    unsigned getConditionalCount (const LogVarSet&);

    TinySet<unsigned> getConditionalCounts (const LogVarSet&);

    bool isCartesianProduct (const LogVarSet&);

    std::pair<ConstraintTree*, ConstraintTree*> split (const Tuple&);

    std::pair<ConstraintTree*, ConstraintTree*> split (
        const LogVars&, ConstraintTree*, const LogVars&);

    ConstraintTrees countNormalize (const LogVarSet&);

    ConstraintTrees jointCountNormalize (
        ConstraintTree*, ConstraintTree*, LogVar, LogVar, LogVar);

    LogVars expand (LogVar);

    ConstraintTrees ground (LogVar);

    void cloneLogVar (LogVar, LogVar);

    ConstraintTree& operator= (const ConstraintTree& ct);

  private:
    unsigned countTuples (const CTNode*) const;

    CTNodes getNodesBelow (CTNode*) const;

    CTNodes getNodesAtLevel (unsigned) const;

    unsigned nrNodes (const CTNode* n) const;

    void appendOnBottom (CTNode* n1, const CTChilds&);

    void swapLogVar (LogVar);

    bool join (CTNode*, const Tuple&, size_t, CTNode*);

    void getTuples (CTNode*, Tuples, unsigned, Tuples&, CTNodes&) const;

    std::vector<std::pair<CTNode*, unsigned>> countNormalize (
        const CTNode*, unsigned);

    static void split (CTNode*, CTNode*, CTChilds&, CTChilds&, unsigned);

    CTNode*    root_;
    LogVars    logVars_;
    LogVarSet  logVarSet_;
};



inline const LogVars&
ConstraintTree::logVars() const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVars_;
}



inline const LogVarSet&
ConstraintTree::logVarSet() const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVarSet_;
}



inline size_t
ConstraintTree::nrLogVars() const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVars_.size();
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_

