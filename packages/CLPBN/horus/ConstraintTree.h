#ifndef YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_
#define YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_

#include <cassert>

#include <vector>
#include <algorithm>
#include <string>
#include <ostream>

#include "TinySet.h"
#include "LiftedUtils.h"


namespace Horus {

class CTNode;
class ConstraintTree;


typedef std::vector<CTNode*>          CTNodes;
typedef std::vector<ConstraintTree*>  ConstraintTrees;


class CTNode {
  private:
    struct CmpSymbol {
      bool operator() (const CTNode* n1, const CTNode* n2) const {
        return n1->symbol() < n2->symbol();
    }};

  public:
    typedef TinySet<CTNode*, CmpSymbol> CTChilds;

    CTNode (const CTNode& n, const CTChilds& chs = CTChilds()) 
        : symbol_(n.symbol()), childs_(chs), level_(n.level()) { }

    CTNode (Symbol s, unsigned l, const CTChilds& chs = CTChilds())
        : symbol_(s), childs_(chs), level_(l) { }

    unsigned level (void) const { return level_; }

    void setLevel (unsigned level) { level_ = level; }

    Symbol symbol (void) const { return symbol_; }

    void setSymbol (Symbol s) { symbol_ = s; }

    CTChilds& childs (void) { return childs_; }

    const CTChilds& childs (void) const { return childs_; }

    size_t nrChilds (void) const { return childs_.size(); }

    bool isRoot (void) const { return level_ == 0; }

    bool isLeaf (void) const { return childs_.empty(); }

    CTChilds::iterator findSymbol (Symbol symb);

    void mergeSubtree (CTNode*, bool = true);

    void removeChild (CTNode*);

    void removeChilds (void);

    void removeAndDeleteChild (CTNode*);

    void removeAndDeleteAllChilds (void);

    SymbolSet childSymbols (void) const;

    static CTNode* copySubtree (const CTNode*);

    static void deleteSubtree (CTNode*);

  private:
    void updateChildLevels (CTNode*, unsigned);

    Symbol     symbol_;
    CTChilds   childs_;
    unsigned   level_;

    DISALLOW_ASSIGN (CTNode);
};


typedef CTNode::CTChilds CTChilds;


inline CTChilds::iterator
CTNode::findSymbol (Symbol symb)
{
  CTNode tmp (symb, 0);
  return childs_.find (&tmp);
}


std::ostream& operator<< (std::ostream&, const CTNode&);


class ConstraintTree {
  public:
    ConstraintTree (unsigned);

    ConstraintTree (const LogVars&);

    ConstraintTree (const LogVars&, const Tuples&);

    ConstraintTree (std::vector<std::vector<std::string>> names);

    ConstraintTree (const ConstraintTree&);

    ConstraintTree (const CTChilds& rootChilds, const LogVars& logVars);

   ~ConstraintTree (void);

    CTNode* root (void) const { return root_; }

    bool empty (void) const { return root_->childs().empty(); }

    const LogVars& logVars (void) const;

    const LogVarSet& logVarSet (void) const;

    size_t nrLogVars (void) const;

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

    LogVarSet singletons (void);

    TupleSet tupleSet (unsigned = 0) const;

    TupleSet tupleSet (const LogVars&);

    unsigned size (void) const;

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
ConstraintTree::logVars (void) const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVars_;
}



inline const LogVarSet&
ConstraintTree::logVarSet (void) const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVarSet_;
}



inline size_t
ConstraintTree::nrLogVars (void) const
{
  assert (LogVarSet (logVars_) == logVarSet_);
  return logVars_.size();
}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_CONSTRAINTTREE_H_

