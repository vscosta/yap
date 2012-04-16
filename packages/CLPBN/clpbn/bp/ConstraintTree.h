#ifndef HORUS_CONSTRAINTTREE_H
#define HORUS_CONSTRAINTTREE_H

#include <cassert>
#include <algorithm>

#include <iostream>
#include <sstream>

#include "TinySet.h"
#include "LiftedUtils.h"

using namespace std;


class CTNode;
typedef vector<CTNode*> CTNodes;

class ConstraintTree;
typedef vector<ConstraintTree*> ConstraintTrees;



class CTNode
{
  public:
    CTNode (const CTNode& n) : symbol_(n.symbol()), level_(n.level()) { }

    CTNode (Symbol s, unsigned l) : symbol_(s) , level_(l) { }

    unsigned level (void) const { return level_; }

    void setLevel (unsigned level) { level_ = level; }

    Symbol symbol (void) const { return symbol_; }

    void setSymbol (const Symbol s) { symbol_ = s; }

    CTNodes& childs (void) { return childs_; }

    const CTNodes& childs (void) const { return childs_; }
 
    unsigned nrChilds (void) const { return childs_.size(); }

    bool isRoot (void) const { return level_ == 0; }

    bool isLeaf (void) const { return childs_.empty(); }

    void addChild (CTNode*, bool = true);

    void removeChild (CTNode*);

    void removeChilds (void);

    void removeAndDeleteChild (CTNode*);

    void removeAndDeleteAllChilds (void);

    SymbolSet childSymbols (void) const;

    static CTNode* copySubtree (const CTNode*);

    static void deleteSubtree (CTNode*);

  private:
    void updateChildLevels (CTNode*, unsigned);

    Symbol    symbol_;
    CTNodes   childs_;
    unsigned  level_;
};

ostream& operator<< (ostream &out, const CTNode&);


class ConstraintTree
{
  public:
    ConstraintTree (unsigned);

    ConstraintTree (const LogVars&);

    ConstraintTree (const LogVars&, const Tuples&);

    ConstraintTree (const ConstraintTree&);

   ~ConstraintTree (void);

    CTNode* root (void) const { return root_; }
    
    bool empty (void) const { return root_->childs().empty(); }

    const LogVars& logVars (void) const
    {
      assert (LogVarSet (logVars_) == logVarSet_);
      return logVars_;
    }

    const LogVarSet& logVarSet (void) const
    {
      assert (LogVarSet (logVars_) == logVarSet_);
      return logVarSet_;
    }
   
    unsigned nrLogVars (void) const
    {
      return logVars_.size();
      assert (LogVarSet (logVars_) == logVarSet_);
    }
  
    void addTuple (const Tuple&);
  
    bool containsTuple (const Tuple&);
  
    void moveToTop (const LogVars&);

    void moveToBottom (const LogVars&);

    void join (ConstraintTree*, bool = false);

    unsigned getLevel (LogVar) const;

    void rename (LogVar, LogVar);

    void applySubstitution (const Substitution&);

    void project (const LogVarSet&);

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

    bool isCarteesianProduct (const LogVarSet&) const;

    std::pair<ConstraintTree*, ConstraintTree*> split (
        const Tuple&, unsigned);

    std::pair<ConstraintTree*, ConstraintTree*> split (
        const ConstraintTree*, unsigned) const;

    ConstraintTrees countNormalize (const LogVarSet&);

    ConstraintTrees jointCountNormalize (
        ConstraintTree*, ConstraintTree*, LogVar, LogVar, LogVar);

    static bool identical (
        const ConstraintTree*, const ConstraintTree*, unsigned);

    static bool overlap (
        const ConstraintTree*, const ConstraintTree*, unsigned);

    LogVars expand (LogVar);
    ConstraintTrees ground (LogVar);
   
  private:
    unsigned countTuples (const CTNode*) const;

    CTNodes getNodesBelow (CTNode*) const;

    CTNodes getNodesAtLevel (unsigned) const;

    void swapLogVar (LogVar);

    bool join (CTNode*, const Tuple&, unsigned, CTNode*);

    bool indenticalSubtrees (
        const CTNode*, const CTNode*, bool) const;

    void getTuples (CTNode*, Tuples, unsigned, Tuples&, CTNodes&) const;

    vector<std::pair<CTNode*, unsigned>> countNormalize (
        const CTNode*, unsigned);

    static void split (
        CTNode*, CTNode*, CTNodes&, unsigned);

    static bool overlap (const CTNode*, const CTNode*, unsigned);

    CTNode*    root_;
    LogVars    logVars_;
    LogVarSet  logVarSet_;
};


#endif // HORUS_CONSTRAINTTREE_H

