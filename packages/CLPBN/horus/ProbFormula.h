#ifndef HORUS_PROBFORMULA_H
#define HORUS_PROBFORMULA_H

#include <limits>

#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"

typedef unsigned long PrvGroup;

class ProbFormula
{
  public:
    ProbFormula (Symbol f, const LogVars& lvs, unsigned range) 
        : functor_(f), logVars_(lvs), range_(range),
          countedLogVar_(), group_(numeric_limits<PrvGroup>::max()) { }

    ProbFormula (Symbol f, unsigned r) 
        : functor_(f), range_(r), group_(numeric_limits<PrvGroup>::max()) { }

    Symbol functor (void) const { return functor_; }

    unsigned arity (void) const { return logVars_.size(); }

    unsigned range (void) const { return range_; }

    LogVars& logVars (void) { return logVars_; }

    const LogVars& logVars (void) const { return logVars_; }

    LogVarSet logVarSet (void) const { return LogVarSet (logVars_); }
  
    PrvGroup group (void) const { return group_; }

    void setGroup (PrvGroup g) { group_ = g; }
   
    bool sameSkeletonAs (const ProbFormula&) const;

    bool contains (LogVar) const;

    bool contains (LogVarSet) const;

    size_t indexOf (LogVar) const;

    bool isAtom (void) const;

    bool isCounting (void) const;

    LogVar countedLogVar (void) const;
    
    void setCountedLogVar (LogVar);

    void clearCountedLogVar (void);
 
    void rename (LogVar, LogVar);
    
    static PrvGroup getNewGroup (void);

    friend std::ostream& operator<< (ostream &os, const ProbFormula& f);

    friend bool operator== (const ProbFormula& f1, const ProbFormula& f2);

  private: 
    Symbol     functor_;
    LogVars    logVars_;
    unsigned   range_;
    LogVar     countedLogVar_;
    PrvGroup   group_;
    static PrvGroup freeGroup_;
};

typedef vector<ProbFormula> ProbFormulas;


class ObservedFormula
{
  public:
    ObservedFormula (Symbol f, unsigned a, unsigned ev) 
        : functor_(f), arity_(a), evidence_(ev), constr_(a) { }

    ObservedFormula (Symbol f, unsigned ev, const Tuple& tuple) 
        : functor_(f), arity_(tuple.size()), evidence_(ev), constr_(arity_)
    {
      constr_.addTuple (tuple);
    }

    Symbol functor (void) const { return functor_; }

    unsigned arity (void) const { return arity_; }

    unsigned evidence (void) const  { return evidence_; }

    ConstraintTree& constr (void) { return constr_; }

    bool isAtom (void) const { return arity_ == 0; }

    void addTuple (const Tuple& tuple) { constr_.addTuple (tuple); }

    friend ostream& operator<< (ostream &os, const ObservedFormula& of);

  private:
    Symbol          functor_;
    unsigned        arity_;
    unsigned        evidence_;
    ConstraintTree  constr_;
};

typedef vector<ObservedFormula> ObservedFormulas;

#endif // HORUS_PROBFORMULA_H

