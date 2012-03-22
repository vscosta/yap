#ifndef HORUS_PROBFORMULA_H
#define HORUS_PROBFORMULA_H

#include <limits>

#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"


class ProbFormula
{
  public:
    ProbFormula (Symbol f, const LogVars& lvs, unsigned range) 
        : functor_(f), logVars_(lvs), range_(range),
          countedLogVar_() { }

    ProbFormula (Symbol f, unsigned r) : functor_(f), range_(r) { }

    Symbol functor (void) const { return functor_; }

    unsigned arity (void) const { return logVars_.size(); }

    unsigned range (void) const { return range_; }

    LogVars& logVars (void) { return logVars_; }

    const LogVars& logVars (void) const { return logVars_; }

    LogVarSet logVarSet (void) const { return LogVarSet (logVars_); }
  
    unsigned group (void) const { return groupId_; }

    void setGroup (unsigned g) { groupId_ = g; }
   
    bool sameSkeletonAs (const ProbFormula&) const;

    bool contains (LogVar) const;

    bool contains (LogVarSet) const;

    bool isAtom (void) const;

    bool isCounting (void) const;

    LogVar countedLogVar (void) const;
    
    void setCountedLogVar (LogVar);
 
    void rename (LogVar, LogVar);
    
    bool operator== (const ProbFormula& f) const;

    friend ostream& operator<< (ostream &out, const ProbFormula& f);

    static unsigned getNewGroup (void);

  private: 
    Symbol          functor_;
    LogVars         logVars_;
    unsigned        range_;
    LogVar          countedLogVar_;
    unsigned        groupId_;
    static int      freeGroup_;
};

typedef vector<ProbFormula> ProbFormulas;


#endif // HORUS_PROBFORMULA_H

