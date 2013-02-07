#ifndef YAP_PACKAGES_CLPBN_HORUS_PROBFORMULA_H_
#define YAP_PACKAGES_CLPBN_HORUS_PROBFORMULA_H_

#include <vector>
#include <ostream>
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
          countedLogVar_(), group_(std::numeric_limits<PrvGroup>::max()) { }

    ProbFormula (Symbol f, unsigned r)
        : functor_(f), range_(r),
          group_(std::numeric_limits<PrvGroup>::max()) { }

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

  private:
    friend bool operator== (
        const ProbFormula& f1, const ProbFormula& f2);

    friend std::ostream& operator<< (
        std::ostream&, const ProbFormula&);

    Symbol     functor_;
    LogVars    logVars_;
    unsigned   range_;
    LogVar     countedLogVar_;
    PrvGroup   group_;
    static PrvGroup freeGroup_;
};

typedef std::vector<ProbFormula> ProbFormulas;


class ObservedFormula
{
  public:
    ObservedFormula (Symbol f, unsigned a, unsigned ev);

    ObservedFormula (Symbol f, unsigned ev, const Tuple& tuple);

    Symbol functor (void) const { return functor_; }

    unsigned arity (void) const { return arity_; }

    unsigned evidence (void) const  { return evidence_; }

    void setEvidence (unsigned ev) { evidence_ = ev; }

    ConstraintTree& constr (void) { return constr_; }

    bool isAtom (void) const { return arity_ == 0; }

    void addTuple (const Tuple& tuple) { constr_.addTuple (tuple); }

  private:
    friend std::ostream& operator<< (
        std::ostream&, const ObservedFormula&);

    Symbol          functor_;
    unsigned        arity_;
    unsigned        evidence_;
    ConstraintTree  constr_;
};

typedef std::vector<ObservedFormula> ObservedFormulas;

#endif // YAP_PACKAGES_CLPBN_HORUS_PROBFORMULA_H_

