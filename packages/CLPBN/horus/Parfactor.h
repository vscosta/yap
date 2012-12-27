#ifndef HORUS_PARFACTOR_H
#define HORUS_PARFACTOR_H

#include "Factor.h"
#include "ProbFormula.h"
#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"


class Parfactor : public TFactor<ProbFormula>
{
  public:
    Parfactor (
        const ProbFormulas&,
        const Params&,
        const Tuples&,
        unsigned distId);

    Parfactor (const Parfactor*, const Tuple&);

    Parfactor (const Parfactor*, ConstraintTree*);

    Parfactor (const Parfactor&);

   ~Parfactor (void);

    ConstraintTree* constr (void) { return constr_; }

    const ConstraintTree* constr (void) const { return constr_; }

    const LogVars& logVars (void) const { return constr_->logVars(); }

    const LogVarSet& logVarSet (void) const { return constr_->logVarSet(); }

    LogVarSet countedLogVars (void) const;

    LogVarSet uncountedLogVars (void) const;

    LogVarSet elimLogVars (void) const;

    LogVarSet exclusiveLogVars (size_t fIdx) const;

    void sumOutIndex (size_t fIdx);

    void multiply (Parfactor&);

    bool canCountConvert (LogVar X);

    void countConvert (LogVar);

    void expand (LogVar, LogVar, LogVar);

    void fullExpand (LogVar);

    void reorderAccordingGrounds (const Grounds&);

    void absorveEvidence (const ProbFormula&, unsigned);

    void setNewGroups (void);

    void applySubstitution (const Substitution&);

    size_t indexOfGround (const Ground&) const;

    PrvGroup findGroup (const Ground&) const;

    bool containsGround (const Ground&) const;

    bool containsGrounds (const Grounds&) const;

    bool containsGroup (PrvGroup) const;

    bool containsGroups (vector<PrvGroup>) const;

    unsigned nrFormulas (LogVar) const;

    int indexOfLogVar (LogVar) const;

    int indexOfGroup (PrvGroup) const;

    unsigned nrFormulasWithGroup (PrvGroup) const;

    vector<PrvGroup> getAllGroups (void) const;

    void print (bool = false) const;

    void printParameters (void) const;

    void printProjections (void) const;

    string getLabel (void) const;

    void simplifyGrounds (void);

    static bool canMultiply (Parfactor*, Parfactor*);

  private:
    void simplifyCountingFormulas (size_t fIdx);

    void simplifyParfactor (size_t fIdx1, size_t fIdx2);

    static std::pair<LogVars, LogVars> getAlignLogVars (
        Parfactor* g1, Parfactor* g2);

    void expandPotential (size_t fIdx, unsigned newRange,
        const vector<unsigned>& sumIndexes);

    static void alignAndExponentiate (Parfactor*, Parfactor*);

    static void alignLogicalVars (Parfactor*, Parfactor*);

    ConstraintTree*  constr_;

    DISALLOW_ASSIGN (Parfactor);
};

typedef vector<Parfactor*> Parfactors;

#endif // HORUS_PARFACTOR_H

