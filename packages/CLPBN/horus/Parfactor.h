#ifndef YAP_PACKAGES_CLPBN_HORUS_PARFACTOR_H_
#define YAP_PACKAGES_CLPBN_HORUS_PARFACTOR_H_

#include <vector>
#include <string>

#include "GenericFactor.h"
#include "ProbFormula.h"
#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"


namespace Horus {

class Parfactor : public GenericFactor<ProbFormula> {
  public:
    Parfactor (const ProbFormulas&, const Params&, const Tuples&,
        unsigned distId);

    Parfactor (const Parfactor*, const Tuple&);

    Parfactor (const Parfactor*, ConstraintTree*);

    Parfactor (const Parfactor&);

   ~Parfactor();

    ConstraintTree* constr() { return constr_; }

    const ConstraintTree* constr() const { return constr_; }

    const LogVars& logVars() const { return constr_->logVars(); }

    const LogVarSet& logVarSet() const { return constr_->logVarSet(); }

    LogVarSet countedLogVars() const;

    LogVarSet uncountedLogVars() const;

    LogVarSet elimLogVars() const;

    LogVarSet exclusiveLogVars (size_t fIdx) const;

    void sumOutIndex (size_t fIdx);

    void multiply (Parfactor&);

    bool canCountConvert (LogVar X);

    void countConvert (LogVar);

    void expand (LogVar, LogVar, LogVar);

    void fullExpand (LogVar);

    void reorderAccordingGrounds (const Grounds&);

    void absorveEvidence (const ProbFormula&, unsigned);

    void setNewGroups();

    void applySubstitution (const Substitution&);

    size_t indexOfGround (const Ground&) const;

    PrvGroup findGroup (const Ground&) const;

    bool containsGround (const Ground&) const;

    bool containsGrounds (const Grounds&) const;

    bool containsGroup (PrvGroup) const;

    bool containsGroups (std::vector<PrvGroup>) const;

    unsigned nrFormulas (LogVar) const;

    int indexOfLogVar (LogVar) const;

    int indexOfGroup (PrvGroup) const;

    unsigned nrFormulasWithGroup (PrvGroup) const;

    std::vector<PrvGroup> getAllGroups() const;

    void print (bool = false) const;

    void printParameters() const;

    void printProjections() const;

    std::string getLabel() const;

    void simplifyGrounds();

    static bool canMultiply (Parfactor*, Parfactor*);

  private:
    void simplifyCountingFormulas (size_t fIdx);

    void simplifyParfactor (size_t fIdx1, size_t fIdx2);

    static std::pair<LogVars, LogVars> getAlignLogVars (
        Parfactor* g1, Parfactor* g2);

    void expandPotential (size_t fIdx, unsigned newRange,
        const std::vector<unsigned>& sumIndexes);

    static void alignAndExponentiate (Parfactor*, Parfactor*);

    static void alignLogicalVars (Parfactor*, Parfactor*);

    ConstraintTree* constr_;

    DISALLOW_ASSIGN (Parfactor);
};

typedef std::vector<Parfactor*> Parfactors;

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_PARFACTOR_H_

