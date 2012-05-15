#ifndef HORUS_PARFACTOR_H
#define HORUS_PARFACTOR_H

#include <list>
#include <unordered_map>

#include "ProbFormula.h"
#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"

#include "Factor.h"

class Parfactor : public TFactor<ProbFormula>
{
  public:
    Parfactor (
        const ProbFormulas&,
        const Params&,
        const Tuples&,
        unsigned);

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
  
    LogVarSet exclusiveLogVars (unsigned) const;
     
    void setConstraintTree (ConstraintTree*);

    void sumOut (unsigned fIdx);

    void multiply (Parfactor&);
    
    void countConvert (LogVar);

    void expand (LogVar, LogVar, LogVar);

    void fullExpand (LogVar);

    void reorderAccordingGrounds (const Grounds&);

    void absorveEvidence (const ProbFormula&, unsigned);

    void setNewGroups (void);

    void applySubstitution (const Substitution&);

    int findGroup (const Ground&) const;

    bool containsGround (const Ground&) const;

    bool containsGroup (unsigned) const;
  
    unsigned nrFormulas (LogVar) const;

    int indexOfLogVar (LogVar) const;

    int indexOfGroup (unsigned) const;

    vector<unsigned> getAllGroups (void) const;

    void print (bool = false) const;

    void printParameters (void) const;

    void printProjections (void) const;

    string getLabel (void) const;

  private:
    void expandPotential (int fIdx, unsigned newRange,
        const vector<unsigned>& sumIndexes);

    static void alignAndExponentiate (Parfactor*, Parfactor*);

    static void align (
        Parfactor*, const LogVars&, Parfactor*, const LogVars&);
   
    ConstraintTree*  constr_;
};


typedef vector<Parfactor*> Parfactors;

#endif // HORUS_PARFACTOR_H

