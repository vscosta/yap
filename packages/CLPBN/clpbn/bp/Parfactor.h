#ifndef HORUS_PARFACTOR_H
#define HORUS_PARFACTOR_H

#include <list>
#include <unordered_map>

#include "ProbFormula.h"
#include "ConstraintTree.h"
#include "LiftedUtils.h"
#include "Horus.h"


class Parfactor
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

    ProbFormulas& formulas (void) { return formulas_; }

    const ProbFormulas& formulas (void) const { return formulas_; }
 
    unsigned nrFormulas (void) const { return formulas_.size(); }

    Params& params (void) { return params_; }

    const Params& params (void) const { return params_; }

    unsigned size (void) const { return params_.size(); }

    const Ranges& ranges (void) const { return ranges_; }
  
    unsigned distId (void) const { return distId_; }

    ConstraintTree* constr (void) { return constr_; }

    const ConstraintTree* constr (void) const { return constr_; }

    const LogVars& logVars (void) const { return constr_->logVars(); }
 
    const LogVarSet& logVarSet (void) const { return constr_->logVarSet(); }

    LogVarSet countedLogVars (void) const;

    LogVarSet uncountedLogVars (void) const;
   
    LogVarSet elimLogVars (void) const;
  
    LogVarSet exclusiveLogVars (unsigned) const;
     
    void setConstraintTree (ConstraintTree*);

    void sumOut (unsigned);

    void multiply (Parfactor&);
    
    void countConvert (LogVar);

    void expandPotential (LogVar, LogVar, LogVar);

    void fullExpand (LogVar);

    void reorderAccordingGrounds (const Grounds&);

    void reorderFormulas (const ProbFormulas&);

    void absorveEvidence (unsigned, unsigned);

    void normalize (void);

    void setFormulaGroup (const ProbFormula&, int);

    void setNewGroups (void);

    void applySubstitution (const Substitution&);

    bool containsGround (const Ground&) const;

    bool containsGroup (unsigned) const;
   
    const ProbFormula& formula (unsigned) const;
   
    unsigned range (unsigned) const;

    unsigned nrFormulas (LogVar) const;

    int indexOf (const ProbFormula&) const;

    int indexOfFormulaWithLogVar (LogVar) const;

    int indexOfFormulaWithGroup (unsigned) const;

    vector<unsigned> getAllGroups (void) const;

    void print (bool = false) const;

    string getHeaderString (void) const;

  private:
    static void alignAndExponentiate (Parfactor*, Parfactor*);
    static void align (
        Parfactor*, const LogVars&, Parfactor*, const LogVars&);

    void insertDimension (unsigned);
   
    ProbFormulas       formulas_;
    Ranges             ranges_;
    Params             params_;
    unsigned           distId_;
    ConstraintTree*    constr_;
};


typedef vector<Parfactor*> Parfactors;

#endif // HORUS_PARFACTOR_H

