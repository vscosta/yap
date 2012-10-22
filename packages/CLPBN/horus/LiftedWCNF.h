#ifndef HORUS_LIFTEDWCNF_H
#define HORUS_LIFTEDWCNF_H

#include "ParfactorList.h"

using namespace std;

typedef long LiteralId;

class ConstraintTree;


class Literal
{
  public:
    Literal (LiteralId lid, const LogVars& lvs, double w = -1.0) :
        lid_(lid), logVars_(lvs), weight_(w), negated_(false) { }
        
    Literal (const Literal& lit, bool negated) :
       lid_(lit.lid_), logVars_(lit.logVars_), weight_(lit.weight_), negated_(negated) { }

    LiteralId lid (void) const { return lid_; }

    double weight (void) const { return weight_; }
    
    void negate (void) { negated_ = !negated_; }

    bool isPositive (void) const { return negated_ == false; }
    
    bool isNegative (void) const { return negated_; }
    
    bool isGround (ConstraintTree* constr) const;
    
    friend std::ostream& operator<< (ostream &os, const Literal& lit);
        
  private:
    LiteralId    lid_;
    LogVars      logVars_;
    double       weight_;
    bool         negated_;
};

typedef vector<Literal> Literals;


class Clause
{
  public:
    Clause (ConstraintTree* ct) : ct_(ct) { }
    
    void addLiteral (const Literal& l) { literals_.push_back (l); }
    
    void addAndNegateLiteral (const Literal& l)
    { 
      literals_.push_back (l);
      literals_.back().negate();
    }
    
    bool containsLiteral (LiteralId lid) const;

    bool containsPositiveLiteral (LiteralId lid) const;
    
    bool containsNegativeLiteral (LiteralId lid) const;
    
    void removeLiterals (LiteralId lid);
    
    void removePositiveLiterals (LiteralId lid);
    
    void removeNegativeLiterals (LiteralId lid);

    const vector<Literal>& literals (void) const { return literals_; }
    
    void removeLiteralByIndex (size_t idx) { literals_.erase (literals_.begin() + idx); }
    
    ConstraintTree* constr (void) const { return ct_; }
    
    ConstraintTree* constr (void) { return ct_; }
    
    bool isUnit (void) const { return literals_.size() == 1; }
    
    TinySet<LiteralId> lidSet (void) const;
    
    friend std::ostream& operator<< (ostream &os, const Clause& clause);
    
  private:
    vector<Literal>  literals_;
    ConstraintTree*  ct_;
};



typedef vector<Clause> Clauses;



class LiftedWCNF
{
  public:
    LiftedWCNF (const ParfactorList& pfList);
    
   ~LiftedWCNF (void);
   
   const Clauses& clauses (void) const { return clauses_; }
   
   void printClauses (void) const;
   
   void printFormulasToIndicators (void) const;
    
  private:
    void addIndicatorClauses (const ParfactorList& pfList);

    void addParameterClauses (const ParfactorList& pfList);

    LiteralId getLiteralId (PrvGroup prvGroup, unsigned range)
    {
      assert (Util::contains (map_, prvGroup));
      return map_[prvGroup][range];
    }

    Clauses clauses_;
    
    unordered_map<PrvGroup,vector<LiteralId>> map_;
    
    const ParfactorList& pfList_;
    
    LiteralId freeLiteralId_;
};

#endif // HORUS_LIFTEDWCNF_H
