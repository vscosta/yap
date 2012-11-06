#ifndef HORUS_LIFTEDWCNF_H
#define HORUS_LIFTEDWCNF_H

#include "ParfactorList.h"

using namespace std;

typedef long LiteralId;

class ConstraintTree;


enum LogVarType
{
  FULL_LV,
  POS_LV,
  NEG_LV
};


typedef vector<LogVarType> LogVarTypes;

class Literal
{
  public:
    Literal (LiteralId lid, const LogVars& lvs) :
       lid_(lid), logVars_(lvs), negated_(false) { }

    Literal (const Literal& lit, bool negated) :
       lid_(lit.lid_), logVars_(lit.logVars_), negated_(negated) { }

    LiteralId lid (void) const { return lid_; }

    LogVars logVars (void) const { return logVars_; }

    LogVarSet logVarSet (void) const { return LogVarSet (logVars_); }

    void negate (void) { negated_ = !negated_; }

    bool isPositive (void) const { return negated_ == false; }

    bool isNegative (void) const { return negated_; }
    
    bool isGround (ConstraintTree constr, LogVarSet ipgLogVars) const;

    string toString (LogVarSet ipgLogVars = LogVarSet(),
      LogVarSet posCountedLvs = LogVarSet(),
      LogVarSet negCountedLvs = LogVarSet()) const;

    friend std::ostream& operator<< (ostream &os, const Literal& lit);

  private:
    LiteralId    lid_;
    LogVars      logVars_;
    bool         negated_;
};

typedef vector<Literal> Literals;



class Clause
{
  public:
    Clause (const ConstraintTree& ct) : constr_(ct) { }

    Clause (vector<vector<string>> names) : constr_(ConstraintTree (names)) { }

    void addLiteral (const Literal& l) { literals_.push_back (l); }

    void addLiteralNegated (const Literal& l)
    { 
      literals_.push_back (l);
      literals_.back().negate();
    }

    const vector<Literal>& literals (void) const { return literals_; }

    const ConstraintTree& constr (void) const { return constr_; }

    ConstraintTree constr (void) { return constr_; }

    bool isUnit (void) const { return literals_.size() == 1; }

    LogVarSet ipgLogVars (void) const { return ipgLogVars_; }

    void addIpgLogVar (LogVar X) { ipgLogVars_.insert (X); }

    void addPositiveCountedLogVar (LogVar X) { posCountedLvs_.insert (X); }

    void addNegativeCountedLogVar (LogVar X) { negCountedLvs_.insert (X); }

    LogVarSet positiveCountedLogVars (void) const { return posCountedLvs_; }

    LogVarSet negativeCountedLogVars (void) const { return negCountedLvs_; }

    unsigned nrPositiveCountedLogVars (void) const { return posCountedLvs_.size(); }

    unsigned nrNegativeCountedLogVars (void) const { return negCountedLvs_.size(); }

    bool containsLiteral (LiteralId lid) const;

    bool containsPositiveLiteral (LiteralId lid, const LogVarTypes&) const;

    bool containsNegativeLiteral (LiteralId lid, const LogVarTypes&) const;

    void removeLiterals (LiteralId lid);

    void removePositiveLiterals (LiteralId lid, const LogVarTypes&);

    void removeNegativeLiterals (LiteralId lid, const LogVarTypes&);

    bool isCountedLogVar (LogVar X) const;

    bool isPositiveCountedLogVar (LogVar X) const;

    bool isNegativeCountedLogVar (LogVar X) const;    
    
    bool isIpgLogVar (LogVar X) const;

    TinySet<LiteralId> lidSet (void) const;

    LogVarSet ipgCandidates (void) const;

    LogVarTypes logVarTypes (size_t litIdx) const;

    void removeLiteral (size_t litIdx);

    static void printClauses (const vector<Clause>& clauses);

    friend std::ostream& operator<< (ostream &os, const Clause& clause);

  private:
    LogVarSet getLogVarSetExcluding (size_t idx) const;

    vector<Literal>  literals_;
    LogVarSet        ipgLogVars_;
    LogVarSet        posCountedLvs_;
    LogVarSet        negCountedLvs_;
    ConstraintTree   constr_;
};

typedef vector<Clause> Clauses;



class LiteralLvTypes
{
  public:
    struct CompareLiteralLvTypes
    {
      bool operator() (
          const LiteralLvTypes& types1,
          const LiteralLvTypes& types2) const
      {
        if (types1.lid_ < types2.lid_) {
          return true;
        }
        return types1.lvTypes_ < types2.lvTypes_;
      }
    };
  
    LiteralLvTypes (LiteralId lid, const LogVarTypes& lvTypes) :
        lid_(lid), lvTypes_(lvTypes) { }
        
    LiteralId lid (void) const { return lid_; }
    
    const LogVarTypes& logVarTypes (void) const { return lvTypes_; }
    
    void setAllFullLogVars (void) {
        lvTypes_ = LogVarTypes (lvTypes_.size(), LogVarType::FULL_LV); }

  private:
    LiteralId    lid_;
    LogVarTypes  lvTypes_;
};

typedef TinySet<LiteralLvTypes,LiteralLvTypes::CompareLiteralLvTypes> LitLvTypesSet;



class LiftedWCNF
{
  public:
    LiftedWCNF (const ParfactorList& pfList);

   ~LiftedWCNF (void);
   
    const Clauses& clauses (void) const { return clauses_; }
   
    double posWeight (LiteralId lid) const
    {
      unordered_map<LiteralId, std::pair<double,double>>::const_iterator it;
      it = weights_.find (lid);
      return it != weights_.end() ? it->second.first : 1.0;
    }

    double negWeight (LiteralId lid) const
    {
      unordered_map<LiteralId, std::pair<double,double>>::const_iterator it;
      it = weights_.find (lid);
      return it != weights_.end() ? it->second.second : 1.0;
    }

    Clause createClauseForLiteral (LiteralId lid) const;

    void printFormulaIndicators (void) const;

    void printWeights (void) const;

    void printClauses (void) const;

  private:
  
    LiteralId getLiteralId (PrvGroup prvGroup, unsigned range)
    {
      assert (Util::contains (map_, prvGroup));
      return map_[prvGroup][range];
    }
  
    void addWeight (LiteralId lid, double posW, double negW)
    {
      weights_[lid] = make_pair (posW, negW);
    }

    void addIndicatorClauses (const ParfactorList& pfList);

    void addParameterClauses (const ParfactorList& pfList);

    Clauses clauses_;

    unordered_map<PrvGroup, vector<LiteralId>> map_;

    unordered_map<LiteralId, std::pair<double,double>> weights_;

    const ParfactorList& pfList_;

    LiteralId freeLiteralId_;
};

#endif // HORUS_LIFTEDWCNF_H

