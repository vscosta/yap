#ifndef YAP_PACKAGES_CLPBN_HORUS_LIFTEDWCNF_H_
#define YAP_PACKAGES_CLPBN_HORUS_LIFTEDWCNF_H_

#include <vector>
#include <unordered_map>
#include <string>
#include <ostream>

#include "ConstraintTree.h"
#include "ProbFormula.h"
#include "LiftedUtils.h"


namespace Horus {

class ParfactorList;

enum class LogVarType {
  fullLvt,
  posLvt,
  negLvt
};



// Workaround GCC bug #38064
inline bool operator< (LogVarType lvt1, LogVarType lvt2)
{
  return (int)lvt1 < (int)lvt2;
}



typedef long                     LiteralId;
typedef std::vector<LogVarType>  LogVarTypes;


class Literal {
  public:
    Literal (LiteralId lid, const LogVars& lvs)
        : lid_(lid), logVars_(lvs), negated_(false) { }

    Literal (const Literal& lit, bool negated)
        : lid_(lit.lid_), logVars_(lit.logVars_), negated_(negated) { }

    LiteralId lid() const { return lid_; }

    LogVars logVars() const { return logVars_; }

    size_t nrLogVars() const { return logVars_.size(); }

    LogVarSet logVarSet() const { return LogVarSet (logVars_); }

    void complement() { negated_ = !negated_; }

    bool isPositive() const { return negated_ == false; }

    bool isNegative() const { return negated_; }

    bool isGround (ConstraintTree constr, const LogVarSet& ipgLogVars) const;

    size_t indexOfLogVar (LogVar X) const;

    std::string toString (
        LogVarSet ipgLogVars = LogVarSet(),
        LogVarSet posCountedLvs = LogVarSet(),
        LogVarSet negCountedLvs = LogVarSet()) const;

  private:
    friend std::ostream& operator<< (std::ostream&, const Literal&);

    LiteralId  lid_;
    LogVars    logVars_;
    bool       negated_;
};

typedef std::vector<Literal> Literals;



class Clause {
  public:
    Clause (const ConstraintTree& ct = ConstraintTree({})) : constr_(ct) { }

    Clause (std::vector<std::vector<std::string>> names) :
        constr_(ConstraintTree (names)) { }

    void addLiteral (const Literal& l) { literals_.push_back (l); }

    const Literals& literals() const { return literals_; }

    Literals& literals() { return literals_; }

    size_t nrLiterals() const { return literals_.size(); }

    const ConstraintTree& constr() const { return constr_; }

    ConstraintTree constr() { return constr_; }

    bool isUnit() const { return literals_.size() == 1; }

    LogVarSet ipgLogVars() const { return ipgLvs_; }

    void addIpgLogVar (LogVar X) { ipgLvs_.insert (X); }

    void addPosCountedLogVar (LogVar X) { posCountedLvs_.insert (X); }

    void addNegCountedLogVar (LogVar X) { negCountedLvs_.insert (X); }

    LogVarSet posCountedLogVars() const { return posCountedLvs_; }

    LogVarSet negCountedLogVars() const { return negCountedLvs_; }

    unsigned nrPosCountedLogVars() const { return posCountedLvs_.size(); }

    unsigned nrNegCountedLogVars() const { return negCountedLvs_.size(); }

    void addLiteralComplemented (const Literal& lit);

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

    TinySet<LiteralId> lidSet() const;

    LogVarSet ipgCandidates() const;

    LogVarTypes logVarTypes (size_t litIdx) const;

    void removeLiteral (size_t litIdx);

    static bool independentClauses (Clause& c1, Clause& c2);

    static std::vector<Clause*> copyClauses (
        const std::vector<Clause*>& clauses);

    static void printClauses (const std::vector<Clause*>& clauses);

    static void deleteClauses (std::vector<Clause*>& clauses);

  private:
    LogVarSet getLogVarSetExcluding (size_t idx) const;

    friend std::ostream& operator<< (std::ostream&, const Clause&);

    Literals        literals_;
    LogVarSet       ipgLvs_;
    LogVarSet       posCountedLvs_;
    LogVarSet       negCountedLvs_;
    ConstraintTree  constr_;

    DISALLOW_ASSIGN (Clause);
};

typedef std::vector<Clause*> Clauses;



class LitLvTypes {
  public:
    LitLvTypes (LiteralId lid, const LogVarTypes& lvTypes) :
        lid_(lid), lvTypes_(lvTypes) { }

    LiteralId lid() const { return lid_; }

    const LogVarTypes& logVarTypes() const { return lvTypes_; }

    void setAllFullLogVars();

  private:
    friend std::ostream& operator<< (std::ostream&, const LitLvTypes&);

    LiteralId    lid_;
    LogVarTypes  lvTypes_;
};



struct CmpLitLvTypes
{
  bool operator() (
      const LitLvTypes& types1,
      const LitLvTypes& types2) const
  {
    if (types1.lid() < types2.lid()) {
      return true;
    }
    if (types1.lid() == types2.lid()){
     return types1.logVarTypes() < types2.logVarTypes();
    }
    return false;
  }
};

typedef TinySet<LitLvTypes, CmpLitLvTypes> LitLvTypesSet;



class LiftedWCNF {
  public:
    LiftedWCNF (const ParfactorList& pfList);

   ~LiftedWCNF();

    const Clauses& clauses() const { return clauses_; }

    void addWeight (LiteralId lid, double posW, double negW);

    double posWeight (LiteralId lid) const;

    double negWeight (LiteralId lid) const;

    std::vector<LiteralId> prvGroupLiterals (PrvGroup prvGroup);

    Clause* createClause (LiteralId lid) const;

    void printFormulaIndicators() const;

    void printWeights() const;

    void printClauses() const;

  private:
    LiteralId getLiteralId (PrvGroup prvGroup, unsigned range);

    void addIndicatorClauses (const ParfactorList& pfList);

    void addParameterClauses (const ParfactorList& pfList);

    Clauses                                                  clauses_;
    LiteralId                                                freeLiteralId_;
    const ParfactorList&                                     pfList_;
    std::unordered_map<PrvGroup, std::vector<LiteralId>>     map_;
    std::unordered_map<LiteralId, std::pair<double,double>>  weights_;

    DISALLOW_COPY_AND_ASSIGN (LiftedWCNF);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_LIFTEDWCNF_H_

