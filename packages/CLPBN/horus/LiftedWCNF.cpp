#include "LiftedWCNF.h"
#include "ConstraintTree.h"
#include "Indexer.h"



bool
Literal::isGround (ConstraintTree constr, LogVarSet ipgLogVars) const
{
  if (logVars_.size() == 0) {
    return true;
  }
  LogVarSet lvs (logVars_);
  lvs -= ipgLogVars;
  return constr.singletons().contains (lvs);
}



size_t
Literal::indexOfLogVar (LogVar X) const
{
  return Util::indexOf (logVars_, X);
}



string
Literal::toString (
    LogVarSet ipgLogVars,
    LogVarSet posCountedLvs,
    LogVarSet negCountedLvs) const
{
  stringstream ss;
  negated_ ? ss << "¬" : ss << "" ;
  // if (negated_ == false) {
  //   posWeight_ < 0.0 ? ss << "λ" : ss << "Θ" ;
  // } else {
  //   negWeight_ < 0.0 ? ss << "λ" : ss << "Θ" ;  
  // }
  ss << "λ" ;
  ss << lid_ ;
  if (logVars_.empty() == false) {
    ss << "(" ;
    for (size_t i = 0; i < logVars_.size(); i++) {
      if (i != 0) ss << ",";
      if (posCountedLvs.contains (logVars_[i])) {
        ss << "+" << logVars_[i];
      } else if (negCountedLvs.contains (logVars_[i])) {
        ss << "-" << logVars_[i];
      } else if (ipgLogVars.contains (logVars_[i])) {
        LogVar X = logVars_[i];
        const string labels[] = {
            "a", "b", "c", "d", "e", "f",
            "g", "h", "i", "j", "k", "m" };
        (X >= 12) ? ss << "x_" << X : ss << labels[X];
      } else {
        ss << logVars_[i];
      }
    }
    ss << ")" ;
  }
  return ss.str();
}



std::ostream& operator<< (ostream &os, const Literal& lit)
{
  os << lit.toString();
  return os;
}



void
Clause::addLiteralComplemented (const Literal& lit)
{
  assert (constr_.logVarSet().contains (lit.logVars()));
  literals_.push_back (lit);
  literals_.back().complement();
}



bool
Clause::containsLiteral (LiteralId lid) const
{
  for (size_t i = 0; i < literals_.size(); i++) {
    if (literals_[i].lid() == lid) {
      return true;
    }
  }
  return false;
}



bool
Clause::containsPositiveLiteral (
    LiteralId lid,
    const LogVarTypes& types) const
{
  for (size_t i = 0; i < literals_.size(); i++) {
    if (literals_[i].lid() == lid
        && literals_[i].isPositive()
        && logVarTypes (i) == types) {
      return true;
    }
  }
  return false;
}


    
bool
Clause::containsNegativeLiteral (
    LiteralId lid,
    const LogVarTypes& types) const
{
  for (size_t i = 0; i < literals_.size(); i++) {
    if (literals_[i].lid() == lid
        && literals_[i].isNegative()
        && logVarTypes (i) == types) {
      return true;
    }
  }
  return false;
}



void
Clause::removeLiterals (LiteralId lid)
{
  size_t i = 0;
  while (i != literals_.size()) {
    if (literals_[i].lid() == lid) {
      removeLiteral (i);
    } else {
      i ++;
    }
  }
}



void
Clause::removePositiveLiterals (
   LiteralId lid,
   const LogVarTypes& types)
{
  size_t i = 0;
  while (i != literals_.size()) {
    if (literals_[i].lid() == lid
        && literals_[i].isPositive()
        && logVarTypes (i) == types) {
      removeLiteral (i);
    } else {
      i ++;
    }
  }
}



void
Clause::removeNegativeLiterals (
    LiteralId lid,
    const LogVarTypes& types)
{
  size_t i = 0;
  while (i != literals_.size()) {
    if (literals_[i].lid() == lid
        && literals_[i].isNegative()
        && logVarTypes (i) == types) {
      removeLiteral (i);
    } else {
      i ++;
    }
  }
}



bool
Clause::isCountedLogVar (LogVar X) const
{
  assert (constr_.logVarSet().contains (X));
  return posCountedLvs_.contains (X)
      || negCountedLvs_.contains (X);
}



bool
Clause::isPositiveCountedLogVar (LogVar X) const
{
  assert (constr_.logVarSet().contains (X));
  return posCountedLvs_.contains (X);
}
  


bool
Clause::isNegativeCountedLogVar (LogVar X) const
{
  assert (constr_.logVarSet().contains (X));
  return negCountedLvs_.contains (X);
}



bool
Clause::isIpgLogVar (LogVar X) const
{
   assert (constr_.logVarSet().contains (X));
   return ipgLvs_.contains (X);
}



TinySet<LiteralId>
Clause::lidSet (void) const
{
  TinySet<LiteralId> lidSet;
  for (size_t i = 0; i < literals_.size(); i++) {
    lidSet.insert (literals_[i].lid());
  }
  return lidSet;
}



LogVarSet
Clause::ipgCandidates (void) const
{
  LogVarSet candidates;
  LogVarSet allLvs = constr_.logVarSet();
  allLvs -= ipgLvs_;
  allLvs -= posCountedLvs_;
  allLvs -= negCountedLvs_;  
  for (size_t i = 0; i < allLvs.size(); i++) {
    bool valid = true;
    for (size_t j = 0; j < literals_.size(); j++) {
      if (Util::contains (literals_[j].logVars(), allLvs[i]) == false) {
        valid = false;
        break;
      }
    }
    if (valid) {
      candidates.insert (allLvs[i]);
    }
  }
  return candidates;
}



LogVarTypes
Clause::logVarTypes (size_t litIdx) const
{
  LogVarTypes types;
  const LogVars lvs = literals_[litIdx].logVars();
  for (size_t i = 0; i < lvs.size(); i++) {
    if (posCountedLvs_.contains (lvs[i])) {
      types.push_back (LogVarType::POS_LV);
    } else if (negCountedLvs_.contains (lvs[i])) {
      types.push_back (LogVarType::NEG_LV);    
    } else {
      types.push_back (LogVarType::FULL_LV);
    }
  }
  return types;
}



void
Clause::removeLiteral (size_t litIdx)
{
  LogVarSet lvsToRemove = literals_[litIdx].logVarSet()
      - getLogVarSetExcluding (litIdx);
  ipgLvs_    -= lvsToRemove;
  posCountedLvs_ -= lvsToRemove;
  negCountedLvs_ -= lvsToRemove;
  constr_.remove (lvsToRemove);
  literals_.erase (literals_.begin() + litIdx);
}



bool
Clause::independentClauses (Clause& c1, Clause& c2)
{
  const Literals& lits1 = c1.literals();
  const Literals& lits2 = c2.literals();
  for (size_t i = 0; i < lits1.size(); i++) {
    for (size_t j = 0; j < lits2.size(); j++) {
      if (lits1[i].lid() == lits2[j].lid()
          && c1.logVarTypes (i) == c2.logVarTypes (j)) {
        return false;
      }
    }
  }
  return true;
}



Clauses
Clause::copyClauses (const Clauses& clauses)
{
  Clauses copy;
  copy.reserve (clauses.size());
  for (size_t i = 0; i < clauses.size(); i++) {
    copy.push_back (new Clause (*clauses[i]));
  }
  return copy;
}



void
Clause::printClauses (const Clauses& clauses)
{
  for (size_t i = 0; i < clauses.size(); i++) {
    cout << *clauses[i] << endl;
  }
}



std::ostream& operator<< (ostream &os, const Clause& clause)
{
  for (unsigned i = 0; i < clause.literals_.size(); i++) {
    if (i != 0) os << " v " ;
    os << clause.literals_[i].toString (clause.ipgLvs_,
        clause.posCountedLvs_, clause.negCountedLvs_);
  }
  if (clause.constr_.empty() == false) {
    ConstraintTree copy (clause.constr_);
    copy.moveToTop (copy.logVarSet().elements());
    os << " | " << copy.tupleSet();
  }
  return os;
}



LogVarSet
Clause::getLogVarSetExcluding (size_t idx) const
{
  LogVarSet lvs;
  for (size_t i = 0; i < literals_.size(); i++) {
    if (i != idx) {
      lvs |= literals_[i].logVars();
    }
  }
  return lvs;
}



LiftedWCNF::LiftedWCNF (const ParfactorList& pfList)
    : freeLiteralId_(0), pfList_(pfList)
{
  addIndicatorClauses (pfList);
  addParameterClauses (pfList);

  /*
  // INCLUSION-EXCLUSION TEST
  vector<vector<string>> names = {
    // {"a1","b1"},{"a2","b2"},{"a1","b3"}
    {"b1","a1"},{"b2","a2"},{"b3","a1"}
  };
  Clause c1 (names);
  c1.addLiteral (Literal (0, LogVars() = {0}));
  c1.addLiteral (Literal (1, LogVars() = {1}));
  clauses_.push_back(c1);
  freeLiteralId_ ++ ;
  freeLiteralId_ ++ ;
  */
  
  /*
  // ATOM-COUNTING TEST
  vector<vector<string>> names = {
    {"p1","p1"},{"p1","p2"},{"p1","p3"},      
    {"p2","p1"},{"p2","p2"},{"p2","p3"},
    {"p3","p1"},{"p3","p2"},{"p3","p3"}
  };
  Clause c1 (names);
  c1.addLiteral (Literal (0, LogVars() = {0}));
  c1.addLiteralComplemented (Literal (1, {0,1}));
  clauses_.push_back(c1);
  Clause c2 (names);
  c2.addLiteral (Literal (0, LogVars()={0}));
  c2.addLiteralComplemented (Literal (1, {1,0}));
  clauses_.push_back(c2);
  addWeight (0, LogAware::log(3.0), LogAware::log(4.0));
  addWeight (1, LogAware::log(2.0), LogAware::log(5.0));
  freeLiteralId_ = 2;
  */

  if (Globals::verbosity > 1) {
    cout << "FORMULA INDICATORS:" << endl;
    printFormulaIndicators();
    cout << endl;
    cout << "WEIGHTED INDICATORS:" << endl;
    printWeights();
    cout << endl;
    cout << "CLAUSES:" << endl;
    printClauses();
    cout << endl;
  }
}



LiftedWCNF::~LiftedWCNF (void)
{

}



void
LiftedWCNF::addWeight (LiteralId lid, double posW, double negW)
{
  weights_[lid] = make_pair (posW, negW);
}



double
LiftedWCNF::posWeight (LiteralId lid) const
{
  unordered_map<LiteralId, std::pair<double,double>>::const_iterator it;
  it = weights_.find (lid);
  return it != weights_.end() ? it->second.first : LogAware::one();
}



double
LiftedWCNF::negWeight (LiteralId lid) const
{
  unordered_map<LiteralId, std::pair<double,double>>::const_iterator it;
  it = weights_.find (lid);
  return it != weights_.end() ? it->second.second : LogAware::one();
}



vector<LiteralId>
LiftedWCNF::prvGroupLiterals (PrvGroup prvGroup)
{
  assert (Util::contains (map_, prvGroup));
  return map_[prvGroup];
}



Clause*
LiftedWCNF::createClause (LiteralId lid) const
{
  for (size_t i = 0; i < clauses_.size(); i++) {
    const Literals& literals = clauses_[i]->literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].lid() == lid) {
        // TODO projectedCopy ?
        ConstraintTree ct = clauses_[i]->constr();
        ct.project (literals[j].logVars());
        Clause* c = new Clause (ct);
        c->addLiteral (literals[j]);
        return c;
      }
    }
  }
  abort(); // we should not reach this point
  return 0;
}



LiteralId
LiftedWCNF::getLiteralId (PrvGroup prvGroup, unsigned range)
{
  assert (Util::contains (map_, prvGroup));
  return map_[prvGroup][range];
}
  


void
LiftedWCNF::addIndicatorClauses (const ParfactorList& pfList)
{
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      if (Util::contains (map_, formulas[i].group()) == false) {
        ConstraintTree tempConstr = *(*it)->constr();
        tempConstr.project (formulas[i].logVars());
        Clause* clause = new Clause (tempConstr);
        vector<LiteralId> lids;
        for (size_t j = 0; j < formulas[i].range(); j++) {
          clause->addLiteral (Literal (freeLiteralId_, formulas[i].logVars()));
          lids.push_back (freeLiteralId_);
          freeLiteralId_ ++;
        }
        clauses_.push_back (clause);
        for (size_t j = 0; j < formulas[i].range() - 1; j++) {
          for (size_t k = j + 1; k < formulas[i].range(); k++) {
            ConstraintTree tempConstr2 = *(*it)->constr();
            tempConstr2.project (formulas[i].logVars());
            Clause* clause2 = new Clause (tempConstr2);
            clause2->addLiteralComplemented (Literal (clause->literals()[j]));
            clause2->addLiteralComplemented (Literal (clause->literals()[k]));
            clauses_.push_back (clause2);
          }
        }
        map_[formulas[i].group()] = lids;
      }
    }
    ++ it;
  }
}



void
LiftedWCNF::addParameterClauses (const ParfactorList& pfList)
{
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    Indexer indexer ((*it)->ranges());
    vector<PrvGroup> groups = (*it)->getAllGroups();
    while (indexer.valid()) {
      LiteralId paramVarLid = freeLiteralId_;
      // λu1 ∧ ... ∧ λun ∧ λxi <=> θxi|u1,...,un
      //
      // ¬λu1 ... ¬λun v θxi|u1,...,un  -> clause1
      // ¬θxi|u1,...,un v λu1           -> tempClause
      // ¬θxi|u1,...,un v λu2           -> tempClause
      double posWeight = (**it)[indexer];
      addWeight (paramVarLid, posWeight, 1.0);
      
      Clause* clause1 = new Clause (*(*it)->constr());

      for (unsigned i = 0; i < groups.size(); i++) {
        LiteralId lid = getLiteralId (groups[i], indexer[i]);

        clause1->addLiteralComplemented (
            Literal (lid, (*it)->argument(i).logVars()));

        ConstraintTree ct = *(*it)->constr();
        Clause* tempClause = new Clause (ct);
        tempClause->addLiteralComplemented (Literal (
            paramVarLid, (*it)->constr()->logVars()));
        tempClause->addLiteral (Literal (lid, (*it)->argument(i).logVars()));
        clauses_.push_back (tempClause);        
      }
      clause1->addLiteral (Literal (paramVarLid, (*it)->constr()->logVars()));
      clauses_.push_back (clause1);
      freeLiteralId_ ++;
      ++ indexer;
    }
    ++ it;
  }
}


void
LiftedWCNF::printFormulaIndicators (void) const
{
  set<PrvGroup> allGroups;
  ParfactorList::const_iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      if (Util::contains (allGroups, formulas[i].group()) == false) {
        allGroups.insert (formulas[i].group());
        cout << formulas[i] << " | " ;
        ConstraintTree tempCt = *(*it)->constr();
        tempCt.project (formulas[i].logVars());
        cout << tempCt.tupleSet();
        cout << " indicators => " ;
        vector<LiteralId> indicators = 
            (map_.find (formulas[i].group()))->second;
        cout << indicators << endl;
      }
    }
    ++ it;
  }
}



void
LiftedWCNF::printWeights (void) const
{
  unordered_map<LiteralId, std::pair<double,double>>::const_iterator it;
  it = weights_.begin();
  while (it != weights_.end()) {
    cout << "λ" << it->first << " weights: " ;     
    cout << it->second.first << " " << it->second.second;
    cout << endl;
    ++ it;
  }
}



void
LiftedWCNF::printClauses (void) const
{
  Clause::printClauses (clauses_);
}

