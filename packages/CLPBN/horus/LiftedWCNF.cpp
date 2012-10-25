#include "LiftedWCNF.h"
#include "ConstraintTree.h"
#include "Indexer.h"


bool
Literal::isGround (ConstraintTree* constr) const
{
  if (logVars_.size() == 0) {
    return true;
  }
  LogVarSet singletons = constr->singletons();
  return singletons.contains (logVars_);
}



std::ostream& operator<< (ostream &os, const Literal& lit)
{
  lit.negated_ ? os << "¬" : os << "" ;
  lit.weight_ < 0.0 ? os << "λ" : os << "Θ" ;
  os << lit.lid_ ;
  if (lit.logVars_.empty() == false) {
    os << "(" ;
    for (size_t i = 0; i < lit.logVars_.size(); i++) {
      if (i != 0) os << ",";
      os << lit.logVars_[i];
    }
    os << ")" ;
  }
  return os;
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
Clause::containsPositiveLiteral (LiteralId lid) const
{
  for (size_t i = 0; i < literals_.size(); i++) {
    if (literals_[i].lid() == lid && literals_[i].isPositive()) {
      return true;
    }
  }
  return false;
}


    
bool
Clause::containsNegativeLiteral (LiteralId lid) const
{
  for (size_t i = 0; i < literals_.size(); i++) {
    if (literals_[i].lid() == lid && literals_[i].isNegative()) {
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
      literals_.erase (literals_.begin() + i);
    } else {
      i ++;
    }
  }
}

    

void
Clause::removePositiveLiterals (LiteralId lid)
{
  size_t i = 0;
  while (i != literals_.size()) {
    if (literals_[i].lid() == lid && literals_[i].isPositive()) {
      literals_.erase (literals_.begin() + i);
    } else {
      i ++;
    }
  }
}
    


void
Clause::removeNegativeLiterals (LiteralId lid)
{
  size_t i = 0;
  while (i != literals_.size()) {
    if (literals_[i].lid() == lid && literals_[i].isNegative()) {
      literals_.erase (literals_.begin() + i);
    } else {
      i ++;
    }
  }
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



std::ostream& operator<< (ostream &os, const Clause& clause)
{
  for (unsigned i = 0; i < clause.literals_.size(); i++) {
    if (i != 0) os << " v " ;
    os << clause.literals_[i];
  }
  if (clause.ct_->empty() == false) {
    ConstraintTree copy (*clause.ct_);
    copy.moveToTop (copy.logVarSet().elements());
    os << " | " << copy.tupleSet();
  }
  return os;
}



LiftedWCNF::LiftedWCNF (const ParfactorList& pfList)
    : pfList_(pfList), freeLiteralId_(0)
{
  addIndicatorClauses (pfList);
  addParameterClauses (pfList);
  cout << "FORMULA INDICATORS:" << endl;
  printFormulaIndicators();
  cout << endl;
  cout << "WEIGHTS:" << endl;
  printWeights();
  cout << endl;
  cout << "CLAUSES:" << endl;
  printClauses();
  cout << endl;
}



LiftedWCNF::~LiftedWCNF (void)
{

}



Clause
LiftedWCNF::createClauseForLiteral (LiteralId lid) const
{
  for (size_t i = 0; i < clauses_.size(); i++) {
    const Literals& literals = clauses_[i].literals();
    for (size_t j = 0; j < literals.size(); j++) {
      if (literals[j].lid() == lid) {
        ConstraintTree* ct = new ConstraintTree (*clauses_[i].constr());
        ct->project (literals[j].logVars());
        Clause clause (ct);
        clause.addLiteral (literals[j]);
        return clause;
      }
    }
  }
  // FIXME
  Clause c (new ConstraintTree({}));
  c.addLiteral (Literal (lid,{}));
  return c;
  //assert (false);
  //return Clause (0);
}



void
LiftedWCNF::addIndicatorClauses (const ParfactorList& pfList)
{
  ParfactorList::const_iterator it = pfList.begin();
  set<PrvGroup> allGroups;
  while (it != pfList.end()) {
    const ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      if (Util::contains (allGroups, formulas[i].group()) == false) {
        allGroups.insert (formulas[i].group());
        ConstraintTree* tempConstr = new ConstraintTree (*(*it)->constr());
        tempConstr->project (formulas[i].logVars());
        Clause clause (tempConstr);
        vector<LiteralId> lids;
        for (size_t j = 0; j < formulas[i].range(); j++) {
          clause.addLiteral (Literal (freeLiteralId_, formulas[i].logVars()));
          lids.push_back (freeLiteralId_);
          freeLiteralId_ ++;
        }
        clauses_.push_back (clause);
        for (size_t j = 0; j < formulas[i].range() - 1; j++) {
          for (size_t k = j + 1; k < formulas[i].range(); k++) {
            ConstraintTree* tempConstr2 = new ConstraintTree (*(*it)->constr());
            tempConstr2->project (formulas[i].logVars());
            Clause clause2 (tempConstr2);
            clause2.addAndNegateLiteral (Literal (clause.literals()[j]));
            clause2.addAndNegateLiteral (Literal (clause.literals()[k]));
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

      double weight = (**it)[indexer];
      
      Clause clause1 ((*it)->constr());

      for (unsigned i = 0; i < groups.size(); i++) {
        LiteralId lid = getLiteralId (groups[i], indexer[i]);

        clause1.addAndNegateLiteral (Literal (lid, (*it)->argument(i).logVars()));

        Clause tempClause ((*it)->constr());
        tempClause.addAndNegateLiteral (Literal (paramVarLid, 1.0));
        tempClause.addLiteral (Literal (lid, (*it)->argument(i).logVars()));
        clauses_.push_back (tempClause);        
      }
      clause1.addLiteral (Literal (paramVarLid, weight));
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
        ConstraintTree tempCt (*(*it)->constr());
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
   for (LiteralId i = 0; i < freeLiteralId_; i++) {

     bool found = false;
     for (size_t j = 0; j < clauses_.size(); j++) {
       Literals literals = clauses_[j].literals();
       for (size_t k = 0; k < literals.size(); k++) {
         if (literals[k].lid() == i && literals[k].isPositive()) {
           cout << "weight(" << literals[k] << ") = " << literals[k].weight();
           cout << endl;
           found = true;
           break;
         }
       }
       if (found == true) {
         break;
       }
     }
     
     found = false;
     for (size_t j = 0; j < clauses_.size(); j++) {
       Literals literals = clauses_[j].literals();
       for (size_t k = 0; k < literals.size(); k++) {
         if (literals[k].lid() == i && literals[k].isNegative()) {
           cout << "weight(" << literals[k] << ") = " << literals[k].weight();
           cout << endl;
           found = true;
           break;
         }
       }
       if (found == true) {
         break;
       }
     }
     
   }
}



void
LiftedWCNF::printClauses (void) const
{
  for (unsigned i = 0; i < clauses_.size(); i++) {
    cout << clauses_[i] << endl;
  }
}

