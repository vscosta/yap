#include <cassert>

#include "ParfactorList.h"


ParfactorList::ParfactorList (Parfactors& pfs)
{
  pfList_.insert (pfList_.end(), pfs.begin(), pfs.end());
}



void
ParfactorList::add (Parfactor* pf)
{
  pf->setNewGroups();
  pfList_.push_back (pf);
}



void
ParfactorList::add (Parfactors& pfs)
{
  for (unsigned i = 0; i < pfs.size(); i++) {
    pfs[i]->setNewGroups();
    pfList_.push_back (pfs[i]);
  }
}



void
ParfactorList::addShattered (Parfactor* pf)
{
  pfList_.push_back (pf);
}



list<Parfactor*>::iterator
ParfactorList::remove (list<Parfactor*>::iterator it) 
{
  return pfList_.erase (it);
}



list<Parfactor*>::iterator
ParfactorList::deleteAndRemove (list<Parfactor*>::iterator it)
{
  delete *it;
  return pfList_.erase (it);
}



void
ParfactorList::shatter (void)
{
  list<Parfactor*> tempList;
  Parfactors newPfs;
  newPfs.insert (newPfs.end(), pfList_.begin(), pfList_.end());
  while (newPfs.empty() == false) {
    tempList.insert (tempList.end(), newPfs.begin(), newPfs.end());
    newPfs.clear();
    list<Parfactor*>::iterator iter1 = tempList.begin();
    while (tempList.size() > 1 && iter1 != -- tempList.end()) {
      list<Parfactor*>::iterator iter2 = iter1;
      ++ iter2;
      bool incIter1 = true;
      while (iter2 != tempList.end()) {
        assert (iter1 != iter2);
        std::pair<Parfactors, Parfactors> res = shatter (
            (*iter1)->formulas(), *iter1, (*iter2)->formulas(), *iter2);
        bool incIter2 = true;
        if (res.second.empty() == false) {
          // cout << "second unshattered" << endl;
          delete *iter2;
          iter2 = tempList.erase (iter2);
          incIter2 = false;
          newPfs.insert (
              newPfs.begin(), res.second.begin(), res.second.end());
        }
        if (res.first.empty() == false) {
          // cout << "first unshattered" << endl;
          delete *iter1;
          iter1 = tempList.erase (iter1);
          newPfs.insert (
              newPfs.begin(), res.first.begin(), res.first.end());
          incIter1 = false;
          break;
        }
        if (incIter2) {
          ++ iter2;
        }
      }
      if (incIter1) {
        ++ iter1;
      }
    }
    // cout << "|||||||||||||||||||||||||||||||||||||||||||||||||" << endl;
    // cout << "||||||||||||| SHATTERING ITERATION ||||||||||||||" << endl;
    // cout << "|||||||||||||||||||||||||||||||||||||||||||||||||" << endl;
    // printParfactors (newPfs);
    // cout << "|||||||||||||||||||||||||||||||||||||||||||||||||" << endl;
  }
  pfList_.clear();
  pfList_.insert (pfList_.end(), tempList.begin(), tempList.end());
}



void
ParfactorList::print (void) const
{
  list<Parfactor*>::const_iterator it;
  for (it = pfList_.begin(); it != pfList_.end(); ++it) {
    (*it)->print();
    cout << endl;
  }
}



std::pair<Parfactors, Parfactors>
ParfactorList::shatter (
    ProbFormulas& formulas1,
    Parfactor* g1,
    ProbFormulas& formulas2,
    Parfactor* g2)
{
  assert (g1 != 0 && g2 != 0 && g1 != g2);
  for (unsigned i = 0; i < formulas1.size(); i++) {
    for (unsigned j = 0; j < formulas2.size(); j++) {
      if (formulas1[i].sameSkeletonAs (formulas2[j])) {
        std::pair<Parfactors, Parfactors> res 
            = shatter (formulas1[i], g1, formulas2[j], g2);
        if (res.first.empty()  == false || 
            res.second.empty() == false) {
          return res;
        }
      }
    }
  }
  return make_pair (Parfactors(), Parfactors());
}



std::pair<Parfactors, Parfactors>
ParfactorList::shatter (
    ProbFormula&  f1,
    Parfactor*    g1,
    ProbFormula&  f2,
    Parfactor*    g2)
{
  // cout << endl;
  // cout << "-------------------------------------------------" << endl;
  // cout << "-> SHATTERING (#" << g1 << ", #" << g2 << ")" << endl;
  // g1->print();
  // cout << "-> WITH" << endl;
  // g2->print();
  // cout << "-> ON: " << f1.toString (g1->constr()) << endl;
  // cout << "-> ON: " << f2.toString (g2->constr()) << endl;
  // cout << "-------------------------------------------------" << endl;

  if (f1.isAtom()) {
    unsigned group = (f1.group() < f2.group()) ? f1.group() : f2.group();
    f1.setGroup (group);
    f2.setGroup (group);
    return { };
  }
  assert (g1->constr()->empty() == false);
  assert (g2->constr()->empty() == false);
  if (f1.group() == f2.group()) {
    // assert (identical (f1, g1->constr(), f2, g2->constr()));
    return { };
  }

  g1->constr()->moveToTop (f1.logVars());
  g2->constr()->moveToTop (f2.logVars());

  std::pair<ConstraintTree*,ConstraintTree*> split1 =
      g1->constr()->split (g2->constr(), f1.arity());
  ConstraintTree* commCt1 = split1.first;
  ConstraintTree* exclCt1 = split1.second;

  if (commCt1->empty()) {
    // disjoint 
    delete commCt1;
    delete exclCt1;
    return { };
  }

  std::pair<ConstraintTree*,ConstraintTree*> split2 =
      g2->constr()->split (g1->constr(), f2.arity());
  ConstraintTree* commCt2 = split2.first;
  ConstraintTree* exclCt2 = split2.second;

  assert (commCt1->tupleSet (f1.arity()) == 
          commCt2->tupleSet (f2.arity()));

  // stringstream ss1; ss1 << "" << count << "_A.dot" ;
  // stringstream ss2; ss2 << "" << count << "_B.dot" ;
  // stringstream ss3; ss3 << "" << count << "_A_comm.dot" ;
  // stringstream ss4; ss4 << "" << count << "_A_excl.dot" ;
  // stringstream ss5; ss5 << "" << count << "_B_comm.dot" ;
  // stringstream ss6; ss6 << "" << count << "_B_excl.dot" ;
  // ct1->exportToGraphViz     (ss1.str().c_str(), true);
  // ct2->exportToGraphViz     (ss2.str().c_str(), true);
  // commCt1->exportToGraphViz (ss3.str().c_str(), true);
  // exclCt1->exportToGraphViz (ss4.str().c_str(), true);
  // commCt2->exportToGraphViz (ss5.str().c_str(), true);
  // exclCt2->exportToGraphViz (ss6.str().c_str(), true);

  if (exclCt1->empty() && exclCt2->empty()) {
    unsigned group = (f1.group() < f2.group()) ? f1.group() : f2.group();
    // identical
    f1.setGroup (group);
    f2.setGroup (group);
    // unifyGroups
    delete commCt1;
    delete exclCt1;
    delete commCt2;
    delete exclCt2;
    return { };
  }

  unsigned group;
  if (exclCt1->empty()) {
    group = f1.group();
  } else if (exclCt2->empty()) {
    group = f2.group();
  } else {
    group = ProbFormula::getNewGroup();
  }
  Parfactors res1 = shatter (g1, f1, commCt1, exclCt1, group);
  Parfactors res2 = shatter (g2, f2, commCt2, exclCt2, group);
  return make_pair (res1, res2);
}



Parfactors
ParfactorList::shatter (
    Parfactor* g,
    const ProbFormula& f,
    ConstraintTree* commCt,
    ConstraintTree* exclCt,
    unsigned commGroup)
{
  Parfactors result;
  if (f.isCounting()) {
    LogVar X_new1 = g->constr()->logVarSet().back() + 1;
    LogVar X_new2 = g->constr()->logVarSet().back() + 2;
    ConstraintTrees cts = g->constr()->jointCountNormalize (
        commCt, exclCt, f.countedLogVar(), X_new1, X_new2);
    for (unsigned i = 0; i < cts.size(); i++) {
      Parfactor* newPf = new Parfactor (g, cts[i]);
      if (cts[i]->nrLogVars() == g->constr()->nrLogVars() + 1) {
        newPf->expandPotential (f.countedLogVar(), X_new1, X_new2);
        assert (g->constr()->getConditionalCount (f.countedLogVar()) == 
            cts[i]->getConditionalCount (X_new1) +
            cts[i]->getConditionalCount (X_new2));
      } else {
        assert (g->constr()->getConditionalCount (f.countedLogVar()) == 
            cts[i]->getConditionalCount (f.countedLogVar()));
      }
      newPf->setNewGroups();
      result.push_back (newPf);
    }
  } else {
    if (exclCt->empty()) {
      delete commCt;
      delete exclCt;
      g->setFormulaGroup (f, commGroup);      
    } else {
      Parfactor* newPf = new Parfactor (g, commCt);
      newPf->setNewGroups();
      newPf->setFormulaGroup (f, commGroup);
      result.push_back (newPf);
      newPf = new Parfactor (g, exclCt);
      newPf->setNewGroups();
      result.push_back (newPf);
    }
  }
  return result;
}



void
ParfactorList::unifyGroups (unsigned group1, unsigned group2)
{
  unsigned newGroup = ProbFormula::getNewGroup();
  for (ParfactorList::iterator it = pfList_.begin();
       it != pfList_.end(); it++) {
    ProbFormulas& formulas = (*it)->formulas();
    for (unsigned i = 0; i < formulas.size(); i++) {
      if (formulas[i].group() == group1 || 
          formulas[i].group() == group2) {
        formulas[i].setGroup (newGroup);
      }
    }
  }
}

