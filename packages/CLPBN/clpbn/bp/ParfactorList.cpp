#include <cassert>

#include "ParfactorList.h"


ParfactorList::ParfactorList (const ParfactorList& pfList)
{
  ParfactorList::const_iterator it = pfList.begin();
  while (it != pfList.end()) {
    addShattered (new Parfactor (**it));
    ++ it;
  }
  
}



ParfactorList::ParfactorList (const Parfactors& pfs)
{
  add (pfs);
}



ParfactorList::~ParfactorList (void)
{
  ParfactorList::const_iterator it = pfList_.begin();
  while (it != pfList_.end()) {
    delete *it;
    ++ it;
  }
}



void
ParfactorList::add (Parfactor* pf)
{
  pf->setNewGroups();
  addToShatteredList (pf);
}



void
ParfactorList::add (const Parfactors& pfs)
{
  for (unsigned i = 0; i < pfs.size(); i++) {
    pfs[i]->setNewGroups();
    addToShatteredList (pfs[i]);
  }
}



void
ParfactorList::addShattered (Parfactor* pf)
{
  assert (isAllShattered());
  pfList_.push_back (pf);
  assert (isAllShattered());
}



list<Parfactor*>::iterator
ParfactorList::insertShattered (
    list<Parfactor*>::iterator it,
    Parfactor* pf)
{
  return pfList_.insert (it, pf);
  assert (isAllShattered());
}



list<Parfactor*>::iterator
ParfactorList::remove (list<Parfactor*>::iterator it) 
{
  return pfList_.erase (it);
}



list<Parfactor*>::iterator
ParfactorList::removeAndDelete (list<Parfactor*>::iterator it)
{
  delete *it;
  return pfList_.erase (it);
}



bool
ParfactorList::isAllShattered (void) const
{
  if (pfList_.size() <= 1) {
    return true;
  }
  vector<Parfactor*> pfs (pfList_.begin(), pfList_.end());
  for (unsigned i = 0; i < pfs.size() - 1; i++) {
    for (unsigned j = i + 1; j < pfs.size(); j++) {
      if (isShattered (pfs[i], pfs[j])  == false) {
        return false;
      }
    }
  }
  return true;
}



void
ParfactorList::print (void) const
{
  list<Parfactor*>::const_iterator it;
  for (it = pfList_.begin(); it != pfList_.end(); ++it) {
    (*it)->print();
  }
}



bool
ParfactorList::isShattered (
    const Parfactor* g1,
    const Parfactor* g2) const
{
  assert (g1 != g2);
  const ProbFormulas& fms1 = g1->arguments();
  const ProbFormulas& fms2 = g2->arguments();
  for (unsigned i = 0; i < fms1.size(); i++) {
    for (unsigned j = 0; j < fms2.size(); j++) {
      if (fms1[i].group() == fms2[j].group()) {
        if (identical (
            fms1[i], *(g1->constr()),
            fms2[j], *(g2->constr())) == false) {
          return false;
        }
      } else {
        if (disjoint (
            fms1[i], *(g1->constr()),
            fms2[j], *(g2->constr())) == false) {
          return false;
        }
      }
    }
  }
  return true;
}


	
void
ParfactorList::addToShatteredList (Parfactor* g)
{
  queue<Parfactor*> residuals;
  residuals.push (g);
  while (residuals.empty() == false) {
    Parfactor* pf = residuals.front();
    bool pfSplitted = false;
    list<Parfactor*>::iterator pfIter;
    pfIter = pfList_.begin();
    while (pfIter != pfList_.end()) {
      std::pair<Parfactors, Parfactors> shattRes;
      shattRes = shatter (*pfIter, pf);
      if (shattRes.first.empty() == false) {
        pfIter = removeAndDelete (pfIter);
        Util::addToQueue (residuals, shattRes.first);
      } else {
        ++ pfIter;
      }
      if (shattRes.second.empty() == false) {
        delete pf;
        Util::addToQueue (residuals, shattRes.second);
        pfSplitted = true;
        break;
      }
    }
    residuals.pop();
    if (pfSplitted == false) {
      addShattered (pf);
    }
  }
  assert (isAllShattered());
}



std::pair<Parfactors, Parfactors>
ParfactorList::shatter (Parfactor* g1, Parfactor* g2)
{
  ProbFormulas& formulas1 = g1->arguments();
  ProbFormulas& formulas2 = g2->arguments();
  assert (g1 != 0 && g2 != 0 && g1 != g2);
  for (unsigned i = 0; i < formulas1.size(); i++) {
    for (unsigned j = 0; j < formulas2.size(); j++) {
      if (formulas1[i].sameSkeletonAs (formulas2[j])) {
        std::pair<Parfactors, Parfactors> res;
        res = shatter (i, g1, j, g2);
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
    unsigned fIdx1, Parfactor* g1,
    unsigned fIdx2, Parfactor* g2)
{
  ProbFormula& f1 = g1->argument (fIdx1);
  ProbFormula& f2 = g2->argument (fIdx2);
  // cout << endl;
  // Util::printDashedLine();
  // cout << "-> SHATTERING (#" << g1 << ", #" << g2 << ")" << endl;
  // g1->print();
  // cout << "-> WITH" << endl;
  // g2->print();
  // cout << "-> ON: " << f1 << "|" ;
  // cout << g1->constr()->tupleSet (f1.logVars()) << endl;
  // cout << "-> ON: " << f2 << "|" ;
  // cout << g2->constr()->tupleSet (f2.logVars()) << endl;
  // Util::printDashedLine();
  if (f1.isAtom()) {
    unsigned group = (f1.group() < f2.group()) ? f1.group() : f2.group();
    f1.setGroup (group);
    f2.setGroup (group);
    return { };
  }
  assert (g1->constr()->empty() == false);
  assert (g2->constr()->empty() == false);
  if (f1.group() == f2.group()) {
    assert (identical (f1, *(g1->constr()), f2, *(g2->constr())));
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

   // unsigned static count = 0; count ++;
   // stringstream ss1; ss1 << "" << count << "_A.dot" ;
   // stringstream ss2; ss2 << "" << count << "_B.dot" ;
   // stringstream ss3; ss3 << "" << count << "_A_comm.dot" ;
   // stringstream ss4; ss4 << "" << count << "_A_excl.dot" ;
   // stringstream ss5; ss5 << "" << count << "_B_comm.dot" ;
   // stringstream ss6; ss6 << "" << count << "_B_excl.dot" ;
   // g1->constr()->exportToGraphViz (ss1.str().c_str(), true);
   // g2->constr()->exportToGraphViz (ss2.str().c_str(), true);
   // commCt1->exportToGraphViz (ss3.str().c_str(), true);
   // exclCt1->exportToGraphViz (ss4.str().c_str(), true);
   // commCt2->exportToGraphViz (ss5.str().c_str(), true);
   // exclCt2->exportToGraphViz (ss6.str().c_str(), true);

  if (exclCt1->empty() && exclCt2->empty()) {
    unsigned group = (f1.group() < f2.group())
                   ? f1.group() 
                   : f2.group();
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
  Parfactors res1 = shatter (g1, fIdx1, commCt1, exclCt1, group);
  Parfactors res2 = shatter (g2, fIdx2, commCt2, exclCt2, group);
  return make_pair (res1, res2);
}



Parfactors
ParfactorList::shatter (
    Parfactor* g,
    unsigned fIdx,
    ConstraintTree* commCt,
    ConstraintTree* exclCt,
    unsigned commGroup)
{
  ProbFormula& f = g->argument (fIdx);
  if (exclCt->empty()) {
    delete commCt;
    delete exclCt;
    f.setGroup (commGroup);
    return { };
  }

  Parfactors result;
  if (f.isCounting()) {
    LogVar X_new1 = g->constr()->logVarSet().back() + 1;
    LogVar X_new2 = g->constr()->logVarSet().back() + 2;
    ConstraintTrees cts = g->constr()->jointCountNormalize (
        commCt, exclCt, f.countedLogVar(), X_new1, X_new2);
    for (unsigned i = 0; i < cts.size(); i++) {
      Parfactor* newPf = new Parfactor (g, cts[i]);
      if (cts[i]->nrLogVars() == g->constr()->nrLogVars() + 1) {
        newPf->expand (f.countedLogVar(), X_new1, X_new2);
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
    delete commCt;
    delete exclCt;
  } else {
    Parfactor* newPf = new Parfactor (g, commCt);
    newPf->setNewGroups();
    newPf->argument (fIdx).setGroup (commGroup);
    result.push_back (newPf);
    newPf = new Parfactor (g, exclCt);
    newPf->setNewGroups();
    result.push_back (newPf);
  }
  return result;
}



void
ParfactorList::unifyGroups (unsigned group1, unsigned group2)
{
  unsigned newGroup = ProbFormula::getNewGroup();
  for (ParfactorList::iterator it = pfList_.begin();
       it != pfList_.end(); it++) {
    ProbFormulas& formulas = (*it)->arguments();
    for (unsigned i = 0; i < formulas.size(); i++) {
      if (formulas[i].group() == group1 || 
          formulas[i].group() == group2) {
        formulas[i].setGroup (newGroup);
      }
    }
  }
}



bool
ParfactorList::proper (
    const ProbFormula& f1, ConstraintTree c1,
    const ProbFormula& f2, ConstraintTree c2) const
{
  return disjoint  (f1, c1, f2, c2)
      || identical (f1, c1, f2, c2);
}



bool
ParfactorList::identical (
    const ProbFormula& f1, ConstraintTree c1,
    const ProbFormula& f2, ConstraintTree c2) const
{
  if (f1.sameSkeletonAs (f2) == false) {
    return false;
  }
  if (f1.isAtom()) {
    return true;
  }
  c1.moveToTop (f1.logVars());
  c2.moveToTop (f2.logVars());
  return ConstraintTree::identical (
      &c1, &c2, f1.logVars().size());
}



bool
ParfactorList::disjoint (
    const ProbFormula& f1, ConstraintTree c1,
    const ProbFormula& f2, ConstraintTree c2) const
{
  if (f1.sameSkeletonAs (f2) == false) {
    return true;
  }
  if (f1.isAtom()) {
    return true;
  }
  c1.moveToTop (f1.logVars());
  c2.moveToTop (f2.logVars());
  return ConstraintTree::overlap (
      &c1, &c2, f1.arity()) == false;
}

