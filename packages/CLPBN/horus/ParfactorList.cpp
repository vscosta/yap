#include <cassert>

#include <queue>
#include <iostream>
#include <sstream>

#include "ParfactorList.h"


namespace Horus {

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



ParfactorList::~ParfactorList()
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
  for (size_t i = 0; i < pfs.size(); i++) {
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



std::list<Parfactor*>::iterator
ParfactorList::insertShattered (
    std::list<Parfactor*>::iterator it,
    Parfactor* pf)
{
  assert (isAllShattered());
  return pfList_.insert (it, pf);
}



std::list<Parfactor*>::iterator
ParfactorList::remove (std::list<Parfactor*>::iterator it)
{
  return pfList_.erase (it);
}



std::list<Parfactor*>::iterator
ParfactorList::removeAndDelete (std::list<Parfactor*>::iterator it)
{
  delete *it;
  return pfList_.erase (it);
}



bool
ParfactorList::isAllShattered() const
{
  if (pfList_.size() <= 1) {
    return true;
  }
  Parfactors pfs (pfList_.begin(), pfList_.end());
  for (size_t i = 0; i < pfs.size(); i++) {
    assert (isShattered (pfs[i]));
  }
  for (size_t i = 0; i < pfs.size() - 1; i++) {
    for (size_t j = i + 1; j < pfs.size(); j++) {
      if (isShattered (pfs[i], pfs[j])  == false) {
        return false;
      }
    }
  }
  return true;
}



struct sortByParams {
  bool operator() (const Parfactor* pf1, const Parfactor* pf2) const
  {
    if (pf1->params().size() < pf2->params().size()) {
      return true;
    } else if (pf1->params().size() == pf2->params().size() &&
               pf1->params()        <  pf2->params()) {
      return true;
    }
    return false;
  }
};



void
ParfactorList::print() const
{
  Parfactors pfVec (pfList_.begin(), pfList_.end());
  std::sort (pfVec.begin(), pfVec.end(), sortByParams());
  for (size_t i = 0; i < pfVec.size(); i++) {
    pfVec[i]->print();
    std::cout << std::endl;
  }
}



ParfactorList&
ParfactorList::operator= (const ParfactorList& pfList)
{
  if (this != &pfList) {
    ParfactorList::const_iterator it0 = pfList_.begin();
    while (it0 != pfList_.end()) {
      delete *it0;
      ++ it0;
    }
    pfList_.clear();
    ParfactorList::const_iterator it = pfList.begin();
    while (it != pfList.end()) {
      addShattered (new Parfactor (**it));
      ++ it;
    }
  }
  return *this;
}



bool
ParfactorList::isShattered (const Parfactor* g) const
{
  const ProbFormulas& formulas = g->arguments();
  if (formulas.size() < 2) {
    return true;
  }
  ConstraintTree ct (*g->constr());
  for (size_t i = 0; i < formulas.size() - 1; i++) {
    for (size_t j = i + 1; j < formulas.size(); j++) {
      if (formulas[i].group() == formulas[j].group()) {
        if (identical (
            formulas[i], *(g->constr()),
            formulas[j], *(g->constr())) == false) {
          g->print();
          std::cout << "-> not identical on positions " ;
          std::cout << i << " and " << j << std::endl;
          return false;
        }
      } else {
        if (disjoint (
            formulas[i], *(g->constr()),
            formulas[j], *(g->constr())) == false) {
          g->print();
          std::cout << "-> not disjoint on positions " ;
          std::cout << i << " and " << j << std::endl;
          return false;
        }
      }
    }
  }
  return true;
}



bool
ParfactorList::isShattered (
    const Parfactor* g1,
    const Parfactor* g2) const
{
  assert (g1 != g2);
  const ProbFormulas& fms1 = g1->arguments();
  const ProbFormulas& fms2 = g2->arguments();

  for (size_t i = 0; i < fms1.size(); i++) {
    for (size_t j = 0; j < fms2.size(); j++) {
      if (fms1[i].group() == fms2[j].group()) {
        if (identical (
            fms1[i], *(g1->constr()),
            fms2[j], *(g2->constr())) == false) {
          g1->print();
          std::cout << "^" << std::endl;
          g2->print();
          std::cout << "-> not identical on group " ;
          std::cout << fms1[i].group() << std::endl;
          return false;
        }
      } else {
        if (disjoint (
            fms1[i], *(g1->constr()),
            fms2[j], *(g2->constr())) == false) {
          g1->print();
          std::cout << "^" << std::endl;
          g2->print();
          std::cout << "-> not disjoint on groups " << fms1[i].group();
          std::cout << " and " << fms2[j].group() << std::endl;
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
  std::queue<Parfactor*> residuals;
  residuals.push (g);
  while (residuals.empty() == false) {
    Parfactor* pf = residuals.front();
    bool pfSplitted = false;
    std::list<Parfactor*>::iterator pfIter;
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
      Parfactors res = shatterAgainstMySelf (pf);
      if (res.empty()) {
        addShattered (pf);
      } else {
        Util::addToQueue (residuals, res);
      }
    }
  }
  assert (isAllShattered());
}



Parfactors
ParfactorList::shatterAgainstMySelf (Parfactor* g)
{
  Parfactors pfs;
  std::queue<Parfactor*> residuals;
  residuals.push (g);
  bool shattered = true;
  while (residuals.empty() == false) {
    Parfactor* pf = residuals.front();
    Parfactors res = shatterAgainstMySelf2 (pf);
    if (res.empty()) {
      assert (isShattered (pf));
      if (shattered) {
        return { };
      }
      pfs.push_back (pf);
    } else {
      shattered = false;
      for (size_t i = 0; i < res.size(); i++) {
        assert (res[i]->constr()->empty() == false);
        residuals.push (res[i]);
      }
      delete pf;
    }
    residuals.pop();
  }
  return pfs;
}



Parfactors
ParfactorList::shatterAgainstMySelf2 (Parfactor* g)
{
  // slip a parfactor with overlapping formulas:
  // e.g. {s(X),s(Y)}, with (X,Y) in {(p1,p2),(p1,p3),(p4,p1)}
  const ProbFormulas& formulas = g->arguments();
  for (size_t i = 0; i < formulas.size() - 1; i++) {
    for (size_t j = i + 1; j < formulas.size(); j++) {
      if (formulas[i].sameSkeletonAs (formulas[j])) {
        Parfactors res = shatterAgainstMySelf (g, i, j);
        if (res.empty() == false) {
          return res;
        }
      }
    }
  }
  return Parfactors();
}



Parfactors
ParfactorList::shatterAgainstMySelf (
    Parfactor* g,
    size_t fIdx1,
    size_t fIdx2)
{
  /*
  Util::printDashedLine();
  std::cout << "-> SHATTERING" << std::endl;
  g->print();
  std::cout << "-> ON: " << g->argument (fIdx1) << "|" ;
  std::cout << g->constr()->tupleSet (g->argument (fIdx1).logVars());
  std::cout << std::endl;
  std::cout << "-> ON: " << g->argument (fIdx2) << "|" ;
  std::cout << g->constr()->tupleSet (g->argument (fIdx2).logVars())
  std::cout << std::endl;
  Util::printDashedLine();
  */
  ProbFormula& f1 = g->argument (fIdx1);
  ProbFormula& f2 = g->argument (fIdx2);
  if (f1.isAtom()) {
    std::cerr << "Error: a ground occurs twice in the same parfactor." ;
    std::cerr << std::endl;
    std::cerr << std::endl;
    exit (EXIT_FAILURE);
  }
  assert (g->constr()->empty() == false);
  ConstraintTree ctCopy (*g->constr());
  if (f1.group() == f2.group()) {
    assert (identical (f1, *(g->constr()), f2, ctCopy));
    return { };
  }

  g->constr()->moveToTop (f1.logVars());
  ctCopy.moveToTop (f2.logVars());

  std::pair<ConstraintTree*,ConstraintTree*> split1 =
      g->constr()->split (f1.logVars(), &ctCopy, f2.logVars());
  ConstraintTree* commCt1 = split1.first;
  ConstraintTree* exclCt1 = split1.second;

  if (commCt1->empty()) {
    // disjoint
    delete commCt1;
    delete exclCt1;
    return { };
  }

  PrvGroup newGroup = ProbFormula::getNewGroup();
  Parfactors res1 = shatter (g, fIdx1, commCt1, exclCt1, newGroup);
  if (res1.empty()) {
    res1.push_back (g);
  }

  Parfactors res;
  ctCopy.moveToTop (f1.logVars());
  for (size_t i = 0; i < res1.size(); i++) {
    res1[i]->constr()->moveToTop (f2.logVars());
    std::pair<ConstraintTree*, ConstraintTree*> split2;
    split2 = res1[i]->constr()->split (f2.logVars(), &ctCopy, f1.logVars());
    ConstraintTree* commCt2 = split2.first;
    ConstraintTree* exclCt2 = split2.second;
    if (commCt2->empty()) {
      if (res1[i] != g) {
        res.push_back (res1[i]);
      }
      delete commCt2;
      delete exclCt2;
      continue;
    }
    newGroup = ProbFormula::getNewGroup();
    Parfactors res2 = shatter (res1[i], fIdx2, commCt2, exclCt2, newGroup);
    if (res2.empty()) {
      if (res1[i] != g) {
        res.push_back (res1[i]);
      }
    } else {
      Util::addToVector (res, res2);
      for (size_t j = 0; j < res2.size(); j++) {
      }
      if (res1[i] != g) {
        delete res1[i];
      }
    }
  }

  if (res.empty()) {
    g->argument (fIdx2).setGroup (g->argument (fIdx1).group());
    updateGroups (f2.group(), f1.group());
  }
  return res;
}



std::pair<Parfactors, Parfactors>
ParfactorList::shatter (Parfactor* g1, Parfactor* g2)
{
  ProbFormulas& formulas1 = g1->arguments();
  ProbFormulas& formulas2 = g2->arguments();
  assert (g1 && g2 && g1 != g2);
  for (size_t i = 0; i < formulas1.size(); i++) {
    for (size_t j = 0; j < formulas2.size(); j++) {
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
    size_t fIdx1, Parfactor* g1,
    size_t fIdx2, Parfactor* g2)
{
  ProbFormula& f1 = g1->argument (fIdx1);
  ProbFormula& f2 = g2->argument (fIdx2);
  /*
  Util::printDashedLine();
  std::cout << "-> SHATTERING" << std::endl;
  g1->print();
  std::cout << "-> WITH" << std::endl;
  g2->print();
  std::cout << "-> ON: " << f1 << "|" ;
  std::cout << g1->constr()->tupleSet (f1.logVars()) << std::endl;
  std::cout << "-> ON: " << f2 << "|" ;
  std::cout << g2->constr()->tupleSet (f2.logVars()) << std::endl;
  Util::printDashedLine();
  */
  if (f1.isAtom()) {
    f2.setGroup (f1.group());
    updateGroups (f2.group(), f1.group());
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
      g1->constr()->split (f1.logVars(), g2->constr(), f2.logVars());
  ConstraintTree* commCt1 = split1.first;
  ConstraintTree* exclCt1 = split1.second;

  if (commCt1->empty()) {
    // disjoint
    delete commCt1;
    delete exclCt1;
    return { };
  }

  std::pair<ConstraintTree*,ConstraintTree*> split2 =
      g2->constr()->split (f2.logVars(), g1->constr(), f1.logVars());
  ConstraintTree* commCt2 = split2.first;
  ConstraintTree* exclCt2 = split2.second;

  assert (commCt1->tupleSet (f1.logVars()) ==
          commCt2->tupleSet (f2.logVars()));

   // std::stringstream ss1; ss1 << "" << count << "_A.dot" ;
   // std::stringstream ss2; ss2 << "" << count << "_B.dot" ;
   // std::stringstream ss3; ss3 << "" << count << "_A_comm.dot" ;
   // std::stringstream ss4; ss4 << "" << count << "_A_excl.dot" ;
   // std::stringstream ss5; ss5 << "" << count << "_B_comm.dot" ;
   // std::stringstream ss6; ss6 << "" << count << "_B_excl.dot" ;
   // g1->constr()->exportToGraphViz (ss1.str().c_str(), true);
   // g2->constr()->exportToGraphViz (ss2.str().c_str(), true);
   // commCt1->exportToGraphViz (ss3.str().c_str(), true);
   // exclCt1->exportToGraphViz (ss4.str().c_str(), true);
   // commCt2->exportToGraphViz (ss5.str().c_str(), true);
   // exclCt2->exportToGraphViz (ss6.str().c_str(), true);

  if (exclCt1->empty() && exclCt2->empty()) {
    // identical
    f2.setGroup (f1.group());
    updateGroups (f2.group(), f1.group());
    delete commCt1;
    delete exclCt1;
    delete commCt2;
    delete exclCt2;
    return { };
  }

  PrvGroup group;
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
    size_t fIdx,
    ConstraintTree* commCt,
    ConstraintTree* exclCt,
    PrvGroup commGroup)
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
    for (size_t i = 0; i < cts.size(); i++) {
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
ParfactorList::updateGroups (PrvGroup oldGroup, PrvGroup newGroup)
{
  for (ParfactorList::iterator it = pfList_.begin();
       it != pfList_.end(); ++it) {
    ProbFormulas& formulas = (*it)->arguments();
    for (size_t i = 0; i < formulas.size(); i++) {
      if (formulas[i].group() == oldGroup) {
        formulas[i].setGroup (newGroup);
      }
    }
  }
}



bool
ParfactorList::proper (
    const ProbFormula& f1, ConstraintTree ct1,
    const ProbFormula& f2, ConstraintTree ct2) const
{
  return disjoint  (f1, ct1, f2, ct2)
      || identical (f1, ct1, f2, ct2);
}



bool
ParfactorList::identical (
    const ProbFormula& f1, ConstraintTree ct1,
    const ProbFormula& f2, ConstraintTree ct2) const
{
  if (f1.sameSkeletonAs (f2) == false) {
    return false;
  }
  if (f1.isAtom()) {
    return true;
  }
  TupleSet ts1 = ct1.tupleSet (f1.logVars());
  TupleSet ts2 = ct2.tupleSet (f2.logVars());
  return ts1 == ts2;
}



bool
ParfactorList::disjoint (
    const ProbFormula& f1, ConstraintTree ct1,
    const ProbFormula& f2, ConstraintTree ct2) const
{
  if (f1.sameSkeletonAs (f2) == false) {
    return true;
  }
  if (f1.isAtom()) {
    return false;
  }
  TupleSet ts1 = ct1.tupleSet (f1.logVars());
  TupleSet ts2 = ct2.tupleSet (f2.logVars());
  return (ts1 & ts2).empty();
}

}  // namespace Horus

