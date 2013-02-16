#include <vector>
#include <queue>
#include <iostream>

#include "LiftedOperations.h"


namespace Horus {

namespace LiftedOperations {

namespace {

Parfactors absorve (ObservedFormula& obsFormula, Parfactor* g);

}

void
shatterAgainstQuery (ParfactorList& pfList, const Grounds& query)
{
  for (size_t i = 0; i < query.size(); i++) {
    if (query[i].isAtom()) {
      continue;
    }
    bool found = false;
    Parfactors newPfs;
    ParfactorList::iterator it = pfList.begin();
    while (it != pfList.end()) {
      if ((*it)->containsGround (query[i])) {
        found = true;
        std::pair<ConstraintTree*, ConstraintTree*> split;
        LogVars queryLvs (
            (*it)->constr()->logVars().begin(),
            (*it)->constr()->logVars().begin() + query[i].arity());
        split = (*it)->constr()->split (query[i].args());
        ConstraintTree* commCt = split.first;
        ConstraintTree* exclCt = split.second;
        newPfs.push_back (new Parfactor (*it, commCt));
        if (exclCt->empty() == false) {
          newPfs.push_back (new Parfactor (*it, exclCt));
        } else {
          delete exclCt;
        }
        it = pfList.removeAndDelete (it);
      } else {
        ++ it;
      }
    }
    if (found == false) {
      std::cerr << "Error: could not find a parfactor with ground " ;
      std::cerr << "`" << query[i] << "'." << std::endl;
      exit (EXIT_FAILURE);
    }
    pfList.add (newPfs);
  }
  if (Globals::verbosity > 2) {
    Util::printAsteriskLine();
    std::cout << "SHATTERED AGAINST THE QUERY" << std::endl;
    for (size_t i = 0; i < query.size(); i++) {
      std::cout << " -> " << query[i] << std::endl;
    }
    Util::printAsteriskLine();
    pfList.print();
  }
}



void
runWeakBayesBall (ParfactorList& pfList, const Grounds& query)
{
  std::queue<PrvGroup> todo; // groups to process
  std::set<PrvGroup> done;   // processed or in queue
  for (size_t i = 0; i < query.size(); i++) {
    ParfactorList::iterator it = pfList.begin();
    while (it != pfList.end()) {
      PrvGroup group = (*it)->findGroup (query[i]);
      if (group != std::numeric_limits<PrvGroup>::max()) {
        todo.push (group);
        done.insert (group);
        break;
      }
      ++ it;
    }
  }

  std::set<Parfactor*> requiredPfs;
  while (todo.empty() == false) {
    PrvGroup group = todo.front();
    ParfactorList::iterator it = pfList.begin();
    while (it != pfList.end()) {
      if (Util::contains (requiredPfs, *it) == false &&
          (*it)->containsGroup (group)) {
        std::vector<PrvGroup> groups = (*it)->getAllGroups();
        for (size_t i = 0; i < groups.size(); i++) {
          if (Util::contains (done, groups[i]) == false) {
            todo.push (groups[i]);
            done.insert (groups[i]);
          }
        }
        requiredPfs.insert (*it);
      }
      ++ it;
    }
    todo.pop();
  }

  ParfactorList::iterator it = pfList.begin();
  bool foundNotRequired = false;
  while (it != pfList.end()) {
    if (Util::contains (requiredPfs, *it) == false) {
      if (Globals::verbosity > 2) {
        if (foundNotRequired == false) {
          Util::printHeader ("PARFACTORS TO DISCARD");
          foundNotRequired = true;
        }
        (*it)->print();
      }
      it = pfList.removeAndDelete (it);
    } else {
      ++ it;
    }
  }
}



void
absorveEvidence (ParfactorList& pfList, ObservedFormulas& obsFormulas)
{
  for (size_t i = 0; i < obsFormulas.size(); i++) {
    Parfactors newPfs;
    ParfactorList::iterator it  = pfList.begin();
    while (it != pfList.end()) {
      Parfactor* pf = *it;
      it = pfList.remove (it);
      Parfactors absorvedPfs = absorve (obsFormulas[i], pf);
      if (absorvedPfs.empty() == false) {
        if (absorvedPfs.size() == 1 && !absorvedPfs[0]) {
          // just remove pf;
        } else {
          Util::addToVector (newPfs, absorvedPfs);
        }
        delete pf;
      } else {
        it = pfList.insertShattered (it, pf);
        ++ it;
      }
    }
    pfList.add (newPfs);
  }
  if (Globals::verbosity > 2 && obsFormulas.empty() == false) {
    Util::printAsteriskLine();
    std::cout << "AFTER EVIDENCE ABSORVED" << std::endl;
    for (size_t i = 0; i < obsFormulas.size(); i++) {
      std::cout << " -> " << obsFormulas[i] << std::endl;
    }
    Util::printAsteriskLine();
    pfList.print();
  }
}



Parfactors
countNormalize (Parfactor* g, const LogVarSet& set)
{
  Parfactors normPfs;
  if (set.empty()) {
    normPfs.push_back (new Parfactor (*g));
  } else {
    ConstraintTrees normCts = g->constr()->countNormalize (set);
    for (size_t i = 0; i < normCts.size(); i++) {
      normPfs.push_back (new Parfactor (g, normCts[i]));
    }
  }
  return normPfs;
}



Parfactor
calcGroundMultiplication (Parfactor pf)
{
  LogVarSet lvs = pf.constr()->logVarSet();
  lvs -= pf.constr()->singletons();
  Parfactors newPfs = {new Parfactor (pf)};
  for (size_t i = 0; i < lvs.size(); i++) {
    Parfactors pfs = newPfs;
    newPfs.clear();
    for (size_t j = 0; j < pfs.size(); j++) {
      bool countedLv = pfs[j]->countedLogVars().contains (lvs[i]);
      if (countedLv) {
        pfs[j]->fullExpand (lvs[i]);
        newPfs.push_back (pfs[j]);
      } else {
        ConstraintTrees cts = pfs[j]->constr()->ground (lvs[i]);
        for (size_t k = 0; k < cts.size(); k++) {
          newPfs.push_back (new Parfactor (pfs[j], cts[k]));
        }
        delete pfs[j];
      }
    }
  }
  ParfactorList pfList (newPfs);
  Parfactors groundShatteredPfs (pfList.begin(),pfList.end());
  for (size_t i = 1; i < groundShatteredPfs.size(); i++) {
     groundShatteredPfs[0]->multiply (*groundShatteredPfs[i]);
  }
  return Parfactor (*groundShatteredPfs[0]);
}



namespace {

Parfactors
absorve (ObservedFormula& obsFormula, Parfactor* g)
{
  Parfactors absorvedPfs;
  const ProbFormulas& formulas = g->arguments();
  for (size_t i = 0; i < formulas.size(); i++) {
    if (obsFormula.functor() == formulas[i].functor() &&
        obsFormula.arity()   == formulas[i].arity()) {

      if (obsFormula.isAtom()) {
        if (formulas.size() > 1) {
          g->absorveEvidence (formulas[i], obsFormula.evidence());
        } else {
          // hack to erase parfactor g
          absorvedPfs.push_back (0);
        }
        break;
      }

      g->constr()->moveToTop (formulas[i].logVars());
      std::pair<ConstraintTree*, ConstraintTree*> res;
      res = g->constr()->split (
          formulas[i].logVars(),
          &(obsFormula.constr()),
          obsFormula.constr().logVars());
      ConstraintTree* commCt = res.first;
      ConstraintTree* exclCt = res.second;

      if (commCt->empty() == false) {
        if (formulas.size() > 1) {
          LogVarSet excl = g->exclusiveLogVars (i);
          Parfactor tempPf (g, commCt);
          Parfactors countNormPfs = LiftedOperations::countNormalize (
              &tempPf, excl);
          for (size_t j = 0; j < countNormPfs.size(); j++) {
            countNormPfs[j]->absorveEvidence (
                formulas[i], obsFormula.evidence());
            absorvedPfs.push_back (countNormPfs[j]);
          }
        } else {
          delete commCt;
        }
        if (exclCt->empty() == false) {
          absorvedPfs.push_back (new Parfactor (g, exclCt));
        } else {
          delete exclCt;
        }
        if (absorvedPfs.empty()) {
          // hack to erase parfactor g
          absorvedPfs.push_back (0);
        }
        break;
      } else {
        delete commCt;
        delete exclCt;
      }
    }
  }
  return absorvedPfs;
}

}

}  // namespace LiftedOperations

}  // namespace Horus

