#ifndef HORUS_PARFACTORLIST_H
#define HORUS_PARFACTORLIST_H

#include <list>

#include "Parfactor.h"
#include "ProbFormula.h"


using namespace std;


class Parfactor;

class ParfactorList
{
  public:
    ParfactorList (void) { }

    ParfactorList (const ParfactorList&);

    ParfactorList (const Parfactors&);

   ~ParfactorList (void);

    const list<Parfactor*>& parfactors (void) const { return pfList_; }

    void clear (void) { pfList_.clear(); }

    size_t size (void) const { return pfList_.size(); }

    typedef std::list<Parfactor*>::iterator iterator;

    iterator begin (void) { return pfList_.begin(); }

    iterator end (void) { return pfList_.end(); }

    typedef std::list<Parfactor*>::const_iterator const_iterator;

    const_iterator begin (void) const { return pfList_.begin(); }

    const_iterator end (void) const { return pfList_.end(); }

    void add (Parfactor* pf);

    void add (const Parfactors& pfs);

    void addShattered (Parfactor* pf);

    list<Parfactor*>::iterator insertShattered (
        list<Parfactor*>::iterator, Parfactor*);

    list<Parfactor*>::iterator remove (list<Parfactor*>::iterator);

    list<Parfactor*>::iterator removeAndDelete (list<Parfactor*>::iterator);

    bool isAllShattered (void) const;

    void print (void) const;

    ParfactorList& operator= (const ParfactorList& pfList);

  private:
    bool isShattered (const Parfactor*) const;

    bool isShattered (const Parfactor*, const Parfactor*) const;

    void addToShatteredList (Parfactor*);

    Parfactors shatterAgainstMySelf (Parfactor* g);

    Parfactors shatterAgainstMySelf2 (Parfactor* g);

    Parfactors shatterAgainstMySelf (
        Parfactor* g, size_t fIdx1, size_t fIdx2);

    std::pair<Parfactors, Parfactors> shatter (
        Parfactor*, Parfactor*);

    std::pair<Parfactors, Parfactors> shatter (
        size_t, Parfactor*, size_t, Parfactor*);

    Parfactors shatter (
        Parfactor*,
        size_t,
        ConstraintTree*,
        ConstraintTree*,
        PrvGroup);

    void updateGroups (PrvGroup group1, PrvGroup group2);

    bool proper (
        const ProbFormula&, ConstraintTree,
        const ProbFormula&, ConstraintTree) const;

    bool identical (
        const ProbFormula&, ConstraintTree,
        const ProbFormula&, ConstraintTree) const;

    bool disjoint (
        const ProbFormula&, ConstraintTree,
        const ProbFormula&, ConstraintTree) const;

    struct sortByParams
    {
      inline bool operator() (const Parfactor* pf1, const Parfactor* pf2)
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

    list<Parfactor*> pfList_;
};

#endif // HORUS_PARFACTORLIST_H

