#ifndef YAP_PACKAGES_CLPBN_HORUS_PARFACTORLIST_H_
#define YAP_PACKAGES_CLPBN_HORUS_PARFACTORLIST_H_

#include <list>

#include "Parfactor.h"
#include "ProbFormula.h"


namespace Horus {

class Parfactor;


class ParfactorList {
  public:
    ParfactorList() { }

    ParfactorList (const ParfactorList&);

    ParfactorList (const Parfactors&);

   ~ParfactorList();

    const std::list<Parfactor*>& parfactors() const { return pfList_; }

    void clear() { pfList_.clear(); }

    size_t size() const { return pfList_.size(); }

    typedef std::list<Parfactor*>::iterator iterator;

    iterator begin() { return pfList_.begin(); }

    iterator end() { return pfList_.end(); }

    typedef std::list<Parfactor*>::const_iterator const_iterator;

    const_iterator begin() const { return pfList_.begin(); }

    const_iterator end() const { return pfList_.end(); }

    void add (Parfactor* pf);

    void add (const Parfactors& pfs);

    void addShattered (Parfactor* pf);

    std::list<Parfactor*>::iterator insertShattered (
        std::list<Parfactor*>::iterator, Parfactor*);

    std::list<Parfactor*>::iterator remove (
        std::list<Parfactor*>::iterator);

    std::list<Parfactor*>::iterator removeAndDelete (
        std::list<Parfactor*>::iterator);

    bool isAllShattered() const;

    void print() const;

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

    std::list<Parfactor*> pfList_;
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_PARFACTORLIST_H_

