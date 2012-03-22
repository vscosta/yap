#ifndef HORUS_PARFACTORLIST_H
#define HORUS_PARFACTORLIST_H

#include <list>

#include "Parfactor.h"
#include "ProbFormula.h"


using namespace std;


class ParfactorList
{
  public:
    ParfactorList (void) { }
    ParfactorList (Parfactors&);
    list<Parfactor*>& getParfactors (void) { return pfList_; }
    const list<Parfactor*>& getParfactors (void) const { return pfList_; }

    void add (Parfactor* pf);
    void add (Parfactors& pfs);
    void addShattered (Parfactor* pf);
    list<Parfactor*>::iterator remove (list<Parfactor*>::iterator);
    list<Parfactor*>::iterator deleteAndRemove (list<Parfactor*>::iterator);

    void     clear (void)       { pfList_.clear();       }
    unsigned size  (void) const { return pfList_.size(); }
    

    void shatter (void);

    typedef std::list<Parfactor*>::iterator iterator;
    iterator begin (void) { return pfList_.begin(); }
    iterator end   (void) { return pfList_.end();   }

    typedef std::list<Parfactor*>::const_iterator const_iterator;
    const_iterator begin (void) const { return pfList_.begin(); }
    const_iterator end   (void) const { return pfList_.end();   }

    void print (void) const;

  private:

    static std::pair<Parfactors, Parfactors> shatter (
                           ProbFormulas&,
                           Parfactor*,
                           ProbFormulas&,
                           Parfactor*);

    static std::pair<Parfactors, Parfactors> shatter (
                           ProbFormula&,
                           Parfactor*,
                           ProbFormula&,
                           Parfactor*);

    static Parfactors   shatter (
                          Parfactor*,
                          const ProbFormula&,
                          ConstraintTree*,
                          ConstraintTree*,
                          unsigned);

    void                 unifyGroups (unsigned group1, unsigned group2);

    list<Parfactor*> pfList_;
};

#endif // HORUS_PARFACTORLIST_H

