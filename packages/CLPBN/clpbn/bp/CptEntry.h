#ifndef BP_CPTENTRY_H
#define BP_CPTENTRY_H

#include <vector>

#include "Shared.h"

using namespace std;

class CptEntry
{
  public:
    CptEntry (unsigned, const vector<unsigned>&);

    unsigned                getParameterIndex (void) const;
    const vector<unsigned>& getParentConfigurations (void) const;
    bool                    matchConstraints (const DomainConstr&) const;
    bool                    matchConstraints (const vector<DomainConstr>&) const;

  private:
    unsigned                index_;
    vector<unsigned>        confs_;
};



inline
CptEntry::CptEntry (unsigned index, const vector<unsigned>& confs)
{
  index_ = index;
  confs_ = confs;
}



inline unsigned
CptEntry::getParameterIndex (void) const
{
  return index_;
}



inline const vector<unsigned>&
CptEntry::getParentConfigurations (void) const
{
  return confs_;
}



inline bool
CptEntry::matchConstraints (const DomainConstr& constr) const
{
  return confs_[constr.first] == constr.second;
}



inline bool
CptEntry::matchConstraints (const vector<DomainConstr>& constrs) const
{
  for (unsigned j = 0; j < constrs.size(); j++) {
    if (confs_[constrs[j].first] != constrs[j].second) {
      return false;
    }
  }
  return true;
}

#endif
