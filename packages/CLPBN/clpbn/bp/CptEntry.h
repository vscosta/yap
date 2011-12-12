#ifndef HORUS_CPTENTRY_H
#define HORUS_CPTENTRY_H

#include <vector>

#include "Shared.h"

using namespace std;

class CptEntry
{
  public:
    CptEntry (unsigned index, const DConf& conf)
    {
      index_ = index;
      conf_  = conf;
    }

    unsigned getParameterIndex (void) const          { return index_; }
    const DConf& getDomainConfiguration (void) const { return conf_;  }

    bool matchConstraints (const DConstraint& constr) const
    {
      return conf_[constr.first] == constr.second;
    }

    bool matchConstraints (const vector<DConstraint>& constrs) const
    {
      for (unsigned j = 0; j < constrs.size(); j++) {
        if (conf_[constrs[j].first] != constrs[j].second) {
          return false;
        }
      }
      return true;
    }

  private:
    unsigned   index_;
    DConf      conf_;
};

#endif // HORUS_CPTENTRY_H

