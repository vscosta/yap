#include "CptEntry.h"

CptEntry::CptEntry (int cptIndex, vector<int> instantiations)
{
  cptIndex_       = cptIndex;
  instantiations_ = instantiations;
}



int
CptEntry::getCptIndex (void) const
{
  return cptIndex_;
}



vector<int> 
CptEntry::getDomainInstantiations (void) const
{
  return instantiations_;
}



bool
CptEntry::matchConstraints (const vector<pair<int,int> >& constraints) const
{
  for (unsigned int j = 0; j < constraints.size(); j++) {
    int index = constraints[j].first;
    if (instantiations_[index] != constraints[j].second) {
      return false;
    }
  }
  return true;
}

