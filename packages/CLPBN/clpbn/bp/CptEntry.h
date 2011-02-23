#ifndef CPT_ENTRY_H
#define CPT_ENTRY_H

#include <vector>

using namespace std;

class CptEntry
{
  public:
    // constructs
    CptEntry (int, vector<int>);
    // methods
    int                getCptIndex (void) const;
    vector<int>        getDomainInstantiations (void) const;
    bool               matchConstraints (const vector<pair<int,int> >&) const;
  private:
    // members
    int                cptIndex_;
    vector<int>        instantiations_;
};

#endif // CPT_ENTRY_H
