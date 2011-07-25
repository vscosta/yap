#ifndef BP_DISTRIBUTION_H
#define BP_DISTRIBUTION_H

#include <vector>

#include "CptEntry.h"
#include "Shared.h"

using namespace std;

struct Distribution
{
  public:
    Distribution (unsigned id, bool shared = false)
    {
      this->id     = id;
      this->params = params;
      this->shared = shared;
    }

    Distribution (const ParamSet& params, bool shared = false)
    {
      this->id     = -1;
      this->params = params;
      this->shared = shared;
    }

    void updateParameters (const ParamSet& params)
    {
      this->params = params;
    }

    unsigned          id;
    ParamSet          params;
    vector<CptEntry>  entries;
    bool              shared;

  private:
    DISALLOW_COPY_AND_ASSIGN (Distribution);
};

#endif //BP_DISTRIBUTION_H

