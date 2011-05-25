#ifndef BP_DISTRIBUTION_H
#define BP_DISTRIBUTION_H

#include <vector>
#include <string>

#include "Shared.h"

using namespace std;

struct Distribution
{
  public:
    Distribution (unsigned id)
    {
      this->id     = id;
      this->params = params;
    }

    Distribution (const ParamSet& params)
    {
      this->id     = -1;
      this->params = params;
    }

    void updateParameters (const ParamSet& params)
    {
      this->params = params;
    }

    unsigned          id;
    ParamSet          params;
    vector<CptEntry>  entries;

  private:
    DISALLOW_COPY_AND_ASSIGN (Distribution);
};

#endif

