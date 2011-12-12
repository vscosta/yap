#ifndef HORUS_DISTRIBUTION_H
#define HORUS_DISTRIBUTION_H

#include <vector>

#include "CptEntry.h"
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

    Distribution (const ParamSet& params, unsigned id = -1)
    {
      this->id     = id;
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

#endif // HORUS_DISTRIBUTION_H

