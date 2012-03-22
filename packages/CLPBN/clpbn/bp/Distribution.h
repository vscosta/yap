#ifndef HORUS_DISTRIBUTION_H
#define HORUS_DISTRIBUTION_H

#include <vector>

#include "Horus.h"

//TODO die die die die die 

using namespace std;


struct Distribution
{
  public:
    Distribution (int id)
    {
      this->id = id;
    }

    Distribution (const Params& params, int id = -1)
    {
      this->id     = id;
      this->params = params;
    }

    void updateParameters (const Params& params)
    {
      this->params = params;
    }

    bool shared (void)
    {
      return id != -1;
    }

    int               id;
    Params          params;

  private:
    DISALLOW_COPY_AND_ASSIGN (Distribution);
};

#endif // HORUS_DISTRIBUTION_H

