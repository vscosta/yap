#ifndef BP_GRAPHICALMODEL_H
#define BP_GRAPHICALMODEL_H

#include "Variable.h"
#include "Shared.h"

using namespace std;

class GraphicalModel
{
  public:
    virtual VarSet getVariables (void) const = 0;

  private:
};

#endif
