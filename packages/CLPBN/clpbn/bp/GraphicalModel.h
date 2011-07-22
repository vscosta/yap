#ifndef BP_GRAPHICAL_MODEL_H
#define BP_GRAPHICAL_MODEL_H

#include "Variable.h"
#include "Shared.h"

using namespace std;

class GraphicalModel
{
  public:
    virtual ~GraphicalModel (void) {};
    virtual Variable*  getVariable          (Vid)  const = 0;
    virtual VarSet     getVariables         (void) const = 0;
    virtual void       printGraphicalModel  (void) const = 0;
};

#endif // BP_GRAPHICAL_MODEL_H
