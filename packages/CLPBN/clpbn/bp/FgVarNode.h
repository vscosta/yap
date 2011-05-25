#ifndef BP_VARIABLE_H
#define BP_VARIABLE_H

#include <vector>
#include <string>

#include "Variable.h"
#include "Shared.h"

using namespace std;

class Factor;

class FgVarNode : public Variable
{
  public:
    FgVarNode (int varId, int dsize) : Variable (varId, dsize) { }

    void                addFactor (Factor* f)      { factors_.push_back (f); }
    vector<Factor*>     getFactors (void) const    { return factors_; }

  private:
    DISALLOW_COPY_AND_ASSIGN (FgVarNode);
    // members
    vector<Factor*>     factors_;
};

#endif // BP_VARIABLE_H
