#ifndef BP_FG_VAR_NODE_H
#define BP_FG_VAR_NODE_H

#include <vector>

#include "Variable.h"
#include "Shared.h"

using namespace std;

class Factor;

class FgVarNode : public Variable
{
  public:
    FgVarNode (unsigned vid, unsigned dsize) : Variable (vid, dsize) { }
    FgVarNode (const Variable* v) : Variable (v) { }

    void           addFactor (Factor* f)      { factors_.push_back (f); }
    CFactorSet     getFactors (void) const    { return factors_; }

    void removeFactor (const Factor* f)
    { 
      if (factors_[factors_.size() -1] == f) {
        factors_.pop_back();
      } else {
        for (unsigned i = 0; i  < factors_.size(); i++) {
          if (factors_[i] == f) {
            factors_.erase (factors_.begin() + i);
            return;
          }
        }
        assert (false);
      }
    }

  private:
    DISALLOW_COPY_AND_ASSIGN (FgVarNode);
    // members
    FactorSet           factors_;
};

#endif // BP_FG_VAR_NODE_H
