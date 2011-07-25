#ifndef BP_FACTORGRAPH_H
#define BP_FACTORGRAPH_H

#include <vector>
#include <string>

#include "GraphicalModel.h"
#include "Shared.h"

using namespace std;

class FgVarNode;
class Factor;

class FactorGraph : public GraphicalModel
{
  public:
    FactorGraph (const char* fileName);
   ~FactorGraph (void);

    FgVarSet            getFgVarNodes (void) const;
    vector<Factor*>     getFactors (void) const;
    VarSet              getVariables (void) const;
    FgVarNode*          getVariableById (unsigned) const;
    FgVarNode*          getVariableByLabel (string) const;
    void                printFactorGraph (void) const;

  private:
    DISALLOW_COPY_AND_ASSIGN (FactorGraph);

    FgVarSet varNodes_;
    vector<Factor*>     factors_;
};

#endif
