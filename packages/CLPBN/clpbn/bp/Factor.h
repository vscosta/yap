#ifndef BP_FACTOR_H
#define BP_FACTOR_H

#include <vector>

#include "CptEntry.h"

using namespace std;

class FgVarNode;

class Factor
{
  public:
    Factor (FgVarNode*);
    Factor (const FgVarSet&);
    Factor (FgVarNode*, const ParamSet&);
    Factor (const FgVarSet&, const ParamSet&);

    const FgVarSet&            getFgVarNodes (void) const;
    FgVarSet&                  getFgVarNodes (void);
    const ParamSet&            getParameters (void) const;
    ParamSet&                  getParameters (void);
    void                       setParameters (const ParamSet&);
    Factor&                    operator= (const Factor& f);
    Factor&                    operator*= (const Factor& f);
    void                       insertVariable (FgVarNode* index);
    void                       marginalizeVariable (const FgVarNode* var);
    void                       marginalizeVariable (unsigned);
    string                     getLabel (void) const;
    string                     toString (void) const;

  private:
    vector<CptEntry>           getCptEntries() const;
    int                        getIndexOf (const FgVarNode*) const;

    FgVarSet                   vs_;
    ParamSet                   ps_;
    int                        id_;
    static int                 indexCount_;
};

Factor operator* (const Factor&, const Factor&);

#endif
