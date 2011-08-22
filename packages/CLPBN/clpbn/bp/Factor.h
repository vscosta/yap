#ifndef BP_FACTOR_H
#define BP_FACTOR_H

#include <vector>

#include "Distribution.h"
#include "CptEntry.h"

using namespace std;

class FgVarNode;
class Distribution;

class Factor
{
  public:
    Factor (void) { }
    Factor (const Factor&);
    Factor (FgVarNode*);
    Factor (CFgVarSet);
    Factor (FgVarNode*, const ParamSet&);
    Factor (FgVarSet&, Distribution*);
    Factor (CFgVarSet, CParamSet);

    void      setParameters (CParamSet);
    void      copyFactor (const Factor& f);
    void      multiplyByFactor (const Factor& f, const vector<CptEntry>* = 0);
    void      insertVariable (FgVarNode* index);
    void      removeVariable (const FgVarNode* var);
    const vector<CptEntry>& getCptEntries (void) const;
    string    getLabel (void) const;
    void      printFactor (void);

    CFgVarSet getFgVarNodes (void) const        { return vars_; }
    CParamSet getParameters (void) const        { return dist_->params; }
    Distribution* getDistribution (void) const  { return dist_; }
    unsigned getIndex (void) const              { return index_; }
    void setIndex (unsigned index)              { index_ = index; }
    void freeDistribution (void)                { delete dist_; dist_ = 0;}
    int                       getIndexOf (const FgVarNode*) const;

  private:
    FgVarSet                  vars_;
    Distribution*             dist_;
    unsigned                  index_;
};

#endif //BP_FACTOR_H
