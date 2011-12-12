#ifndef HORUS_FACTOR_H
#define HORUS_FACTOR_H

#include <vector>

#include "Distribution.h"
#include "CptEntry.h"
#include "VarNode.h"


using namespace std;

class Distribution;


class Factor
{
  public:
    Factor (void) { }
    Factor (const Factor&);
    Factor (VarId, unsigned);
    Factor (const VarNodes&);
    Factor (VarId, unsigned, const ParamSet&);
    Factor (VarNodes&, Distribution*);
    Factor (const VarNodes&, const ParamSet&);
    Factor (const VarIdSet&, const Ranges&, const ParamSet&);

    void      setParameters (const ParamSet&);
    void      copyFromFactor (const Factor& f);
    void      multiplyByFactor (const Factor&, const vector<CptEntry>* = 0);
    void      insertVariable (VarId, unsigned);
    void      removeAllVariablesExcept (VarId);
    void      removeVariable (VarId);
    void      removeFirstVariable (void);
    void      removeLastVariable (void);
    void      orderVariables (void);
    void      orderVariables (const VarIdSet&);
    void      removeInconsistentEntries (VarId, unsigned);
    string    getLabel (void) const;
    void      printFactor (void) const;
    int       getPositionOf (VarId) const;
    const vector<CptEntry>& getCptEntries (void) const;

    const VarIdSet&  getVarIds (void) const        { return varids_; }
    const Ranges&    getRanges (void) const        { return ranges_; }
    const ParamSet&  getParameters (void) const    { return dist_->params; }
    Distribution*    getDistribution (void) const  { return dist_; }
    unsigned     nrVariables (void) const     { return varids_.size(); }
    unsigned     nrParameters() const         { return dist_->params.size(); }

    void setDistribution (Distribution* dist)
    { 
      dist_ = dist;
    }
    void freeDistribution (void) 
    {
      delete dist_;
      dist_ = 0;
    }

  private:

    VarIdSet         varids_;
    Ranges         ranges_;
    Distribution*  dist_;
};

#endif // HORUS_FACTOR_H

