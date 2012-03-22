#ifndef HORUS_FACTOR_H
#define HORUS_FACTOR_H

#include <vector>

#include "Distribution.h"
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
    Factor (VarId, unsigned, const Params&);
    Factor (VarNodes&, Distribution*);
    Factor (const VarNodes&, const Params&);
    Factor (const VarIds&, const Ranges&, const Params&);
   ~Factor (void);

    void      setParameters (const Params&);
    void      copyFromFactor (const Factor& f);
    void      multiply (const Factor&);
    void      insertVariable (VarId, unsigned);
    void      insertVariables (const VarIds&, const Ranges&);
    void      sumOutAllExcept (VarId);
    void      sumOutAllExcept (const VarIds&);
    void      sumOut (VarId);
    void      sumOutFirstVariable (void);
    void      sumOutLastVariable (void);
    void      orderVariables (void);
    void      reorderVariables (const VarIds&);
    void      absorveEvidence (VarId, unsigned);
    void      normalize (void);
    bool      contains (const VarIds&) const;
    int       indexOf (VarId) const;
    string    getLabel (void) const;
    void      print (void) const;

    const VarIds&    getVarIds (void) const        { return varids_; }
    const Ranges&    getRanges (void) const        { return ranges_; }
    const Params&    getParameters (void) const    { return dist_->params; }
    Distribution*    getDistribution (void) const  { return dist_; }
    unsigned         nrVariables (void) const      { return varids_.size(); }
    unsigned         nrParameters() const          { return dist_->params.size(); }

    void setDistribution (Distribution* dist)
    { 
      dist_ = dist;
    }

  private:

    VarIds         varids_;
    Ranges         ranges_;
    Distribution*  dist_;
};

#endif // HORUS_FACTOR_H

