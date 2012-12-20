#ifndef HORUS_WEIGHTEDBP_H
#define HORUS_WEIGHTEDBP_H

#include "BeliefProp.h"

class WeightedLink : public BpLink
{
  public:
    WeightedLink (FacNode* fn, VarNode* vn, size_t idx, unsigned weight)
        : BpLink (fn, vn), index_(idx), weight_(weight),
          pwdMsg_(vn->range(), LogAware::one()) { }

    size_t index (void) const { return index_; }

    unsigned weight (void) const { return weight_; }

    const Params& powMessage (void) const { return pwdMsg_; }

    void updateMessage (void)
    {
      pwdMsg_ = *nextMsg_;
      swap (currMsg_, nextMsg_);
      LogAware::pow (pwdMsg_, weight_);
    }

  private:
    size_t    index_;
    unsigned  weight_;
    Params    pwdMsg_;
};



class WeightedBp : public BeliefProp
{
  public:
    WeightedBp (const FactorGraph& fg,
        const vector<vector<unsigned>>& weights)
      : BeliefProp (fg), weights_(weights) { }

   ~WeightedBp (void);

    Params getPosterioriOf (VarId);

   private:

     void createLinks (void);

     void maxResidualSchedule (void);

     void calcFactorToVarMsg (BpLink*);

     Params getVarToFactorMsg (const BpLink*) const;

     void printLinkInformation (void) const;

     vector<vector<unsigned>> weights_;
};

#endif // HORUS_WEIGHTEDBP_H

