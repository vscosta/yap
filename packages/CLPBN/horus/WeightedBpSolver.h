#ifndef HORUS_WEIGHTEDBPSOLVER_H
#define HORUS_WEIGHTEDBPSOLVER_H

#include "BpSolver.h"

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



class WeightedBpSolver : public BpSolver
{
  public:   
    WeightedBpSolver (const FactorGraph& fg,
        const vector<vector<unsigned>>& weights)
      : BpSolver (fg), weights_(weights) { }

   ~WeightedBpSolver (void);
  
    Params getPosterioriOf (VarId);

   private:

     void createLinks (void);

     void maxResidualSchedule (void);

     void calcFactorToVarMsg (BpLink*);

     Params getVarToFactorMsg (const BpLink*) const;

     void printLinkInformation (void) const;
    
     vector<vector<unsigned>> weights_;
};

#endif // HORUS_WEIGHTEDBPSOLVER_H

