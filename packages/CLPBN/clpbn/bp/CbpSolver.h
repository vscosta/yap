#ifndef HORUS_CBP_H
#define HORUS_CBP_H

#include "BpSolver.h"
#include "CFactorGraph.h"

class Factor;

class CbpSolverLink : public SpLink
{
  public:
    CbpSolverLink (FacNode* fn, VarNode* vn, unsigned c) 
        : SpLink (fn, vn), nrEdges_(c),
          pwdMsg_(vn->range(), LogAware::one()) { }

    unsigned nrEdges  (void) const { return nrEdges_; }

    const Params& poweredMessage (void) const { return pwdMsg_; }

    void updateMessage (void) 
    {
      pwdMsg_ = *nextMsg_;
      swap (currMsg_, nextMsg_);
      msgSended_ = true;
      LogAware::pow (pwdMsg_, nrEdges_);
    }
  
  private:
    unsigned  nrEdges_;
    Params    pwdMsg_;
};



class CbpSolver : public BpSolver
{
  public:
    CbpSolver (const FactorGraph& fg);

   ~CbpSolver (void);

    void printSolverFlags (void) const;
  
    Params getPosterioriOf (VarId);

    Params getJointDistributionOf (const VarIds&);

   private:

     void createLinks (void);

     void maxResidualSchedule (void);

     Params getVar2FactorMsg (const SpLink*) const;

     void printLinkInformation (void) const;

     CFactorGraph* cfg_;
};

#endif // HORUS_CBP_H

