#ifndef HORUS_CBP_H
#define HORUS_CBP_H

#include "BpSolver.h"
#include "CFactorGraph.h"

class Factor;

class CbpSolverLink : public SpLink
{
  public:
    CbpSolverLink (FacNode* fn, VarNode* vn, unsigned c) : SpLink (fn, vn)
    {
      edgeCount_ = c;
      poweredMsg_.resize (vn->range(), LogAware::one());
    }

    unsigned getNumberOfEdges  (void) const { return edgeCount_; }

    const Params& getPoweredMessage (void) const { return poweredMsg_; }

    void updateMessage (void) 
    {
      poweredMsg_ = *nextMsg_;
      swap (currMsg_, nextMsg_);
      msgSended_  = true;
      LogAware::pow (poweredMsg_, edgeCount_);
    }
  
  private:
    Params    poweredMsg_;
    unsigned  edgeCount_;
};



class CbpSolver : public BpSolver
{
  public:
    CbpSolver (const FactorGraph& fg) : BpSolver (fg) { }

   ~CbpSolver (void);

    Params getPosterioriOf (VarId);

    Params getJointDistributionOf (const VarIds&);

   private:
     void initializeSolver (void);

     void createLinks (void);

     void maxResidualSchedule (void);

     Params getVar2FactorMsg (const SpLink*) const;

     void printLinkInformation (void) const;

     CFactorGraph* lfg_;
     FactorGraph*  factorGraph_;
};

#endif // HORUS_CBP_H

