#ifndef HORUS_CBP_H
#define HORUS_CBP_H

#include "FgBpSolver.h"
#include "CFactorGraph.h"

class Factor;

class CbpSolverLink : public SpLink
{
  public:
    CbpSolverLink (FgFacNode* fn, FgVarNode* vn, unsigned c) : SpLink (fn, vn)
    {
      edgeCount_ = c;
      poweredMsg_.resize (vn->nrStates(), Util::one());
    }

    void updateMessage (void) 
    {
      poweredMsg_ = *nextMsg_;
      swap (currMsg_, nextMsg_);
      msgSended_  = true;
      Util::pow (poweredMsg_, edgeCount_);
    }
  
    unsigned         getNumberOfEdges  (void) const { return edgeCount_; }
    const ParamSet&  getPoweredMessage (void) const { return poweredMsg_; }

  private:
    ParamSet poweredMsg_;
    unsigned edgeCount_;
};



class CbpSolver : public FgBpSolver
{
  public:
    CbpSolver (FactorGraph& fg) : FgBpSolver (fg) { }
   ~CbpSolver (void);

    ParamSet        getPosterioriOf (VarId);
    ParamSet        getJointDistributionOf (const VarIdSet&);

   private:
     void           initializeSolver (void);
     void           createLinks (void);

     void           maxResidualSchedule (void);
     ParamSet       getVar2FactorMsg (const SpLink*) const;
     void           printLinkInformation (void) const;


     CFactorGraph*      lfg_;
};

#endif // HORUS_CBP_H

