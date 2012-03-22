#ifndef HORUS_FGBPSOLVER_H
#define HORUS_FGBPSOLVER_H

#include <set>
#include <vector>
#include <sstream>

#include "Solver.h"
#include "Factor.h"
#include "FactorGraph.h"
#include "Util.h"

using namespace std;



class SpLink
{
  public:
    SpLink (FgFacNode* fn, FgVarNode* vn)
    { 
      fac_ = fn;
      var_ = vn;
      v1_.resize (vn->nrStates(), Util::tl (1.0 / vn->nrStates()));
      v2_.resize (vn->nrStates(), Util::tl (1.0 / vn->nrStates()));
      currMsg_   = &v1_;
      nextMsg_   = &v2_;
      msgSended_ = false;
      residual_  = 0.0;
    }

    virtual ~SpLink (void) {};

    virtual void updateMessage (void) 
    {
      swap (currMsg_, nextMsg_);
      msgSended_ = true;
    }

    void updateResidual (void)
    {
      residual_ = Util::getMaxNorm (v1_, v2_);
    }

    string toString (void) const
    {
      stringstream ss;
      ss << fac_->getLabel();
      ss << " -- " ;
      ss << var_->label();
      return ss.str();
    }

    FgFacNode*       getFactor (void) const         { return fac_; }
    FgVarNode*       getVariable (void) const       { return var_; }
    const Params&    getMessage (void) const        { return *currMsg_; }
    Params&          getNextMessage (void)          { return *nextMsg_; }
    bool             messageWasSended (void) const  { return msgSended_; }
    double           getResidual (void) const       { return residual_; }
    void             clearResidual (void)           { residual_ = 0.0; }
 
  protected:
    FgFacNode*    fac_;
    FgVarNode*    var_;
    Params        v1_;
    Params        v2_;
    Params*       currMsg_;
    Params*       nextMsg_;
    bool          msgSended_;
    double        residual_;
};


typedef vector<SpLink*> SpLinkSet;


class SPNodeInfo
{
  public:
    void              addSpLink (SpLink* link)    { links_.push_back (link); }
    const SpLinkSet&  getLinks (void)             { return links_; }

  private:
    SpLinkSet         links_;
};


class FgBpSolver : public Solver
{
  public:
    FgBpSolver (const FactorGraph&);
    virtual ~FgBpSolver (void);

    void              runSolver (void);
    virtual Params    getPosterioriOf (VarId);
    virtual Params    getJointDistributionOf (const VarIds&);
 
  protected:
    virtual void      initializeSolver (void);
    virtual void      createLinks (void);
    virtual void      maxResidualSchedule (void);
    virtual void      calculateFactor2VariableMsg (SpLink*) const;
    virtual Params    getVar2FactorMsg (const SpLink*) const;
    virtual Params    getJointByConditioning (const VarIds&) const;
    virtual void      printLinkInformation (void) const;

    void calculateAndUpdateMessage (SpLink* link, bool calcResidual = true)
    {
      if (DL >= 3) {
        cout << "calculating & updating " << link->toString() << endl;
      }
      calculateFactor2VariableMsg (link);
      if (calcResidual) {
        link->updateResidual();
      }
      link->updateMessage();
    }

    void calculateMessage (SpLink* link, bool calcResidual = true)
    {
      if (DL >= 3) {
        cout << "calculating " << link->toString() << endl;
      }
      calculateFactor2VariableMsg (link);
      if (calcResidual) {
        link->updateResidual();
      }
    }

    void updateMessage (SpLink* link)
    {
      link->updateMessage();
      if (DL >= 3) {
        cout << "updating " << link->toString() << endl;
      }
    }

    SPNodeInfo* ninf (const FgVarNode* var) const
    {
      return varsI_[var->getIndex()];
    }

    SPNodeInfo* ninf (const FgFacNode* fac) const
    {
      return facsI_[fac->getIndex()];
    }

    struct CompareResidual {
      inline bool operator() (const SpLink* link1, const SpLink* link2)
      {
        return link1->getResidual() > link2->getResidual();
      }
    };

    SpLinkSet            links_;
    unsigned             nIters_;
    vector<SPNodeInfo*>  varsI_;
    vector<SPNodeInfo*>  facsI_;
    const FactorGraph*   factorGraph_;

    typedef multiset<SpLink*, CompareResidual> SortedOrder;
    SortedOrder sortedOrder_;

    typedef unordered_map<SpLink*, SortedOrder::iterator> SpLinkMap;
    SpLinkMap linkMap_;

  private:
    void              runLoopySolver (void);
    bool              converged (void);


};

#endif // HORUS_FGBPSOLVER_H

