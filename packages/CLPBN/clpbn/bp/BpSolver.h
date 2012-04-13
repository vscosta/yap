#ifndef HORUS_BPSOLVER_H
#define HORUS_BPSOLVER_H

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
    SpLink (FacNode* fn, VarNode* vn)
    { 
      fac_ = fn;
      var_ = vn;
      v1_.resize (vn->range(), LogAware::tl (1.0 / vn->range()));
      v2_.resize (vn->range(), LogAware::tl (1.0 / vn->range()));
      currMsg_   = &v1_;
      nextMsg_   = &v2_;
      msgSended_ = false;
      residual_  = 0.0;
    }

    virtual ~SpLink (void) { };

    FacNode* getFactor (void) const { return fac_; }

    VarNode* getVariable (void) const { return var_; }

    const Params& getMessage (void) const { return *currMsg_; }

    Params& getNextMessage (void) { return *nextMsg_; }

    bool messageWasSended (void) const { return msgSended_; }

    double getResidual (void) const { return residual_; }

    void clearResidual (void) { residual_ = 0.0; }

    void updateResidual (void)
    {
      residual_ = LogAware::getMaxNorm (v1_,v2_);
    }

    virtual void updateMessage (void) 
    {
      swap (currMsg_, nextMsg_);
      msgSended_ = true;
    }

    string toString (void) const
    {
      stringstream ss;
      ss << fac_->getLabel();
      ss << " -- " ;
      ss << var_->label();
      return ss.str();
    }
 
  protected:
    FacNode*  fac_;
    VarNode*  var_;
    Params    v1_;
    Params    v2_;
    Params*   currMsg_;
    Params*   nextMsg_;
    bool      msgSended_;
    double    residual_;
};

typedef vector<SpLink*> SpLinkSet;


class SPNodeInfo
{
  public:
    void addSpLink (SpLink* link) { links_.push_back (link); }
    const SpLinkSet& getLinks (void) { return links_; }
  private:
    SpLinkSet links_;
};


class BpSolver : public Solver
{
  public:
    BpSolver (const FactorGraph&);

    virtual ~BpSolver (void);

    Params solveQuery (VarIds);

    virtual void printSolverFlags (void) const;

    virtual Params getPosterioriOf (VarId);

    virtual Params getJointDistributionOf (const VarIds&);
 
  protected:
    void runSolver (void);

    virtual void createLinks (void);

    virtual void maxResidualSchedule (void);

    virtual void calculateFactor2VariableMsg (SpLink*);

    virtual Params getVar2FactorMsg (const SpLink*) const;

    virtual Params getJointByConditioning (const VarIds&) const;

    SPNodeInfo* ninf (const VarNode* var) const
    {
      return varsI_[var->getIndex()];
    }

    SPNodeInfo* ninf (const FacNode* fac) const
    {
      return facsI_[fac->getIndex()];
    }

    void calculateAndUpdateMessage (SpLink* link, bool calcResidual = true)
    {
      if (Constants::DEBUG >= 3) {
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
      if (Constants::DEBUG >= 3) {
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
      if (Constants::DEBUG >= 3) {
        cout << "updating " << link->toString() << endl;
      }
    }

    struct CompareResidual
    {
      inline bool operator() (const SpLink* link1, const SpLink* link2)
      {
        return link1->getResidual() > link2->getResidual();
      }
    };

    SpLinkSet            links_;
    unsigned             nIters_;
    vector<SPNodeInfo*>  varsI_;
    vector<SPNodeInfo*>  facsI_;
    bool                 runned_;
    const FactorGraph*   fg_;

    typedef multiset<SpLink*, CompareResidual> SortedOrder;
    SortedOrder sortedOrder_;

    typedef unordered_map<SpLink*, SortedOrder::iterator> SpLinkMap;
    SpLinkMap linkMap_;

  private:
    void initializeSolver (void);

    bool converged (void);

    void printLinkInformation (void) const;
};

#endif // HORUS_BPSOLVER_H

