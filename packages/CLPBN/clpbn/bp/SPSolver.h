#ifndef BP_SP_SOLVER_H
#define BP_SP_SOLVER_H

#include <vector>
#include <set>

#include "Solver.h"
#include "FgVarNode.h"
#include "Factor.h"

using namespace std;

class FactorGraph;
class SPSolver;


class Link
{
  public:
    Link (Factor* f, FgVarNode* v)
    { 
      factor_ = f;
      var_    = v;
      currMsg_.resize (v->getDomainSize(), 1);
      nextMsg_.resize (v->getDomainSize(), 1);
      msgSended_ = false;
      residual_ = 0.0;
    }
  
    void setMessage (ParamSet msg)
    {
      Util::normalize (msg);
      residual_ = Util::getMaxNorm (currMsg_, msg);
      currMsg_ = msg;
    }

    void setNextMessage (CParamSet msg)
    {
      nextMsg_ = msg;
      Util::normalize (nextMsg_);
      residual_ = Util::getMaxNorm (currMsg_, nextMsg_);
    }

    void updateMessage (void) 
    {
      currMsg_ = nextMsg_;
      msgSended_ = true;
    }

    string toString (void) const
    {
      stringstream ss;
      ss << factor_->getLabel();
      ss << " -- " ;
      ss << var_->getLabel();
      return ss.str();
    }

    Factor*      getFactor (void) const        { return factor_; }
    FgVarNode*   getVariable (void) const      { return var_; }
    CParamSet    getMessage (void) const       { return currMsg_; }
    bool         messageWasSended (void) const { return msgSended_; }
    double       getResidual (void) const      { return residual_; }
    void         clearResidual (void)          { residual_ = 0.0; }
 
  private:
    Factor*      factor_;
    FgVarNode*   var_;
    ParamSet     currMsg_;
    ParamSet     nextMsg_;
    bool         msgSended_;
    double       residual_;
};


class SPNodeInfo
{
  public:
    void         addLink (Link* link)         { links_.push_back (link); }
    CLinkSet     getLinks (void)              { return links_; }

  private:
    LinkSet      links_;
};


class SPSolver : public Solver
{
  public:
    SPSolver (FactorGraph&);
    virtual ~SPSolver (void);

    void              runSolver (void);
    virtual ParamSet  getPosterioriOf (Vid) const;
    ParamSet          getJointDistributionOf (CVidSet);
 
  protected:
    virtual void      initializeSolver (void);
    void              runTreeSolver (void);
    bool              readyToSendMessage (const Link*) const;
    virtual void      createLinks (void);
    virtual void      deleteJunction (Factor*, FgVarNode*);
    bool              converged (void);
    virtual void      maxResidualSchedule (void);
    virtual ParamSet  getFactor2VarMsg (const Link*) const;
    virtual ParamSet  getVar2FactorMsg (const Link*) const;

    struct CompareResidual {
      inline bool operator() (const Link* link1, const Link* link2)
      {
        return link1->getResidual() > link2->getResidual();
      }
    };

    FactorGraph*         fg_;
    LinkSet              links_;
    vector<SPNodeInfo*>  varsI_;
    vector<SPNodeInfo*>  factorsI_;
    unsigned             nIter_;

    typedef multiset<Link*, CompareResidual> SortedOrder;
    SortedOrder sortedOrder_;

    typedef map<Link*, SortedOrder::iterator> LinkMap;
    LinkMap linkMap_;

};

#endif // BP_SP_SOLVER_H

