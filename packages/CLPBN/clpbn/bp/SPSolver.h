#ifndef BP_SPSOLVER_H
#define BP_SPSOLVER_H

#include <cmath>
#include <map>
#include <vector>
#include <string>

#include "Solver.h"
#include "FgVarNode.h"
#include "Factor.h"

using namespace std;

class FactorGraph;
class SPSolver;

struct Link
{
  Link (Factor* s, FgVarNode* d)
  { 
    source      = s;
    destination = d;
  }
  string toString (void) const
  {
    stringstream ss;
    ss << source->getLabel() << " --> " ;
    ss << destination->getLabel();
    return ss.str();
  }
  Factor*           source;
  FgVarNode*        destination;
  static SPSolver*  klass;
};



class MessageBanket
{
  public:
    MessageBanket (const FgVarNode* var)
    {
      vector<Factor*> sources = var->getFactors();
      for (unsigned i = 0; i < sources.size(); i++) {
        indexMap_.insert (make_pair (sources[i], i));
        currMsgs_.push_back (Message(var->getDomainSize(), 1));
        nextMsgs_.push_back (Message(var->getDomainSize(), -10));
        residuals_.push_back (0.0);
      }
    }

    void updateMessage (const Factor* source)
    {
      unsigned idx = getIndex(source);
      currMsgs_[idx] = nextMsgs_[idx];
    }

    void setNextMessage (const Factor* source, const Message& msg)
    {
      unsigned idx = getIndex(source);
      nextMsgs_[idx] = msg;
      residuals_[idx] = computeResidual (source);
    }

    const Message& getMessage (const Factor* source) const
    {
      return currMsgs_[getIndex(source)];
    }

    double getResidual (const Factor* source) const
    {
      return residuals_[getIndex(source)];
    }
    
    void resetResidual (const Factor* source)
    {
      residuals_[getIndex(source)] = 0.0;
    }

  private:
    double computeResidual (const Factor* source)
    {
      double change = 0.0;
      unsigned idx  = getIndex (source);
      const Message& currMessage = currMsgs_[idx];
      const Message& nextMessage = nextMsgs_[idx];
      for (unsigned i = 0; i < currMessage.size(); i++) {
        change += abs (currMessage[i] - nextMessage[i]);
      }
      return change;
    }
 
    unsigned getIndex (const Factor* factor) const
    {
      assert (factor);
      assert (indexMap_.find(factor) != indexMap_.end());
      return indexMap_.find(factor)->second;
    }
 
    typedef map<const Factor*, unsigned> IndexMap;

    IndexMap               indexMap_;
    vector<Message>        currMsgs_;
    vector<Message>        nextMsgs_;
    vector<double>         residuals_;
};



class SPSolver : public Solver
{
  public:
    SPSolver (const FactorGraph&);
   ~SPSolver (void);

    void            runSolver (void);
    ParamSet        getPosterioriOf (const Variable* var) const;

  private:
    bool            converged (void);
    void            maxResidualSchedule (void);
    void            updateMessage (const Link&);
    void            updateMessage (const Factor*, const FgVarNode*);
    void            calculateNextMessage (const Link&);
    void            calculateNextMessage (const Factor*, const FgVarNode*);
    void            calculateVarFactorMessage (
                         const FgVarNode*, const Factor*, Message&) const;
    double          getResidual (const Link&) const;
    void            resetResidual (const Link&) const;
    friend bool     compareResidual (const Link&, const Link&);

    const FactorGraph*          fg_;
    vector<MessageBanket*>      msgs_;
    Schedule                    schedule_;
    int                         nIter_;
    double                      accuracy_;
    int                         maxIter_;
    vector<Link>                updateOrder_;
};



inline double
SPSolver::getResidual (const Link& link) const
{
  MessageBanket* mb = Link::klass->msgs_[link.destination->getIndex()];
  return mb->getResidual (link.source);
}



inline void
SPSolver::resetResidual (const Link& link) const
{
  MessageBanket* mb = Link::klass->msgs_[link.destination->getIndex()];
  mb->resetResidual (link.source);
}



inline bool
compareResidual (const Link& link1, const Link& link2)
{
  MessageBanket* mb1 = Link::klass->msgs_[link1.destination->getIndex()];
  MessageBanket* mb2 = Link::klass->msgs_[link2.destination->getIndex()];
  return mb1->getResidual(link1.source) > mb2->getResidual(link2.source);
}

#endif

