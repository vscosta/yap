#ifndef BP_COUNTING_BP_H
#define BP_COUNTING_BP_H

#include "SPSolver.h"
#include "LiftedFG.h"

class Factor;
class FgVarNode;

class CountingBPLink : public Link
{
  public:
    CountingBPLink (Factor* f, FgVarNode* v, unsigned c) : Link (f, v)
    {
      edgeCount_ = c;
    }
  
    unsigned getNumberOfEdges (void) const { return edgeCount_; }

  private:
    unsigned edgeCount_;
};


class CountingBP : public SPSolver
{
  public:
    CountingBP (FactorGraph& fg) : SPSolver (fg) { }
   ~CountingBP (void);

    ParamSet        getPosterioriOf (Vid) const;

   private:
     void           initializeSolver (void);
     void           createLinks (void);
     void           deleteJunction (Factor*, FgVarNode*);

     void           maxResidualSchedule (void);
     ParamSet       getVar2FactorMsg (const Link*) const;

     LiftedFG*      lfg_;
};

#endif // BP_COUNTING_BP_H

