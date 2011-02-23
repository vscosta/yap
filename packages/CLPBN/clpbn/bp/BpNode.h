#ifndef BP_BP_NODE_H
#define BP_BP_NODE_H

#include <vector>
#include <map>
#include <deque>
#include <string>

#include "BayesianNode.h"

using namespace std;

class BpNode : public BayesianNode 
{
  public:
    // constructs
    BpNode (string, vector<BayesianNode*>, Distribution* dist, int = -1);
    // destruct
    ~BpNode (void);
    // methods
    static void                enableParallelSchedule (void);
    void                       allocateMemory (void);
    double*                    getPiValues (void) const;
    double                     getPiValue (int) const;
    void                       setPiValue (int, double);
    double*                    getLambdaValues (void) const;
    double                     getLambdaValue (int) const;
    void                       setLambdaValue (int, double);
    double*                    getPiMessages (BpNode*) const;
    double                     getPiMessage (BpNode*, int) const;
    void                       setPiMessage (BpNode*, int, double);
    double*                    getLambdaMessages (BpNode*) const;
    double                     getLambdaMessage (BpNode*, int) const;
    void                       setLambdaMessage (BpNode*, int, double);
    double*                    getBeliefs (void);
    double                     getBeliefChange (void);
    void                       normalizeMessages (void);
    void                       swapMessages (void);
 
  private:
    BpNode (const BpNode&);          // disallow copy
    void operator= (const BpNode&);  // disallow assign
    // members
    double*                    lambdaValues_;
    double*                    piValues_;
    map<BpNode*, double*>      piMessages_;
    map<BpNode*, double*>      lambdaMessages_;
    map<BpNode*, double*>*     newPiMessages_;
    map<BpNode*, double*>*     newLambdaMessages_;
    double*                    oldBeliefs_;
    static bool                parallelSchedule_;
    static const double        MAX_CHANGE_ = 1.0;
};

#endif // BP_BP_NODE_H

