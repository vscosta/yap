#ifndef BP_BP_NETWORK_H
#define BP_BP_NETWORK_H

#include <vector>
#include <string>

#include "BayesianNetwork.h"

using namespace std;

class BpNode;

enum Schedule
{
  SEQUENTIAL_SCHEDULE,
  PARALLEL_SCHEDULE 
};

class BpNetwork : public BayesianNetwork 
{
  public:
    // constructs
    BpNetwork (void);
    // destruct
    ~BpNetwork (void);
    // methods
    void                setSolverParameters (Schedule, int, double);
    void                runSolver (BayesianNode* queryVar);
    void                runSolver (vector<BayesianNode*>);
    void                printCurrentStatus (void);
    void                printCurrentStatusOf (BpNode*);
    void                printBeliefs (void);
    vector<double>      getBeliefs (void);
    vector<double>      getBeliefs (BpNode*);

  private:
    BpNetwork (const BpNetwork&);        // disallow copy
    void operator= (const BpNetwork&);   // disallow assign
    // methods
    void                initializeSolver (vector<BayesianNode*>);
    void                addJunctionNode (vector<BayesianNode*>);
    void                addEvidence (BpNode*);
    void                runNeapolitanSolver (void);
    void                sendLambdaMessage (BpNode*, BpNode*);
    void                sendPiMessage (BpNode*, BpNode*);
    void                updatePiValues (BpNode*);
    void                updatePiMessages (BpNode*, BpNode*);
    void                updateLambdaValues (BpNode*);
    void                updateLambdaMessages (BpNode*, BpNode*);
    void                runIterativeBpSolver (void);
    void                addNode (string, vector<BayesianNode*>, int, int);
    void                addNode (string, vector<BayesianNode*>,
                                 double*, int, vector<string>);
    vector<BpNode*>     cast (vector<BayesianNode*>);
    // members
    Schedule            schedule_;
    int                 nIter_;
    int                 maxIter_;
    double              stableThreashold_;
    BpNode*             queryNode_;
    static const int    DL_ = 3;
    static const int    PRECISION_ = 10;
};

#endif // BP_BP_NETWORK_H

