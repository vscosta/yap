#ifndef HORUS_HORUS_H
#define HORUS_HORUS_H

#include <limits>

#include <vector>

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class Var;
class Factor;
class VarNode;
class FacNode;

typedef vector<double>      Params;
typedef unsigned            VarId;
typedef vector<VarId>       VarIds;
typedef vector<Var*>        Vars;
typedef vector<VarNode*>    VarNodes;
typedef vector<FacNode*>    FacNodes;
typedef vector<Factor*>     Factors;
typedef vector<string>      States;
typedef vector<unsigned>    Ranges;
typedef unsigned long long  ullong;


enum LiftedSolver
{
  FOVE, // first order variable elimination
  LBP,  // lifted belief propagation
};


enum GroundSolver
{
  VE,   // variable elimination
  BP,   // belief propagation
  CBP   // counting belief propagation
};


namespace Globals {

extern bool logDomain;

// level of debug information
extern unsigned verbosity;

extern LiftedSolver liftedSolver;
extern GroundSolver groundSolver;

};


namespace Constants {

// show message calculation for belief propagation
const bool SHOW_BP_CALCS = false;

const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
const unsigned PRECISION = 6;

};


namespace BpOptions
{
  enum Schedule {
    SEQ_FIXED,
    SEQ_RANDOM,
    PARALLEL,
    MAX_RESIDUAL
  };
  extern Schedule  schedule;
  extern double    accuracy;
  extern unsigned  maxIter;
}

#endif // HORUS_HORUS_H

