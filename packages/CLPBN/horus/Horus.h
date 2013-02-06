#ifndef PACKAGES_CLPBN_HORUS_HORUS_H
#define PACKAGES_CLPBN_HORUS_HORUS_H

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

#define DISALLOW_COPY(TypeName)   \
  TypeName(const TypeName&)

#define DISALLOW_ASSIGN(TypeName) \
  void operator=(const TypeName&)

#include <vector>
#include <string>

class Var;
class Factor;
class VarNode;
class FacNode;

typedef std::vector<double>      Params;
typedef unsigned                 VarId;
typedef std::vector<VarId>       VarIds;
typedef std::vector<Var*>        Vars;
typedef std::vector<VarNode*>    VarNodes;
typedef std::vector<FacNode*>    FacNodes;
typedef std::vector<Factor*>     Factors;
typedef std::vector<std::string> States;
typedef std::vector<unsigned>    Ranges;
typedef unsigned long long       ullong;


enum LiftedSolverType
{
  LVE,  // generalized counting first-order variable elimination (GC-FOVE)
  LBP,  // lifted first-order belief propagation
  LKC   // lifted first-order knowledge compilation
};


enum GroundSolverType
{
  VE,   // variable elimination
  BP,   // belief propagation
  CBP   // counting belief propagation
};


namespace Globals {

extern bool logDomain;

// level of debug information
extern unsigned verbosity;

extern LiftedSolverType liftedSolver;
extern GroundSolverType groundSolver;

};


namespace Constants {

// show message calculation for belief propagation
const bool SHOW_BP_CALCS = false;

const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
const unsigned PRECISION = 6;

};

#endif // PACKAGES_CLPBN_HORUS_HORUS_H

