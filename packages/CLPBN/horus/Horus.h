#ifndef YAP_PACKAGES_CLPBN_HORUS_HORUS_H_
#define YAP_PACKAGES_CLPBN_HORUS_HORUS_H_

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

#define DISALLOW_COPY(TypeName)   \
  TypeName(const TypeName&)

#define DISALLOW_ASSIGN(TypeName) \
  void operator=(const TypeName&)

#include <vector>
#include <string>


namespace Horus {

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


enum class LiftedSolverType {
  lveSolver,  // generalized counting first-order variable elimination
  lbpSolver,  // lifted first-order belief propagation
  lkcSolver   // lifted first-order knowledge compilation
};


enum class GroundSolverType {
  veSolver,   // variable elimination
  bpSolver,   // belief propagation
  CbpSolver   // counting belief propagation
};


namespace Globals {

extern bool logDomain;

// level of debug information
extern unsigned verbosity;

extern LiftedSolverType liftedSolver;
extern GroundSolverType groundSolver;

}


namespace Constants {

// show message calculation for belief propagation
const bool showBpCalcs = false;

const int unobserved = -1;

// number of digits to show when printing a parameter
const unsigned precision = 8;

}

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_HORUS_H_

