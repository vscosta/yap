#ifndef HORUS_HORUS_H
#define HORUS_HORUS_H

#include <limits>

#include <vector>

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class VarNode;
class BayesNode;
class FgVarNode;
class FgFacNode;
class Factor;

typedef vector<double>       Params;
typedef unsigned             VarId;
typedef vector<VarId>        VarIds;
typedef vector<VarNode*>     VarNodes;
typedef vector<BayesNode*>   BnNodeSet;
typedef vector<FgVarNode*>   FgVarSet;
typedef vector<FgFacNode*>   FgFacSet;
typedef vector<Factor*>      FactorSet;
typedef vector<string>       States;
typedef vector<unsigned>     Ranges;


enum InfAlgorithms
{
  VE,     // variable elimination
  BN_BP,  // bayesian network belief propagation
  FG_BP,  // factor graph belief propagation
  CBP     // counting bp solver
};


namespace Globals {

extern bool logDomain;

extern InfAlgorithms infAlgorithm;

};


namespace Constants {

// level of debug information
const unsigned DEBUG = 2;

const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
const unsigned PRECISION = 5;

const bool COLLECT_STATS = false;

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

