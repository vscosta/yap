#ifndef HORUS_HORUS_H
#define HORUS_HORUS_H

#include <cmath>
#include <cassert>
#include <limits>

#include <algorithm>
#include <vector>
#include <unordered_map>

#include <iostream>
#include <fstream>
#include <sstream>

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class VarNode;
class BayesNode;
class FgVarNode;
class FgFacNode;
class Factor;

typedef vector<double>            Params;
typedef unsigned                  VarId;
typedef vector<VarId>             VarIds;
typedef vector<VarNode*>          VarNodes;
typedef vector<BayesNode*>        BnNodeSet;
typedef vector<FgVarNode*>        FgVarSet;
typedef vector<FgFacNode*>        FgFacSet;
typedef vector<Factor*>           FactorSet;
typedef vector<string>            States;
typedef vector<unsigned>          Ranges;


namespace Globals {
  extern bool logDomain;
};


// level of debug information
static const unsigned DL = 1;

static const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
static const unsigned PRECISION = 5;

static const bool COLLECT_STATISTICS = false;

static const bool EXPORT_TO_GRAPHVIZ = false;
static const unsigned EXPORT_MINIMAL_SIZE = 100;

static const double INF = -numeric_limits<double>::infinity();



namespace InfAlgorithms {
  enum InfAlgs
  {
    VE,     // variable elimination
    BN_BP,  // bayesian network belief propagation
    FG_BP,  // factor graph belief propagation
    CBP     // counting bp solver
  };
  extern InfAlgs infAlgorithm;
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

