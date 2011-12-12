#ifndef HORUS_SHARED_H
#define HORUS_SHARED_H

#include <cmath>
#include <cassert>
#include <limits>

#include <vector>
#include <unordered_map>

#include <iostream>
#include <fstream>

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class VarNode;
class BayesNet;
class BayesNode;
class Factor;
class FgVarNode;
class FgFacNode;
class SpLink;
class BpLink;

typedef double                    Param;
typedef vector<Param>             ParamSet;
typedef unsigned                  VarId;
typedef vector<VarId>             VarIdSet;
typedef vector<VarNode*>          VarNodes;
typedef vector<BayesNode*>        BnNodeSet;
typedef vector<FgVarNode*>        FgVarSet;
typedef vector<FgFacNode*>        FgFacSet;
typedef vector<Factor*>           FactorSet;
typedef vector<string>            States;
typedef vector<unsigned>          Ranges;
typedef vector<unsigned>          DConf;
typedef pair<unsigned, unsigned>  DConstraint;


// level of debug information
static const unsigned DL = 0;

static const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
static const unsigned PRECISION = 5;

static const bool COLLECT_STATISTICS = false;

static const bool EXPORT_TO_GRAPHVIZ = false;
static const unsigned EXPORT_MINIMAL_SIZE = 100;

static const double INF = -numeric_limits<Param>::infinity();


namespace NumberSpace {
  enum ns {
    NORMAL,
    LOGARITHM
  };
};



extern NumberSpace::ns NSPACE;


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
  extern bool      useAlwaysLoopySolver;
}


namespace Util
{
  void            toLog (ParamSet&);
  void            fromLog (ParamSet&);
  void            normalize (ParamSet&);
  void            logSum (Param&, Param);
  void            multiply (ParamSet&, const ParamSet&);
  void            multiply (ParamSet&, const ParamSet&, unsigned);
  void            add (ParamSet&, const ParamSet&);
  void            add (ParamSet&, const ParamSet&, unsigned);
  void            pow (ParamSet&, unsigned);
  Param           pow (Param, unsigned);
  double          getL1Distance (const ParamSet&, const ParamSet&);
  double          getMaxNorm (const ParamSet&, const ParamSet&);
  unsigned        getNumberOfDigits (int);
  bool            isInteger (const string&);
  string          parametersToString (const ParamSet&, unsigned = PRECISION);
  BayesNet*       generateBayesianNetworkTreeWithLevel (unsigned);
  vector<DConf>   getDomainConfigurations (const VarNodes&);
  vector<string>  getJointStateStrings (const VarNodes&);
  double          tl (Param v);
  double          fl (Param v);
  double          multIdenty();
  double          addIdenty();
  double          withEvidence();
  double          noEvidence();
  double          one();
  double          zero();
};



inline void
Util::logSum (Param& x, Param y)
{
  // x = log (exp (x) + exp (y)); return;
  assert (isfinite (x) && finite (y));
  // If one value is much smaller than the other, keep the larger value.
  if (x < (y - log (1e200))) {
    x = y;
    return;
  }
  if (y < (x - log (1e200))) {
    return;
  }
  double diff = x - y;
  assert (isfinite (diff) && finite (x) && finite (y));
  if (!isfinite (exp (diff))) { // difference is too large
    x = x > y ? x : y;
  } else { // otherwise return the sum.
    x = y + log (static_cast<double>(1.0) + exp (diff));
  }
}



inline void
Util::multiply (ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  for (unsigned i = 0; i < v1.size(); i++) {
    v1[i] *= v2[i];
  }
}



inline void
Util::multiply (ParamSet& v1, const ParamSet& v2, unsigned repetitions)
{
  for (unsigned count = 0; count < v1.size(); ) {
    for (unsigned i = 0; i < v2.size(); i++) {
      for (unsigned r = 0; r < repetitions; r++) {
        v1[count] *= v2[i];
        count ++;
      }
    }
  }
}



inline void
Util::add (ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  for (unsigned i = 0; i < v1.size(); i++) {
    v1[i] += v2[i];
  }
}



inline void
Util::add (ParamSet& v1, const ParamSet& v2, unsigned repetitions)
{
  for (unsigned count = 0; count < v1.size(); ) {
    for (unsigned i = 0; i < v2.size(); i++) {
      for (unsigned r = 0; r < repetitions; r++) {
        v1[count] += v2[i];
        count ++;
      }
    }
  }
}



inline double
Util::tl (Param v)
{
  return NSPACE == NumberSpace::NORMAL ? v : log(v);
}

inline double
Util::fl (Param v) 
{
  return NSPACE == NumberSpace::NORMAL ? v : exp(v);
}

inline double
Util::multIdenty() {
  return NSPACE == NumberSpace::NORMAL ? 1.0 : 0.0;
}

inline double
Util::addIdenty()
{
  return NSPACE == NumberSpace::NORMAL ? 0.0 : INF;
}

inline double
Util::withEvidence()
{
  return NSPACE == NumberSpace::NORMAL ? 1.0 : 0.0;
}

inline double
Util::noEvidence() {
  return NSPACE == NumberSpace::NORMAL ? 0.0 : INF;
}

inline double
Util::one()
{
  return NSPACE == NumberSpace::NORMAL ? 1.0 : 0.0;
}

inline double
Util::zero() {
  return NSPACE == NumberSpace::NORMAL ? 0.0 : INF;
}


struct NetInfo
{
  NetInfo (unsigned size, bool loopy, unsigned nIters, double time)
  { 
    this->size   = size;
    this->loopy  = loopy;
    this->nIters = nIters;
    this->time   = time;
  }
  unsigned  size;
  bool      loopy;
  unsigned  nIters;
  double    time;
};


struct CompressInfo
{ 
  CompressInfo (unsigned a, unsigned b, unsigned c, unsigned d, unsigned e)
  {
    nGroundVars     = a; 
    nGroundFactors  = b; 
    nClusterVars    = c;
    nClusterFactors = d;
    nWithoutNeighs  = e;
  }
  unsigned nGroundVars;
  unsigned nGroundFactors;
  unsigned nClusterVars;
  unsigned nClusterFactors;
  unsigned nWithoutNeighs;
};


class Statistics
{
  public:
    static unsigned getSolvedNetworksCounting (void);
    static void incrementPrimaryNetworksCounting (void);
    static unsigned getPrimaryNetworksCounting (void);
    static void updateStatistics (unsigned, bool, unsigned, double);
    static void printStatistics (void);
    static void writeStatisticsToFile (const char*);
    static void updateCompressingStatistics (
        unsigned, unsigned, unsigned, unsigned, unsigned);

  private:
    static string getStatisticString (void);

    static vector<NetInfo> netInfo_;
    static vector<CompressInfo> compressInfo_;
    static unsigned primaryNetCount_;
};

#endif // HORUS_SHARED_H

