#ifndef HORUS_UTIL_H
#define HORUS_UTIL_H

#include <vector>

#include "Horus.h"

using namespace std;

namespace Util {

void            toLog (Params&);
void            fromLog (Params&);
void            normalize (Params&);
void            logSum (double&, double);
void            multiply (Params&, const Params&);
void            multiply (Params&, const Params&, unsigned);
void            add (Params&, const Params&);
void            add (Params&, const Params&, unsigned);
void            pow (Params&, double);
void            pow (Params&, unsigned);
double          pow (double, unsigned);
double          factorial (double);
unsigned        nrCombinations (unsigned, unsigned);
double          getL1Distance (const Params&, const Params&);
double          getMaxNorm (const Params&, const Params&);
unsigned        getNumberOfDigits (int);
bool            isInteger (const string&);
string          parametersToString (const Params&, unsigned = PRECISION);
vector<string>  getJointStateStrings (const VarNodes&);
double          tl (double);
double          fl (double);
double          multIdenty();
double          addIdenty();
double          withEvidence();
double          noEvidence();
double          one();
double          zero();


template <class T>
std::string toString (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}

};



template <typename T> 
std::ostream& operator << (std::ostream& os, const vector<T>& v)
{
  os << "[" ;
  for (unsigned i = 0; i < v.size(); i++) {
    os << ((i != 0) ? ", " : "") << v[i];
  }
  os << "]" ;
  return os;
}




inline void
Util::logSum (double& x, double y)
{
  x = log (exp (x) + exp (y)); return;
  assert (isfinite (x) && isfinite (y));
  // If one value is much smaller than the other, keep the larger value.
  if (x < (y - log (1e200))) {
    x = y;
    return;
  }
  if (y < (x - log (1e200))) {
    return;
  }
  double diff = x - y;
  assert (isfinite (diff) && isfinite (x) && isfinite (y));
  if (!isfinite (exp (diff))) { // difference is too large
    x = x > y ? x : y;
  } else { // otherwise return the sum.
    x = y + log (static_cast<double>(1.0) + exp (diff));
  }
}



inline void
Util::multiply (Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  for (unsigned i = 0; i < v1.size(); i++) {
    v1[i] *= v2[i];
  }
}



inline void
Util::multiply (Params& v1, const Params& v2, unsigned repetitions)
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
Util::add (Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  for (unsigned i = 0; i < v1.size(); i++) {
    v1[i] += v2[i];
  }
}



inline void
Util::add (Params& v1, const Params& v2, unsigned repetitions)
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
Util::tl (double v)
{
  return Globals::logDomain ? log(v) : v;
}

inline double
Util::fl (double v) 
{
  return Globals::logDomain ? exp(v) : v;
}

inline double
Util::multIdenty() {
  return Globals::logDomain ? 0.0 : 1.0;
}

inline double
Util::addIdenty()
{
  return Globals::logDomain ? INF : 0.0;
}

inline double
Util::withEvidence()
{
  return Globals::logDomain ? 0.0 : 1.0;
}

inline double
Util::noEvidence() {
  return Globals::logDomain ? INF : 0.0;
}

inline double
Util::one()
{
  return Globals::logDomain ? 0.0 : 1.0;
}

inline double
Util::zero() {
  return Globals::logDomain ? INF : 0.0 ;
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

#endif // HORUS_UTIL_H

