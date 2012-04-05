#ifndef HORUS_UTIL_H
#define HORUS_UTIL_H

#include <cmath>
#include <cassert>
#include <limits>

#include <algorithm>
#include <vector>
#include <set>
#include <queue>
#include <unordered_map>

#include <sstream>
#include <iostream>

#include "Horus.h"

using namespace std;


namespace Util {

template <typename T> void addToVector (vector<T>&, const vector<T>&);

template <typename T> void addToSet (set<T>&,  const vector<T>&);

template <typename T> void addToQueue (queue<T>&,  const vector<T>&);

template <typename T> bool contains (const vector<T>&, const T&);

template <typename T> bool contains (const set<T>&, const T&);

template <typename K, typename V> bool contains (
    const unordered_map<K, V>&, const K&);

template <typename T> std::string toString (const T&);

void toLog (Params&);

void fromLog (Params&);

double logSum (double, double);

void multiply (Params&, const Params&);

void multiply (Params&, const Params&, unsigned);

void add (Params&, const Params&);

void add (Params&, const Params&, unsigned);

double factorial (double);

unsigned nrCombinations (unsigned, unsigned);

unsigned expectedSize (const Ranges&);

unsigned getNumberOfDigits (int);

bool isInteger (const string&);

string parametersToString (const Params&, unsigned = Constants::PRECISION);

vector<string> getJointStateStrings (const VarNodes&);

void printHeader (string, std::ostream& os = std::cout);

void printSubHeader (string, std::ostream& os = std::cout);

void printAsteriskLine (std::ostream& os = std::cout);

void printDashedLine (std::ostream& os = std::cout);

unsigned maxUnsigned (void);

};



template <typename T> void
Util::addToVector (vector<T>& v, const vector<T>& elements)
{
  v.insert (v.end(), elements.begin(), elements.end());
}



template <typename T> void
Util::addToSet (set<T>& s, const vector<T>& elements)
{
  s.insert (elements.begin(), elements.end());
}



template <typename T> void
Util::addToQueue (queue<T>& q, const vector<T>& elements)
{
  for (unsigned i = 0; i < elements.size(); i++) {
    q.push (elements[i]);
  }
}



template <typename T> bool
Util::contains (const vector<T>& v, const T& e)
{
  return std::find (v.begin(), v.end(), e) != v.end();
}



template <typename T> bool
Util::contains (const set<T>& s, const T& e)
{
  return s.find (e) != s.end();
}



template <typename K, typename V> bool
Util::contains (const unordered_map<K, V>& m, const K& k)
{
  return m.find (k) != m.end();
}



template <typename T> std::string
Util::toString (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}



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


namespace {
const double INF = -numeric_limits<double>::infinity();
};


inline double
Util::logSum (double x, double y)
{
  return log (exp (x) + exp (y));
  assert (isfinite (x) && isfinite (y));
  // If one value is much smaller than the other, keep the larger value.
  if (x < (y - log (1e200))) {
    return y;
  }
  if (y < (x - log (1e200))) {
    return x;
  }
  double diff = x - y;
  assert (isfinite (diff) && isfinite (x) && isfinite (y));
  if (!isfinite (exp (diff))) {
    // difference is too large
    return x > y ? x : y;
  }
  // otherwise return the sum.
  return y + log (static_cast<double>(1.0) + exp (diff));
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



inline unsigned
Util::maxUnsigned (void)
{
  return numeric_limits<unsigned>::max();
}



namespace LogAware {

inline double
one()
{
  return Globals::logDomain ? 0.0 : 1.0;
}


inline double
zero() {
  return Globals::logDomain ? INF : 0.0 ;
}


inline double
addIdenty()
{
  return Globals::logDomain ? INF : 0.0;
}


inline double
multIdenty()
{
  return Globals::logDomain ? 0.0 : 1.0;
}


inline double
withEvidence()
{
  return Globals::logDomain ? 0.0 : 1.0;
}


inline double
noEvidence() {
  return Globals::logDomain ? INF : 0.0;
}


inline double
tl (double v)
{
  return Globals::logDomain ? log (v) : v;
}


inline double
fl (double v) 
{
  return Globals::logDomain ? exp (v) : v;
}


void normalize (Params&);

double getL1Distance (const Params&, const Params&);

double getMaxNorm (const Params&, const Params&);

double pow (double, unsigned);

double pow (double, double);

void pow (Params&, unsigned);

void pow (Params&, double);

};


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

