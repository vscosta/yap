#ifndef YAP_PACKAGES_CLPBN_HORUS_UTIL_H_
#define YAP_PACKAGES_CLPBN_HORUS_UTIL_H_

#include <cmath>
#include <cassert>

#include <vector>
#include <queue>
#include <set>
#include <unordered_map>
#include <algorithm>
#include <limits>
#include <string>
#include <iostream>
#include <sstream>
#include <functional>

#include "Horus.h"

namespace Horus {

namespace {

const double NEG_INF = -std::numeric_limits<double>::infinity();

}


namespace Util {

template <typename T> void
addToVector (std::vector<T>&, const std::vector<T>&);

template <typename T> void
addToSet (std::set<T>&, const std::vector<T>&);

template <typename T> void
addToQueue (std::queue<T>&, const std::vector<T>&);

template <typename T> bool
contains (const std::vector<T>&, const T&);

template <typename T> bool contains
(const std::set<T>&, const T&);

template <typename K, typename V> bool
contains (const std::unordered_map<K, V>&, const K&);

template <typename T> size_t
indexOf (const std::vector<T>&, const T&);

template <class Operation> void
apply_n_times (Params& v1, const Params& v2, unsigned reps, Operation);

template <typename T> void
log (std::vector<T>&);

template <typename T> void
exp (std::vector<T>&);

template <typename T> std::string
elementsToString (const std::vector<T>& v, std::string sep = " ");

template <typename T> std::string
toString (const T&);

template <> std::string
toString (const bool&);

double logSum (double, double);

unsigned maxUnsigned();

unsigned stringToUnsigned (std::string);

double stringToDouble (std::string);

double factorial (unsigned);

double logFactorial (unsigned);

unsigned nrCombinations (unsigned, unsigned);

size_t sizeExpected (const Ranges&);

unsigned nrDigits (int);

bool isInteger (const std::string&);

std::string parametersToString (
    const Params&, unsigned = Constants::precision);

std::vector<std::string> getStateLines (const Vars&);

bool setHorusFlag (std::string option, std::string value);

void printHeader (std::string, std::ostream& os = std::cout);

void printSubHeader (std::string, std::ostream& os = std::cout);

void printAsteriskLine (std::ostream& os = std::cout);

void printDashedLine (std::ostream& os = std::cout);

}  // namespace Util



template <typename T> void
Util::addToVector (std::vector<T>& v, const std::vector<T>& elements)
{
  v.insert (v.end(), elements.begin(), elements.end());
}



template <typename T> void
Util::addToSet (std::set<T>& s, const std::vector<T>& elements)
{
  s.insert (elements.begin(), elements.end());
}



template <typename T> void
Util::addToQueue (std::queue<T>& q, const std::vector<T>& elements)
{
  for (size_t i = 0; i < elements.size(); i++) {
    q.push (elements[i]);
  }
}



template <typename T> bool
Util::contains (const std::vector<T>& v, const T& e)
{
  return std::find (v.begin(), v.end(), e) != v.end();
}



template <typename T> bool
Util::contains (const std::set<T>& s, const T& e)
{
  return s.find (e) != s.end();
}



template <typename K, typename V> bool
Util::contains (const std::unordered_map<K, V>& m, const K& k)
{
  return m.find (k) != m.end();
}



template <typename T> size_t
Util::indexOf (const std::vector<T>& v, const T& e)
{
  return std::distance (v.begin(),
      std::find (v.begin(), v.end(), e));
}



template <class Operation> void
Util::apply_n_times (
    Params& v1,
    const Params& v2,
    unsigned repetitions,
    Operation unary_op)
{
  Params::iterator       first  = v1.begin();
  Params::const_iterator last   = v1.end();
  Params::const_iterator first2 = v2.begin();
  Params::const_iterator last2  = v2.end();
  while (first != last) {
    for (first2 = v2.begin(); first2 != last2; ++first2) {
      std::transform (first, first + repetitions, first,
          std::bind1st (unary_op, *first2));
      first += repetitions;
    }
  }
}



template <typename T> void
Util::log (std::vector<T>& v)
{
  std::transform (v.begin(), v.end(), v.begin(),  (double (*)(double))std::log);
}



template <typename T> void
Util::exp (std::vector<T>& v)
{
  std::transform (v.begin(), v.end(), v.begin(),  (double (*)(double))std::exp);
}



template <typename T> std::string
Util::elementsToString (const std::vector<T>& v, std::string sep)
{
  std::stringstream ss;
  for (size_t i = 0; i < v.size(); i++) {
    ss << ((i != 0) ? sep : "") << v[i];
  }
  return ss.str();
}



template <typename T> std::string
Util::toString (const T& t)
{
  std::stringstream ss;
  ss << t;
  return ss.str();
}



inline double
Util::logSum (double x, double y)
{
  // std::log (std::exp (x) + std::exp (y)) can overflow!
  assert (std::isnan (x) == false);
  assert (std::isnan (y) == false);
  if (x == NEG_INF) {
    return y;
  }
  if (y == NEG_INF) {
    return x;
  }
  // if one value is much smaller than the other,
  // keep the larger value
  const double tol = 460.517; // log (1e200)
  if (x < y - tol) {
    return y;
  }
  if (y < x - tol) {
    return x;
  }
  assert (std::isnan (x - y) == false);
  const double exp_diff = std::exp (x - y);
  if (std::isfinite (exp_diff) == false) {
    // difference is too large
    return x > y ? x : y;
  }
  // otherwise return the sum
  return y + std::log (static_cast<double>(1.0) + exp_diff);
}



inline unsigned
Util::maxUnsigned()
{
  return std::numeric_limits<unsigned>::max();
}



namespace LogAware {

inline double one()          { return Globals::logDomain ? 0.0     : 1.0; }
inline double zero()         { return Globals::logDomain ? NEG_INF : 0.0; }
inline double addIdenty()    { return Globals::logDomain ? NEG_INF : 0.0; }
inline double multIdenty()   { return Globals::logDomain ? 0.0     : 1.0; }
inline double withEvidence() { return Globals::logDomain ? 0.0     : 1.0; }
inline double noEvidence()   { return Globals::logDomain ? NEG_INF : 0.0; }
inline double log (double v) { return Globals::logDomain ? ::log (v) : v; }
inline double exp (double v) { return Globals::logDomain ? ::exp (v) : v; }

void normalize (Params&);

double getL1Distance (const Params&, const Params&);

double getMaxNorm (const Params&, const Params&);

double pow (double, unsigned);

double pow (double, double);

void pow (Params&, unsigned);

void pow (Params&, double);

}  // namespace LogAware



template <typename T> void
operator+=(std::vector<T>& v, double val)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::plus<double>(), val));
}



template <typename T> void
operator-=(std::vector<T>& v, double val)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::minus<double>(), val));
}



template <typename T> void
operator*=(std::vector<T>& v, double val)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::multiplies<double>(), val));
}



template <typename T> void
operator/=(std::vector<T>& v, double val)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::divides<double>(), val));
}



template <typename T> void
operator+=(std::vector<T>& a, const std::vector<T>& b)
{
  assert (a.size() == b.size());
  std::transform (a.begin(), a.end(), b.begin(), a.begin(),
      std::plus<double>());
}



template <typename T> void
operator-=(std::vector<T>& a, const std::vector<T>& b)
{
  assert (a.size() == b.size());
  std::transform (a.begin(), a.end(), b.begin(), a.begin(),
      std::minus<double>());
}



template <typename T> void
operator*=(std::vector<T>& a, const std::vector<T>& b)
{
  assert (a.size() == b.size());
  std::transform (a.begin(), a.end(), b.begin(), a.begin(),
      std::multiplies<double>());
}



template <typename T> void
operator/=(std::vector<T>& a, const std::vector<T>& b)
{
  assert (a.size() == b.size());
  std::transform (a.begin(), a.end(), b.begin(), a.begin(),
      std::divides<double>());
}



template <typename T> void
operator^=(std::vector<T>& v, double exp)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::ptr_fun<double, double, double> (std::pow), exp));
}



template <typename T> void
operator^=(std::vector<T>& v, int iexp)
{
  std::transform (v.begin(), v.end(), v.begin(),
      std::bind2nd (std::ptr_fun<double, int, double> (std::pow), iexp));
}



template <typename T> std::ostream&
operator<< (std::ostream& os, const std::vector<T>& v)
{
  os << "[" ;
  os << Util::elementsToString (v, ", ");
  os << "]" ;
  return os;
}


namespace FuncObj {

template<typename T>
struct max : public std::binary_function<T, T, T> {
  T operator() (const T& x, const T& y) const {
    return x < y ? y : x;
}};



template <typename T>
struct abs_diff : public std::binary_function<T, T, T> {
  T operator() (const T& x, const T& y) const {
    return std::abs (x - y);
}};



template <typename T>
struct abs_diff_exp : public std::binary_function<T, T, T> {
  T operator() (const T& x, const T& y) const {
    return std::abs (std::exp (x) - std::exp (y));
}};

}  // namespace FuncObj

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_UTIL_H_

