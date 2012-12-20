#include <limits>

#include <sstream>
#include <fstream>

#include "Util.h"
#include "Indexer.h"
#include "ElimGraph.h"


namespace Globals {
bool logDomain = false;

unsigned verbosity = 0;

LiftedSolverType liftedSolver = LiftedSolverType::LVE;

GroundSolverType groundSolver = GroundSolverType::VE;

};



namespace BpOptions {
Schedule schedule = BpOptions::Schedule::SEQ_FIXED;
//Schedule schedule = BpOptions::Schedule::SEQ_RANDOM;
//Schedule schedule = BpOptions::Schedule::PARALLEL;
//Schedule schedule = BpOptions::Schedule::MAX_RESIDUAL;
double    accuracy  = 0.0001;
unsigned  maxIter   = 1000;
}



namespace Util {


template <> std::string
toString (const bool& b)
{
  std::stringstream ss;
  ss << std::boolalpha << b;
  return ss.str();
}



unsigned
stringToUnsigned (string str)
{
  int val;
  stringstream ss;
  ss << str;
  ss >> val;
  if (val < 0) {
    cerr << "Error: the number readed is negative." << endl;
    exit (EXIT_FAILURE);
  }
  return static_cast<unsigned> (val);
}



double
stringToDouble (string str)
{
  double val;
  stringstream ss;
  ss << str;
  ss >> val;
  return val;
}



double
factorial (unsigned num)
{
  double result = 1.0;
  for (unsigned i = 1; i <= num; i++) {
    result *= i;
  }
  return result;
}



double
logFactorial (unsigned num)
{
  double result = 0.0;
  if (num < 150) {
    result = std::log (factorial (num));
  } else {
    for (unsigned i = 1; i <= num; i++) {
      result += std::log (i);
    }
  }
  return result;
}



unsigned
nrCombinations (unsigned n, unsigned k)
{
  assert (n >= k);
  int diff = n - k;
  unsigned result = 0;
  if (n < 150) {
    unsigned prod = 1;
    for (int i = n; i > diff; i--) {
      prod *= i;
    }
    result = prod / factorial (k);
  } else {
    double prod = 0.0;
    for (int i = n; i > diff; i--) {
      prod += std::log (i);
    }
    prod -= logFactorial (k);
    result = static_cast<unsigned> (std::exp (prod));
  }
  return result;
}



size_t
sizeExpected (const Ranges& ranges)
{
  return std::accumulate (ranges.begin(),
      ranges.end(), 1, multiplies<unsigned>());
}



unsigned
nrDigits (int num)
{
  unsigned count = 1;
  while (num >= 10) {
    num /= 10;
    count ++;
  }
  return count;
}



bool
isInteger (const string& s)
{
  stringstream ss1 (s);
  stringstream ss2;
  int integer;
  ss1 >> integer;
  ss2 << integer;
  return (ss1.str() == ss2.str());
}



string
parametersToString (const Params& v, unsigned precision)
{
  stringstream ss;
  ss.precision (precision);
  ss << "[" ;
  for (size_t i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", " ;
    ss << v[i];
  }
  ss << "]" ;
  return ss.str();
}



vector<string>
getStateLines (const Vars& vars)
{
  Ranges ranges;
  for (size_t i = 0; i < vars.size(); i++) {
    ranges.push_back (vars[i]->range());
  }
  Indexer indexer (ranges);
  vector<string> jointStrings;
  while (indexer.valid()) {
    stringstream ss;
    for (size_t i = 0; i < vars.size(); i++) {
      if (i != 0) ss << ", " ;
      ss << vars[i]->label() << "=" ;
      ss << vars[i]->states()[(indexer[i])];
    }
    jointStrings.push_back (ss.str());
    ++ indexer;
  }
  return jointStrings;
}



bool
setHorusFlag (string key, string value)
{
  bool returnVal = true;
  if (key == "verbosity") {
    stringstream ss;
    ss << value;
    ss >> Globals::verbosity;
  } else if (key == "lifted_solver") {
    if (       value == "lve") {
      Globals::liftedSolver = LiftedSolverType::LVE;
    } else if (value == "lbp") {
      Globals::liftedSolver = LiftedSolverType::LBP;
    } else if (value == "lkc") {
      Globals::liftedSolver = LiftedSolverType::LKC;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else if (key == "ground_solver") {
    if (       value == "ve") {
      Globals::groundSolver = GroundSolverType::VE;
    } else if (value == "bp") {
      Globals::groundSolver = GroundSolverType::BP;
    } else if (value == "cbp") {
      Globals::groundSolver = GroundSolverType::CBP;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else if (key == "elim_heuristic") {
    if (       value == "sequential") {
      ElimGraph::elimHeuristic = ElimHeuristic::SEQUENTIAL;
    } else if (value == "min_neighbors") {
      ElimGraph::elimHeuristic = ElimHeuristic::MIN_NEIGHBORS;
    } else if (value == "min_weight") {
      ElimGraph::elimHeuristic = ElimHeuristic::MIN_WEIGHT;
    } else if (value == "min_fill") {
      ElimGraph::elimHeuristic = ElimHeuristic::MIN_FILL;
    } else if (value == "weighted_min_fill") {
      ElimGraph::elimHeuristic = ElimHeuristic::WEIGHTED_MIN_FILL;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else if (key == "schedule") {
    if (       value == "seq_fixed") {
      BpOptions::schedule = BpOptions::Schedule::SEQ_FIXED;
    } else if (value == "seq_random") {
      BpOptions::schedule = BpOptions::Schedule::SEQ_RANDOM;
    } else if (value == "parallel") {
      BpOptions::schedule = BpOptions::Schedule::PARALLEL;
    } else if (value == "max_residual") {
      BpOptions::schedule = BpOptions::Schedule::MAX_RESIDUAL;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else if (key == "accuracy") {
    stringstream ss;
    ss << value;
    ss >> BpOptions::accuracy;
  } else if (key == "max_iter") {
    stringstream ss;
    ss << value;
    ss >> BpOptions::maxIter;
  } else if (key == "use_logarithms") {
    if (       value == "true") {
      Globals::logDomain = true;
    } else if (value == "false") {
      Globals::logDomain = false;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else {
    cerr << "warning: invalid key `" << key << "'" << endl;
    returnVal = false;
  }
  return returnVal;
}



void
printHeader (string header, std::ostream& os)
{
  printAsteriskLine (os);
  os << header << endl;
  printAsteriskLine (os);
}



void
printSubHeader (string header, std::ostream& os)
{
  printDashedLine (os);
  os << header << endl;
  printDashedLine (os);
}



void
printAsteriskLine (std::ostream& os)
{
  os << "********************************" ;
  os << "********************************" ;
  os << endl;
}



void
printDashedLine (std::ostream& os)
{
  os << "--------------------------------" ;
  os << "--------------------------------" ;
  os << endl;
}


}



namespace LogAware {

void
normalize (Params& v)
{
  if (Globals::logDomain) {
    double sum = std::accumulate (v.begin(), v.end(),
        LogAware::addIdenty(), Util::logSum);
    assert (sum != -numeric_limits<double>::infinity());
    v -= sum;
  } else {
    double sum = std::accumulate (v.begin(), v.end(), 0.0);
    assert (sum != 0.0);
    v /= sum;
  }
}



double
getL1Distance (const Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  double dist = 0.0;
  if (Globals::logDomain) {
    dist = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        std::plus<double>(), FuncObject::abs_diff_exp<double>());
  } else {
    dist = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        std::plus<double>(), FuncObject::abs_diff<double>());
  }
  return dist;
}



double
getMaxNorm (const Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  double max = 0.0;
  if (Globals::logDomain) {
    max = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        FuncObject::max<double>(), FuncObject::abs_diff_exp<double>());
  } else {
    max = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        FuncObject::max<double>(), FuncObject::abs_diff<double>());
  }
  return max;
}



double
pow (double base, unsigned iexp)
{
  return Globals::logDomain
      ? base * iexp
      : std::pow (base, iexp);
}



double
pow (double base, double exp)
{
  // `expoent' should not be in log domain
  return Globals::logDomain
      ? base * exp
      : std::pow (base, exp);
}



void
pow (Params& v, unsigned iexp)
{
  if (iexp == 1) {
    return;
  }
  Globals::logDomain ? v *= iexp : v ^= (int)iexp;
}



void
pow (Params& v, double exp)
{
  // `expoent' should not be in log domain
  Globals::logDomain ? v *= exp : v ^= exp;
}

}

