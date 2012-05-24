#include <limits>

#include <sstream>
#include <fstream>

#include "Util.h"
#include "Indexer.h"
#include "ElimGraph.h"


namespace Globals {
bool logDomain = false;

unsigned verbosity = 0;

InfAlgorithms infAlgorithm = InfAlgorithms::VE;
};




namespace BpOptions {
Schedule schedule = BpOptions::Schedule::SEQ_FIXED;
//Schedule schedule = BpOptions::Schedule::SEQ_RANDOM;
//Schedule schedule = BpOptions::Schedule::PARALLEL;
//Schedule schedule = BpOptions::Schedule::MAX_RESIDUAL;
double    accuracy  = 0.0001;
unsigned  maxIter   = 1000;
}



vector<NetInfo>      Statistics::netInfo_;
vector<CompressInfo> Statistics::compressInfo_;
unsigned             Statistics::primaryNetCount_;


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
    cerr << "error: the readed number is negative" << endl;
    abort();
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



unsigned
sizeExpected (const Ranges& ranges)
{
  return std::accumulate (
      ranges.begin(), ranges.end(), 1, multiplies<unsigned>());
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
  for (unsigned i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", " ;
    ss << v[i];
  }
  ss << "]" ;
  return ss.str();
}



vector<string>
getStateLines (const Vars& vars)
{
  StatesIndexer idx (vars);
  vector<string> jointStrings;
  while (idx.valid()) {
    stringstream ss;
    for (unsigned i = 0; i < vars.size(); i++) {
      if (i != 0) ss << ", " ;
      ss << vars[i]->label() << "=" << vars[i]->states()[(idx[i])];
    }
    jointStrings.push_back (ss.str());
    ++ idx;
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
  } else if (key == "inf_alg") {
    if (       value == "ve") {
      Globals::infAlgorithm = InfAlgorithms::VE;
    } else if (value == "bp") {
      Globals::infAlgorithm = InfAlgorithms::BP;
    } else if (value == "cbp") {
      Globals::infAlgorithm = InfAlgorithms::CBP;
    } else {
      cerr << "warning: invalid value `" << value << "' " ;
      cerr << "for `" << key << "'" << endl;
      returnVal = false;
    }
  } else if (key == "elim_heuristic") {
    if (       value == "min_neighbors") {
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
  double sum = LogAware::addIdenty();
  if (Globals::logDomain) {
    for (unsigned i = 0; i < v.size(); i++) {
      sum = Util::logSum (sum, v[i]);
    }
    assert (sum != -numeric_limits<double>::infinity());
    for (unsigned i = 0; i < v.size(); i++) {
      v[i] -= sum;
    }
  } else {
    for (unsigned i = 0; i < v.size(); i++) {
      sum += v[i];
    }
    assert (sum != 0.0);
    for (unsigned i = 0; i < v.size(); i++) {
      v[i] /= sum;
    }
  }
}



double
getL1Distance (const Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  double dist = 0.0;
  if (Globals::logDomain) {
    for (unsigned i = 0; i < v1.size(); i++) {
      dist += abs (exp(v1[i]) - exp(v2[i]));
    }
  } else {
    for (unsigned i = 0; i < v1.size(); i++) {
      dist += abs (v1[i] - v2[i]);
    }
  }
  return dist;
}



double
getMaxNorm (const Params& v1, const Params& v2)
{
  assert (v1.size() == v2.size());
  double max = 0.0;
  if (Globals::logDomain) {
    for (unsigned i = 0; i < v1.size(); i++) {
      double diff = abs (exp(v1[i]) - exp(v2[i]));
      if (diff > max) {
        max = diff;
      }
    }
  } else {
    for (unsigned i = 0; i < v1.size(); i++) {
      double diff = abs (v1[i] - v2[i]);
      if (diff > max) {
        max = diff;
      }
    }
  }
  return max;
}



double
pow (double base, unsigned iexp)
{
  return Globals::logDomain ? base * iexp : std::pow (base, iexp);
}



double
pow (double base, double exp)
{
  // assumes that `expoent' is never in log domain
  return Globals::logDomain ? base * exp : std::pow (base, exp);
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



unsigned
Statistics::getSolvedNetworksCounting (void)
{
  return netInfo_.size();
}



void
Statistics::incrementPrimaryNetworksCounting (void)
{
  primaryNetCount_ ++;
}
   


unsigned
Statistics::getPrimaryNetworksCounting (void)
{
  return primaryNetCount_;
}



void
Statistics::updateStatistics (
    unsigned size,
    bool loopy,
    unsigned nIters,
    double time)
{
  netInfo_.push_back (NetInfo (size, loopy, nIters, time));
}

  

void
Statistics::printStatistics (void)
{
  cout << getStatisticString();
}



void
Statistics::writeStatistics (const char* fileName)
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "Statistics::writeStats()" << endl;
    abort();
  }
  out << getStatisticString();
  out.close();
}



void
Statistics::updateCompressingStatistics (
    unsigned nrGroundVars,
    unsigned nrGroundFactors,
    unsigned nrClusterVars, 
    unsigned nrClusterFactors,
    unsigned nrNeighborless)
{
  compressInfo_.push_back (CompressInfo (nrGroundVars, nrGroundFactors,
      nrClusterVars, nrClusterFactors, nrNeighborless));
}



string
Statistics::getStatisticString (void)
{
  stringstream ss2, ss3, ss4, ss1;
  ss1 << "running mode:          " ;
  switch (Globals::infAlgorithm) {
    case InfAlgorithms::VE:  ss1 << "ve"  << endl;  break;
    case InfAlgorithms::BP:  ss1 << "bp"  << endl;  break;
    case InfAlgorithms::CBP: ss1 << "cbp" << endl;  break;
  }
  ss1 << "message schedule:      " ;
  switch (BpOptions::schedule) {
    case BpOptions::Schedule::SEQ_FIXED:
      ss1 << "sequential fixed"  << endl;
      break;
    case BpOptions::Schedule::SEQ_RANDOM:
      ss1 << "sequential random" << endl;
      break;
    case BpOptions::Schedule::PARALLEL:
      ss1 << "parallel"          << endl;
      break;
    case BpOptions::Schedule::MAX_RESIDUAL:
      ss1 << "max residual"      << endl;
      break;
  }
  ss1 << "max iterations:        " << BpOptions::maxIter  << endl;
  ss1 << "accuracy               " << BpOptions::accuracy << endl;
  ss1 << endl << endl;
  Util::printSubHeader ("Network information", ss2);
  ss2 << left;
  ss2 << setw (15) << "Network Size" ;
  ss2 << setw (9)  << "Loopy" ;
  ss2 << setw (15) << "Iterations" ;
  ss2 << setw (15) << "Solving Time" ;
  ss2 << endl;
  unsigned nLoopyNets       = 0;
  unsigned nUnconvergedRuns = 0;
  double totalSolvingTime   = 0.0;
  for (unsigned i = 0; i < netInfo_.size(); i++) {
    ss2 << setw (15) << netInfo_[i].size;
    if (netInfo_[i].loopy) {
      ss2 << setw (9) << "yes";
      nLoopyNets ++;
    } else {
      ss2 << setw (9) << "no";
    }
    if (netInfo_[i].nIters == 0) {
      ss2 << setw (15) << "n/a" ;
    } else {
      ss2 << setw (15) << netInfo_[i].nIters;
      if (netInfo_[i].nIters > BpOptions::maxIter) {
        nUnconvergedRuns ++;
      }
    }
    ss2 << setw (15) << netInfo_[i].time;
    totalSolvingTime += netInfo_[i].time;
    ss2 << endl;
  }
  ss2 << endl << endl;

  unsigned c1 = 0, c2 = 0, c3 = 0, c4 = 0;
  if (compressInfo_.size() > 0) {
    Util::printSubHeader ("Compress information", ss3);
    ss3 << left;
    ss3 << "Ground   Cluster   Ground    Cluster   Neighborless" << endl;
    ss3 << "Vars     Vars      Factors   Factors   Vars"         << endl;
    for (unsigned i = 0; i < compressInfo_.size(); i++) {
      ss3 << setw (9)  << compressInfo_[i].nrGroundVars;
      ss3 << setw (10) << compressInfo_[i].nrClusterVars;
      ss3 << setw (10) << compressInfo_[i].nrGroundFactors;
      ss3 << setw (10) << compressInfo_[i].nrClusterFactors;
      ss3 << setw (10) << compressInfo_[i].nrNeighborless;
      ss3 << endl;
      c1 += compressInfo_[i].nrGroundVars - compressInfo_[i].nrNeighborless;
      c2 += compressInfo_[i].nrClusterVars;
      c3 += compressInfo_[i].nrGroundFactors - compressInfo_[i].nrNeighborless;
      c4 += compressInfo_[i].nrClusterFactors;
      if (compressInfo_[i].nrNeighborless != 0) {
        c2 --;
        c4 --;
      } 
    }
    ss3 << endl << endl;
  }

  ss4 << "primary networks:      " << primaryNetCount_ << endl;
  ss4 << "solved networks:       " << netInfo_.size()  << endl;
  ss4 << "loopy networks:        " << nLoopyNets       << endl;
  ss4 << "unconverged runs:      " << nUnconvergedRuns << endl;
  ss4 << "total solving time:    " << totalSolvingTime << endl;
  if (compressInfo_.size() > 0) {
    double pc1 = (1.0 - (c2 / (double)c1)) * 100.0;
    double pc2 = (1.0 - (c4 / (double)c3)) * 100.0;
    ss4 << setprecision (5);
    ss4 << "variable compression:  " << pc1 << "%" << endl;
    ss4 << "factor compression:    " << pc2 << "%" << endl;
  }
  ss4 << endl << endl; 

  ss1 << ss4.str() << ss2.str() << ss3.str();
  return ss1.str();
}

