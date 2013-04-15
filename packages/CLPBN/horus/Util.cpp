#include "Util.h"
#include "Indexer.h"
#include "ElimGraph.h"
#include "BeliefProp.h"


namespace Horus {

namespace Globals {

bool logDomain = false;

unsigned verbosity = 0;

LiftedSolverType liftedSolver = LiftedSolverType::lveSolver;

GroundSolverType groundSolver = GroundSolverType::veSolver;

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
stringToUnsigned (std::string str)
{
  int val;
  std::stringstream ss;
  ss << str;
  ss >> val;
  if (val < 0) {
    std::cerr << "Error: the number readed is negative." << std::endl;
    exit (EXIT_FAILURE);
  }
  return static_cast<unsigned> (val);
}



double
stringToDouble (std::string str)
{
  double val;
  std::stringstream ss;
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
      ranges.end(), 1, std::multiplies<unsigned>());
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
isInteger (const std::string& s)
{
  std::stringstream ss1 (s);
  std::stringstream ss2;
  int integer;
  ss1 >> integer;
  ss2 << integer;
  return (ss1.str() == ss2.str());
}



std::string
parametersToString (const Params& v, unsigned precision)
{
  std::stringstream ss;
  ss.precision (precision);
  ss << "[" ;
  for (size_t i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", " ;
    ss << v[i];
  }
  ss << "]" ;
  return ss.str();
}



std::vector<std::string>
getStateLines (const Vars& vars)
{
  Ranges ranges;
  for (size_t i = 0; i < vars.size(); i++) {
    ranges.push_back (vars[i]->range());
  }
  Indexer indexer (ranges);
  std::vector<std::string> jointStrings;
  while (indexer.valid()) {
    std::stringstream ss;
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



bool invalidValue (std::string option, std::string value)
{
  std::cerr << "Warning: invalid value `" << value << "' " ;
  std::cerr << "for `" << option << "'." ;
  std::cerr << std::endl;
  return false;
}



bool
setHorusFlag (std::string option, std::string value)
{
  bool returnVal = true;
  if (option == "lifted_solver") {
    if      (value == "lve")
      Globals::liftedSolver = LiftedSolverType::lveSolver;
    else if (value == "lbp")
      Globals::liftedSolver = LiftedSolverType::lbpSolver;
    else if (value == "lkc")
      Globals::liftedSolver = LiftedSolverType::lkcSolver;
    else
      returnVal = invalidValue (option, value);

  } else if (option == "ground_solver" || option == "solver") {
    if      (value == "hve")
      Globals::groundSolver = GroundSolverType::veSolver;
    else if (value == "bp")
      Globals::groundSolver = GroundSolverType::bpSolver;
    else if (value == "cbp")
      Globals::groundSolver = GroundSolverType::CbpSolver;
    else
      returnVal = invalidValue (option, value);

  } else if (option == "verbosity") {
    std::stringstream ss;
    ss << value;
    ss >> Globals::verbosity;

  } else if (option == "use_logarithms") {
    if      (value == "true")  Globals::logDomain = true;
    else if (value == "false") Globals::logDomain = false;
    else                       returnVal = invalidValue (option, value);

  } else if (option == "hve_elim_heuristic") {
    typedef ElimGraph::ElimHeuristic ElimHeuristic;
    if      (value == "sequential")
      ElimGraph::setElimHeuristic (ElimHeuristic::sequentialEh);
    else if (value == "min_neighbors")
      ElimGraph::setElimHeuristic (ElimHeuristic::minNeighborsEh);
    else if (value == "min_weight")
      ElimGraph::setElimHeuristic (ElimHeuristic::minWeightEh);
    else if (value == "min_fill")
      ElimGraph::setElimHeuristic (ElimHeuristic::minFillEh);
    else if (value == "weighted_min_fill")
      ElimGraph::setElimHeuristic (ElimHeuristic::weightedMinFillEh);
    else
      returnVal = invalidValue (option, value);

  } else if (option == "bp_msg_schedule") {
    typedef BeliefProp::MsgSchedule MsgSchedule;
    if      (value == "seq_fixed")
      BeliefProp::setMsgSchedule (MsgSchedule::seqFixedSch);
    else if (value == "seq_random")
      BeliefProp::setMsgSchedule (MsgSchedule::seqRandomSch);
    else if (value == "parallel")
      BeliefProp::setMsgSchedule (MsgSchedule::parallelSch);
    else if (value == "max_residual")
      BeliefProp::setMsgSchedule (MsgSchedule::maxResidualSch);
    else
      returnVal = invalidValue (option, value);

  } else if (option == "bp_accuracy") {
    std::stringstream ss;
    double acc;
    ss << value;
    ss >> acc;
    BeliefProp::setAccuracy (acc);

  } else if (option == "bp_max_iter") {
    std::stringstream ss;
    unsigned mi;
    ss << value;
    ss >> mi;
    BeliefProp::setMaxIterations (mi);

  } else if (option == "export_libdai") {
    if      (value == "true")  FactorGraph::enableExportToLibDai();
    else if (value == "false") FactorGraph::disableExportToLibDai();
    else                       returnVal = invalidValue (option, value);

  } else if (option == "export_uai") {
    if      (value == "true")  FactorGraph::enableExportToUai();
    else if (value == "false") FactorGraph::disableExportToUai();
    else                       returnVal = invalidValue (option, value);

  } else if (option == "export_graphviz") {
    if      (value == "true")  FactorGraph::enableExportToGraphViz();
    else if (value == "false") FactorGraph::disableExportToGraphViz();
    else                       returnVal = invalidValue (option, value);

  } else if (option == "print_fg") {
    if      (value == "true")  FactorGraph::enablePrintFactorGraph();
    else if (value == "false") FactorGraph::disablePrintFactorGraph();
    else                       returnVal = invalidValue (option, value);

  } else {
    std::cerr << "Warning: invalid option `" << option << "'" << std::endl;
    returnVal = false;
  }
  return returnVal;
}



void
printHeader (std::string header, std::ostream& os)
{
  printAsteriskLine (os);
  os << header << std::endl;
  printAsteriskLine (os);
}



void
printSubHeader (std::string header, std::ostream& os)
{
  printDashedLine (os);
  os << header << std::endl;
  printDashedLine (os);
}



void
printAsteriskLine (std::ostream& os)
{
  os << "********************************" ;
  os << "********************************" ;
  os << std::endl;
}



void
printDashedLine (std::ostream& os)
{
  os << "--------------------------------" ;
  os << "--------------------------------" ;
  os << std::endl;
}

}  // namespace Util



namespace LogAware {

void
normalize (Params& v)
{
  if (Globals::logDomain) {
    double sum = std::accumulate (v.begin(), v.end(),
        LogAware::addIdenty(), Util::logSum);
    assert (sum != -std::numeric_limits<double>::infinity());
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
        std::plus<double>(), FuncObj::abs_diff_exp<double>());
  } else {
    dist = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        std::plus<double>(), FuncObj::abs_diff<double>());
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
        FuncObj::max<double>(), FuncObj::abs_diff_exp<double>());
  } else {
    max = std::inner_product (v1.begin(), v1.end(), v2.begin(), 0.0,
        FuncObj::max<double>(), FuncObj::abs_diff<double>());
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

}  // namespace LogAware

}  // namespace Horus


