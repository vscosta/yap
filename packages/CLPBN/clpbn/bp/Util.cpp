#include <sstream>

#include "BayesNet.h"
#include "VarNode.h"
#include "Shared.h"
#include "StatesIndexer.h"

namespace InfAlgorithms {
InfAlgs infAlgorithm = InfAlgorithms::VE;
//InfAlgs infAlgorithm = InfAlgorithms::BN_BP;
//InfAlgs infAlgorithm = InfAlgorithms::FG_BP;
//InfAlgs infAlgorithm = InfAlgorithms::CBP;
}


namespace BpOptions {
Schedule schedule = BpOptions::Schedule::SEQ_FIXED;
//Schedule schedule = BpOptions::Schedule::SEQ_RANDOM;
//Schedule schedule = BpOptions::Schedule::PARALLEL;
//Schedule schedule = BpOptions::Schedule::MAX_RESIDUAL;
double    accuracy              = 0.0001;
unsigned  maxIter               = 1000;
bool      useAlwaysLoopySolver  = true;
}

NumberSpace::ns NSPACE = NumberSpace::NORMAL;

unordered_map<VarId,VariableInfo> GraphicalModel::varsInfo_;

vector<NetInfo>      Statistics::netInfo_;
vector<CompressInfo> Statistics::compressInfo_;
unsigned             Statistics::primaryNetCount_;


namespace Util {

void
toLog (ParamSet& v)
{
  for (unsigned i = 0; i < v.size(); i++) {
    v[i] = log (v[i]);
  }
}



void
fromLog (ParamSet& v)
{
  for (unsigned i = 0; i < v.size(); i++) {
    v[i] = exp (v[i]);
  }
}



void
normalize (ParamSet& v)
{
  double sum;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      sum = 0.0;
      for (unsigned i = 0; i < v.size(); i++) {
        sum += v[i];
      }
      assert (sum != 0.0);
      for (unsigned i = 0; i < v.size(); i++) {
        v[i] /= sum;
      }
      break;
    case NumberSpace::LOGARITHM:
      sum = addIdenty();
      for (unsigned i = 0; i < v.size(); i++) {
        logSum (sum, v[i]);
      }
      assert (sum != -numeric_limits<Param>::infinity());
      for (unsigned i = 0; i < v.size(); i++) {
        v[i] -= sum;
      }
  }
}



void
pow (ParamSet& v, unsigned expoent)
{
  if (expoent == 1) {
    return; // optimization
  }
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned i = 0; i < v.size(); i++) {
        double value = 1.0;
         for (unsigned j = 0; j < expoent; j++) {
           value *= v[i];
         }  
        v[i] = value;
      }
      break;
    case NumberSpace::LOGARITHM:
      for (unsigned i = 0; i < v.size(); i++) {
        v[i] *= expoent;
      }
  }
}



Param
pow (Param p, unsigned expoent)
{
  double value = 1.0;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned i = 0; i < expoent; i++) {
        value *= p;
      }
      break;
    case NumberSpace::LOGARITHM:
      value =  p * expoent;
  }
  return value;
}



double
getL1Distance (const ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  double dist = 0.0;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned i = 0; i < v1.size(); i++) {
        dist += abs (v1[i] - v2[i]);
      }
      break;
    case NumberSpace::LOGARITHM:
      for (unsigned i = 0; i < v1.size(); i++) {
        dist += abs (exp(v1[i]) - exp(v2[i]));
      }
  }
  return dist;
}



double
getMaxNorm (const ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  double max = 0.0;
  switch (NSPACE) {
    case NumberSpace::NORMAL:
      for (unsigned i = 0; i < v1.size(); i++) {
        double diff = abs (v1[i] - v2[i]);
        if (diff > max) {
          max = diff;
        }
      }
      break;
    case NumberSpace::LOGARITHM:
      for (unsigned i = 0; i < v1.size(); i++) {
        double diff = abs (exp(v1[i]) - exp(v2[i]));
        if (diff > max) {
          max = diff;
        }
      }
  }
  return max;
}



unsigned
getNumberOfDigits (int number) {
  unsigned count = 1;
  while (number >= 10) {
    number /= 10; 
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
parametersToString (const ParamSet& v, unsigned precision)
{
  stringstream ss;
  ss.precision (precision);
  ss << "[" ; 
  for (unsigned i = 0; i < v.size() - 1; i++) {
    ss << v[i] << ", " ;
  }
  if (v.size() != 0) {
    ss << v[v.size() - 1];
  }
  ss << "]" ;
  return ss.str();
}



BayesNet*
generateBayesianNetworkTreeWithLevel (unsigned level)
{
  BayesNet* bn = new BayesNet();
  Distribution* dist = new Distribution (ParamSet() = {0.1, 0.5, 0.2, 0.7});
  BayesNode* root = bn->addNode (0, 2, -1, BnNodeSet() = {},
      new Distribution (ParamSet() = {0.1, 0.5}));
  BnNodeSet prevLevel = { root };
  BnNodeSet currLevel;
  VarId vidCount = 1;
  for (unsigned l = 1; l < level; l++) {
    currLevel.clear();
    for (unsigned i = 0; i < prevLevel.size(); i++) {
      currLevel.push_back (
          bn->addNode (vidCount, 2, -1, BnNodeSet() = {prevLevel[i]}, dist));
      vidCount ++;
      currLevel.push_back (
          bn->addNode (vidCount, 2, -1, BnNodeSet() = {prevLevel[i]}, dist));
      vidCount ++;
    }
    prevLevel = currLevel;
  }
  for (unsigned i = 0; i < prevLevel.size(); i++) {
    prevLevel[i]->setEvidence (0);
  }
  bn->setIndexes();
  return bn;
}



vector<DConf>
getDomainConfigurations (const VarNodes& vars)
{
  // TODO this method must die
  unsigned nConfs = 1;
  for (unsigned i = 0; i < vars.size(); i++) {
    nConfs *= vars[i]->nrStates();
  }

  vector<DConf> confs (nConfs);
  for (unsigned i = 0; i < nConfs; i++) {
    confs[i].resize (vars.size());
  }

  unsigned nReps = 1;
  for (int i = vars.size() - 1; i >= 0; i--) {
    unsigned index = 0;
    while (index < nConfs) {
      for (unsigned j = 0; j < vars[i]->nrStates(); j++) {
        for (unsigned r = 0; r < nReps; r++) {
          confs[index][i] = j; 
          index++;
        }
      }
    }
    nReps *= vars[i]->nrStates();
  }
  return confs;
}



vector<string>
getJointStateStrings (const VarNodes& vars)
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
Statistics::updateStatistics (unsigned size, bool loopy,
                              unsigned nIters, double time)
{
  netInfo_.push_back (NetInfo (size, loopy, nIters, time));
}

  

void
Statistics::printStatistics (void)
{
  cout << getStatisticString();
}



void
Statistics::writeStatisticsToFile (const char* fileName)
{
  ofstream out (fileName);
  if (!out.is_open()) {
    cerr << "error: cannot open file to write at " ;
    cerr << "Statistics::writeStatisticsToFile()" << endl;
    abort();
  }
  out << getStatisticString();
  out.close();
}



void
Statistics::updateCompressingStatistics (unsigned nGroundVars,
                                         unsigned nGroundFactors,
                                         unsigned nClusterVars, 
                                         unsigned nClusterFactors,
                                         unsigned nWithoutNeighs) {
  compressInfo_.push_back (CompressInfo (nGroundVars, nGroundFactors,
      nClusterVars, nClusterFactors, nWithoutNeighs));
}



string
Statistics::getStatisticString (void)
{
  stringstream ss2, ss3, ss4, ss1;
  ss1 << "running mode:          " ;
  switch (InfAlgorithms::infAlgorithm) {
    case InfAlgorithms::VE:    ss1 << "ve"     << endl;  break;
    case InfAlgorithms::BN_BP: ss1 << "bn_bp"  << endl;  break;
    case InfAlgorithms::FG_BP: ss1 << "fg_bp"  << endl;  break;
    case InfAlgorithms::CBP:   ss1 << "cbp"    << endl;  break;
  }
  ss1 << "message schedule:      " ;
  switch (BpOptions::schedule) {
    case BpOptions::Schedule::SEQ_FIXED:    ss1 << "sequential fixed"  << endl;  break;
    case BpOptions::Schedule::SEQ_RANDOM:   ss1 << "sequential random" << endl;  break;
    case BpOptions::Schedule::PARALLEL:     ss1 << "parallel"          << endl;  break;
    case BpOptions::Schedule::MAX_RESIDUAL: ss1 << "max residual"      << endl;  break;
  }
  ss1 << "max iterations:        " << BpOptions::maxIter  << endl;
  ss1 << "accuracy               " << BpOptions::accuracy << endl;
  if (BpOptions::useAlwaysLoopySolver) {
    ss1 << "always loopy solver:   yes" << endl;
  } else {
    ss1 << "always loopy solver:   no" << endl;
  }
  ss1 << endl << endl;

  ss2 << "---------------------------------------------------" << endl;
  ss2 << " Network information" << endl;
  ss2 << "---------------------------------------------------" << endl;
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
    ss3 << "---------------------------------------------------" << endl;
    ss3 << " Compression information" << endl;
    ss3 << "---------------------------------------------------" << endl;
    ss3 << left;
    ss3 << "Ground   Cluster   Ground    Cluster   Neighborless" << endl;
    ss3 << "Vars     Vars      Factors   Factors   Vars"         << endl;
    for (unsigned i = 0; i < compressInfo_.size(); i++) {
      ss3 << setw (9) << compressInfo_[i].nGroundVars;
      ss3 << setw (10) << compressInfo_[i].nClusterVars;
      ss3 << setw (10) << compressInfo_[i].nGroundFactors;
      ss3 << setw (10) << compressInfo_[i].nClusterFactors;
      ss3 << setw (10) << compressInfo_[i].nWithoutNeighs;
      ss3 << endl;
      c1 += compressInfo_[i].nGroundVars - compressInfo_[i].nWithoutNeighs;
      c2 += compressInfo_[i].nClusterVars;
      c3 += compressInfo_[i].nGroundFactors - compressInfo_[i].nWithoutNeighs;
      c4 += compressInfo_[i].nClusterFactors;
      if (compressInfo_[i].nWithoutNeighs != 0) {
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

