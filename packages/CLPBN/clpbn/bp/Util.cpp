#include <sstream>

#include "Variable.h"
#include "Shared.h"

namespace SolverOptions {

bool runBayesBall         = false;
bool convertBn2Fg         = true;
bool compressFactorGraph  = true;
Schedule schedule = S_SEQ_FIXED;
//Schedule schedule = S_SEQ_RANDOM;
//Schedule schedule = S_PARALLEL;
//Schedule schedule = S_MAX_RESIDUAL;
double accuracy  = 0.0001;
unsigned maxIter = 1000; //FIXME

}



unsigned Statistics::numCreatedNets      = 0;
unsigned Statistics::numSolvedPolyTrees  = 0;
unsigned Statistics::numSolvedLoopyNets  = 0;
unsigned Statistics::numUnconvergedRuns  = 0;
unsigned Statistics::maxIterations       = 0;
unsigned Statistics::totalOfIterations   = 0;
vector<CompressInfo> Statistics::compressInfo_;
StatisticMap Statistics::stats_;



namespace Util {

void
normalize (ParamSet& v)
{
  double sum = 0.0;
  for (unsigned i = 0; i < v.size(); i++) {
    sum += v[i];
  }
  assert (sum != 0.0);
  for (unsigned i = 0; i < v.size(); i++) {
    v[i] /= sum;
  }
}


void
pow (ParamSet& v, unsigned expoent)
{
  for (unsigned i = 0; i < v.size(); i++) {
    double value = 1;
    for (unsigned j = 0; j < expoent; j++) {
      value *= v[i];
    }
    v[i] = value;
  }
}


double
getL1dist (const ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  double dist = 0.0;
  for (unsigned i = 0; i < v1.size(); i++) {
    dist += abs (v1[i] - v2[i]);
  }
  return dist;
}


double
getMaxNorm (const ParamSet& v1, const ParamSet& v2)
{
  assert (v1.size() == v2.size());
  double max = 0.0;
  for (unsigned i = 0; i < v1.size(); i++) {
    double diff = abs (v1[i] - v2[i]);
    if (diff > max) {
      max = diff;
    }
  }
  return max;
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
parametersToString (CParamSet v)
{
  stringstream ss;  
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



vector<DConf>
getDomainConfigurations (const VarSet& vars)
{
  unsigned nConfs = 1;
  for (unsigned i = 0; i < vars.size(); i++) {
    nConfs *= vars[i]->getDomainSize();
  }

  vector<DConf> confs (nConfs);
  for (unsigned i = 0; i < nConfs; i++) {
    confs[i].resize (vars.size());
  }

  unsigned nReps = 1;
  for (int i = vars.size() - 1; i >= 0; i--) {
    unsigned index = 0;
    while (index < nConfs) {
      for (unsigned j = 0; j < vars[i]->getDomainSize(); j++) {
        for (unsigned r = 0; r < nReps; r++) {
          confs[index][i] = j; 
          index++;
        }
      }
    }
    nReps *= vars[i]->getDomainSize();
  }
  return confs;
}


vector<string>
getInstantiations (const VarSet& vars)
{
  //FIXME handle variables without domain
  /*
  char c = 'a' ;
  const DConf& conf = entries[i].getDomainConfiguration();
  for (unsigned j = 0; j < conf.size(); j++) {
    if (j != 0) ss << "," ;
    ss << c << conf[j] + 1;
    c ++;
  }
  */
  unsigned rowSize  = 1;
  for (unsigned i = 0; i < vars.size(); i++) {
    rowSize *= vars[i]->getDomainSize();
  }

  vector<string> headers (rowSize);

  unsigned nReps = 1;
  for (int i = vars.size() - 1; i >= 0; i--) {
    Domain domain = vars[i]->getDomain();
    unsigned index = 0;
    while (index < rowSize) {
      for (unsigned j = 0; j < vars[i]->getDomainSize(); j++) {
        for (unsigned r = 0; r < nReps; r++) {
          if (headers[index] != "") {
            headers[index] = domain[j] + ", " + headers[index];
          } else {
            headers[index] = domain[j];
          }
          index++;
        }
      }
    }
    nReps *= vars[i]->getDomainSize();
  }
  return headers;
}

}

