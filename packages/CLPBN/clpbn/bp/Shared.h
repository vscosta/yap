#ifndef BP_SHARED_H
#define BP_SHARED_H

#include <cmath>
#include <cassert>
#include <vector>
#include <map>
#include <unordered_map>

#include <iostream>
#include <fstream>
#include <iomanip>

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class Variable;
class BayesNode;
class FgVarNode;
class Factor;
class Link;
class Edge;

typedef double                             Param;
typedef vector<Param>                      ParamSet;
typedef const ParamSet&                    CParamSet;
typedef unsigned                           Vid;
typedef vector<Vid>                        VidSet;
typedef const VidSet&                      CVidSet;
typedef vector<Variable*>                  VarSet;
typedef vector<BayesNode*>                 BnNodeSet;
typedef const BnNodeSet&                   CBnNodeSet;
typedef vector<FgVarNode*>                 FgVarSet;
typedef const FgVarSet&                    CFgVarSet;
typedef vector<Factor*>                    FactorSet;
typedef const FactorSet&                   CFactorSet;
typedef vector<Link*>                      LinkSet;
typedef const LinkSet&                     CLinkSet;
typedef vector<Edge*>                      EdgeSet;
typedef const EdgeSet&                     CEdgeSet;
typedef vector<string>                     Domain;
typedef vector<unsigned>                   DConf;
typedef pair<unsigned, unsigned>           DConstraint;
typedef map<unsigned, unsigned> IndexMap;

// level of debug information
static const unsigned DL = 0;

static const int NO_EVIDENCE = -1;

// number of digits to show when printing a parameter
static const unsigned PRECISION = 5;

static const bool     EXPORT_TO_DOT   = false;
static const unsigned EXPORT_MIN_SIZE = 30;


namespace SolverOptions
{
  enum Schedule
  {
    S_SEQ_FIXED,
    S_SEQ_RANDOM,
    S_PARALLEL,
    S_MAX_RESIDUAL
  };
  extern bool      runBayesBall;
  extern bool      convertBn2Fg;
  extern bool      compressFactorGraph;
  extern Schedule  schedule;
  extern double    accuracy;
  extern unsigned  maxIter;
}


namespace Util
{
  void normalize (ParamSet&);
  void pow (ParamSet&, unsigned);
  double getL1dist (CParamSet, CParamSet);
  double getMaxNorm (CParamSet, CParamSet);
  bool isInteger (const string&);
  string parametersToString (CParamSet);
  vector<DConf> getDomainConfigurations (const VarSet&);
  vector<string> getInstantiations (const VarSet&);
};


struct NetInfo
{
  NetInfo (void)
  { 
    counting    = 0;
    nIters      = 0;
    solvingTime = 0.0;
  }
  unsigned  counting;
  double    solvingTime;
  unsigned  nIters;
};


struct CompressInfo
{ 
  CompressInfo (unsigned a, unsigned b, unsigned c,
                unsigned d, unsigned e) {
    nUncVars     = a; 
    nUncFactors  = b; 
    nCompVars    = c;
    nCompFactors = d;
    nNeighborlessVars = e;
  }
  unsigned nUncVars;
  unsigned nUncFactors;
  unsigned nCompVars;
  unsigned nCompFactors;
  unsigned nNeighborlessVars;
};


typedef map<unsigned, NetInfo> StatisticMap; 
class Statistics
{
  public:

    static void updateStats (unsigned size, unsigned nIters, double time)
    {
      StatisticMap::iterator it  = stats_.find (size);
      if (it == stats_.end()) {
        it = (stats_.insert (make_pair (size, NetInfo()))).first;
      } else {
        it->second.counting ++;
        it->second.nIters += nIters;
        it->second.solvingTime += time;
        totalOfIterations += nIters;
          if (nIters > maxIterations) { 
          maxIterations = nIters;
        }
      }
    }

    static void updateCompressingStats (unsigned nUncVars,
                                        unsigned nUncFactors,
                                        unsigned nCompVars, 
                                        unsigned nCompFactors,
                                        unsigned nNeighborlessVars) {
      compressInfo_.push_back (CompressInfo (
          nUncVars, nUncFactors, nCompVars, nCompFactors, nNeighborlessVars));
    }

    static void printCompressingStats (const char* fileName)
    {
      ofstream out (fileName);
      if (!out.is_open()) {
        cerr << "error: cannot open file to write at " ;
        cerr << "BayesNet::printCompressingStats()" << endl;
        abort();
      }
      out << "--------------------------------------" ;
      out << "--------------------------------------" << endl;
      out << " Compression Stats" << endl;
      out << "--------------------------------------" ;
      out << "--------------------------------------" << endl;
      out << left;
      out << "Uncompress   Compressed   Uncompress   Compressed   Neighborless";
      out << endl;
      out << "Vars         Vars         Factors      Factors      Vars" ;
      out << endl;
      for (unsigned i = 0; i < compressInfo_.size(); i++) {
        out << setw (13) << compressInfo_[i].nUncVars;
        out << setw (13) << compressInfo_[i].nCompVars;
        out << setw (13) << compressInfo_[i].nUncFactors;
        out << setw (13) << compressInfo_[i].nCompFactors;
        out << setw (13) << compressInfo_[i].nNeighborlessVars;
        out << endl;
      }
    }

    static unsigned getCounting (unsigned size)
    {
      StatisticMap::iterator it = stats_.find(size);
      assert (it != stats_.end());
      return it->second.counting;
    }

    static void writeStats (void)
    {
      ofstream out ("../../stats.txt");
      if (!out.is_open()) {
        cerr << "error: cannot open file to write at " ;
        cerr << "Statistics::updateStats()" << endl;
        abort();
      }
      unsigned avgIterations = 0;
      if (numSolvedLoopyNets > 0) {
        avgIterations = totalOfIterations / numSolvedLoopyNets;
      }
      double totalSolvingTime = 0.0;
      for (StatisticMap::iterator it = stats_.begin();
          it != stats_.end(); it++) {
        totalSolvingTime += it->second.solvingTime;
      }
      out << "created networks:              " << numCreatedNets     << endl;
      out << "solver runs on polytrees:      " << numSolvedPolyTrees << endl;
      out << "solver runs on loopy networks: " << numSolvedLoopyNets << endl;
      out << "   unconverged:                " << numUnconvergedRuns << endl;
      out << "   max iterations:             " << maxIterations      << endl;
      out << "   average iterations:         " << avgIterations      << endl;
      out << "total solving time             " << totalSolvingTime   << endl;
      out << endl;
      out << left << endl;
      out << setw (15) << "Network Size" ;
      out << setw (15) << "Counting" ;
      out << setw (15) << "Solving Time" ;
      out << setw (15) << "Average Time" ;
      out << setw (15) << "#Iterations" ;
      out << endl;
      for (StatisticMap::iterator it = stats_.begin();
          it != stats_.end(); it++) {
        out << setw (15) << it->first;
        out << setw (15) << it->second.counting;
        out << setw (15) << it->second.solvingTime;
        if (it->second.counting > 0) {
          out << setw (15) << it->second.solvingTime / it->second.counting;
        } else {
          out << setw (15) << "0.0" ;
        }
        out << setw (15) << it->second.nIters;
        out << endl;
      }
      out.close();
    }

    static unsigned numCreatedNets;
    static unsigned numSolvedPolyTrees;
    static unsigned numSolvedLoopyNets;
    static unsigned numUnconvergedRuns;

  private:
    static StatisticMap stats_;
    static unsigned maxIterations;
    static unsigned totalOfIterations;
    static vector<CompressInfo> compressInfo_;
};

#endif //BP_SHARED_H

