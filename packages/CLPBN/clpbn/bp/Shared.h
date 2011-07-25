#ifndef BP_SHARED_H
#define BP_SHARED_H

#include <cmath>
#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <map>
#include <unordered_map>

// Macro to disallow the copy constructor and operator= functions
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

using namespace std;

class Variable;
class BayesNode;
class FgVarNode;

typedef double                             Param;
typedef vector<Param>                      ParamSet;
typedef vector<Param>                      Message;
typedef vector<Variable*>                  VarSet;
typedef vector<BayesNode*>                 NodeSet;
typedef vector<FgVarNode*>                 FgVarSet;
typedef vector<string>                     Domain;
typedef vector<unsigned>                   DomainConf;
typedef pair<unsigned, unsigned>           DomainConstr;
typedef unordered_map<unsigned, unsigned>  IndexMap;


//extern unsigned DL;
static const unsigned DL = 0;

// number of digits to show when printing a parameter
static const unsigned PRECISION = 10;

// shared by bp and sp solver
enum Schedule
{
  S_SEQ_FIXED,
  S_SEQ_RANDOM,
  S_PARALLEL,
  S_MAX_RESIDUAL
};


struct NetInfo
{
  NetInfo (unsigned c, double t)
  { 
    counting    = c;
    solvingTime = t;
  }
  unsigned  counting;
  double    solvingTime;
};

typedef map<unsigned, NetInfo> StatisticMap; 


class Statistics
{
  public:

    static void updateStats (unsigned size, double time)
    {
      StatisticMap::iterator it  = stats_.find(size);
      if (it == stats_.end()) {
        stats_.insert (make_pair (size, NetInfo (1, 0.0)));
      } else {
        it->second.counting ++;
        it->second.solvingTime += time;
      }
    }

    static unsigned getCounting (unsigned size)
    {
      StatisticMap::iterator it = stats_.find(size);
      assert (it != stats_.end());
      return it->second.counting;
    }

    static void updateIterations (unsigned nIters)
    {
      totalOfIterations += nIters;
      if (nIters > maxIterations) { 
        maxIterations = nIters;
      }
    }

    static void writeStats (void)
    {
      ofstream out ("../../stats.txt");
      if (!out.is_open()) {
        cerr << "error: cannot open file to write at " ;
        cerr << "Statistics:::updateStats()" << endl;
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
      out << "Network Size\tCounting\tSolving Time\tAverage Time" << endl;
      for (StatisticMap::iterator it = stats_.begin();
          it != stats_.end(); it++) {
        out << it->first;
        out << "\t\t" << it->second.counting;
        out << "\t\t" << it->second.solvingTime;
        if (it->second.counting > 0) {
          out << "\t\t" << it->second.solvingTime / it->second.counting;
        } else {
          out << "\t\t0.0" ;
        }
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

};



class Util
{
  public:
    static void normalize (ParamSet& v)
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

    static double getL1dist (const ParamSet& v1, const ParamSet& v2)
    {
      assert (v1.size() == v2.size());
      double dist = 0.0;
      for (unsigned i = 0; i < v1.size(); i++) {
        dist += abs (v1[i] - v2[i]);
      }
      return dist;
    }

    static double getMaxNorm (const ParamSet& v1, const ParamSet& v2)
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

    static bool isInteger (const string& s)
    {
      stringstream ss1 (s);
      stringstream ss2;
      int integer;
      ss1 >> integer;
      ss2 << integer;
      return (ss1.str() == ss2.str());
    }
};


//unsigned Statistics::totalOfIterations  = 0;

#endif

