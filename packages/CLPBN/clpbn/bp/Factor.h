#ifndef HORUS_FACTOR_H
#define HORUS_FACTOR_H

#include <vector>

#include "Var.h"
#include "Indexer.h"
#include "Util.h"


using namespace std;


template <typename T>
class TFactor
{
  public:
    const vector<T>& arguments (void) const { return args_; }

    vector<T>& arguments (void) { return args_; }

    const Ranges& ranges (void) const { return ranges_; }

    const Params& params (void) const { return params_; }

    Params& params (void) { return params_; }

    unsigned nrArguments (void) const { return args_.size(); }

    unsigned size (void) const { return params_.size(); }

    unsigned distId (void) const { return distId_; }

    void setDistId (unsigned id) { distId_ = id; }

    void setParams (const Params& newParams)
    {
      params_ = newParams;
      assert (params_.size() == Util::expectedSize (ranges_));
    }

    void normalize (void)
    {
      LogAware::normalize (params_);
    }

    int indexOf (const T& t) const
    {
      int idx = -1;
      for (unsigned i = 0; i < args_.size(); i++) {
        if (args_[i] == t) {
          idx = i;
          break;
        }
      }
      return idx;
    }

    const T& argument (unsigned idx) const
    {
      assert (idx < args_.size());
      return args_[idx];
    }

    T& argument (unsigned idx)
    {
      assert (idx < args_.size());
      return args_[idx];
    }

    unsigned range (unsigned idx) const
    {
      assert (idx < ranges_.size());
      return ranges_[idx];
    }

    void multiply (TFactor<T>& g)
    {
      const vector<T>& g_args = g.arguments();
      const Ranges& g_ranges  = g.ranges();
      const Params& g_params  = g.params();
      if (args_ == g_args) {
        // optimization: if the factors contain the same set of args,
        // we can do a 1 to 1 operation on the parameters
        if (Globals::logDomain) {
          Util::add (params_, g_params);
        } else {
          Util::multiply (params_, g_params);
        }
      } else {
        bool sharedArgs = false;
        vector<unsigned> gvarpos;
        for (unsigned i = 0; i < g_args.size(); i++) {
          int idx = indexOf (g_args[i]);
          if (idx == -1) {
            insertArgument (g_args[i], g_ranges[i]);
            gvarpos.push_back (args_.size() - 1);
          } else {
            sharedArgs = true;
            gvarpos.push_back (idx);
          }
        }
        if (sharedArgs == false) {
          // optimization: if the original factors doesn't have common args,
          // we don't need to marry the states of the common args
          unsigned count = 0;
          for (unsigned i = 0; i < params_.size(); i++) {
            if (Globals::logDomain) {
              params_[i] += g_params[count];
            } else {
              params_[i] *= g_params[count];
            }
            count ++;
            if (count >= g_params.size()) {
              count = 0;
            }
          }
        } else {
          StatesIndexer indexer (ranges_, false);
          while (indexer.valid()) {
            unsigned g_li = 0;
            unsigned prod = 1;
            for (int j = gvarpos.size() - 1; j >= 0; j--) {
              g_li += indexer[gvarpos[j]] * prod;
              prod *= g_ranges[j];
            }
            if (Globals::logDomain) {
              params_[indexer] += g_params[g_li];
            } else {
              params_[indexer] *= g_params[g_li];
            }
            ++ indexer;
          }
        }
      }
    }

    void absorveEvidence (const T& arg, unsigned evidence)
    {
      int idx = indexOf (arg);
      assert (idx != -1);
      assert (evidence < ranges_[idx]);
      Params copy = params_;
      params_.clear();
      params_.reserve (copy.size() / ranges_[idx]);
      StatesIndexer indexer (ranges_);
      for (unsigned i = 0; i < evidence; i++) {
        indexer.increment (idx);
      }
      while (indexer.valid()) {
       params_.push_back (copy[indexer]);
       indexer.incrementExcluding (idx);
      }
      args_.erase (args_.begin() + idx);
      ranges_.erase (ranges_.begin() + idx);
    }

    void reorderArguments (const vector<T> newArgs)
    {
      assert (newArgs.size() == args_.size());
      if (newArgs == args_) {
        return; // already in the wanted order
      }
      Ranges newRanges;
      vector<unsigned> positions;
      for (unsigned i = 0; i < newArgs.size(); i++) {
        unsigned idx = indexOf (newArgs[i]);
        newRanges.push_back (ranges_[idx]);
        positions.push_back (idx);
      }
      unsigned N = ranges_.size();
      Params newParams (params_.size());
      for (unsigned i = 0; i < params_.size(); i++) {
        unsigned li = i;
        // calculate vector index corresponding to linear index
        vector<unsigned> vi (N);
        for (int k = N-1; k >= 0; k--) {
          vi[k] = li % ranges_[k];
          li /= ranges_[k];
        }
        // convert permuted vector index to corresponding linear index
        unsigned prod = 1;
        unsigned new_li = 0;
        for (int k = N - 1; k >= 0; k--) {
          new_li += vi[positions[k]] * prod;
          prod   *= ranges_[positions[k]];
        }
        newParams[new_li] = params_[i];
      }
      args_    = newArgs;
      ranges_  = newRanges;
      params_  = newParams;
    }

    bool contains (const T& arg) const
    {
      return Util::contains (args_, arg);
    }

    bool contains (const vector<T>& args) const
    {
      for (unsigned i = 0; i < args_.size(); i++) {
        if (contains (args[i]) == false) {
          return false;
        }
      }
      return true;
    }

  protected:
    vector<T>  args_;
    Ranges     ranges_;
    Params     params_;
    unsigned   distId_;
  
  private:
    void insertArgument (const T& arg, unsigned range)
    {
      assert (indexOf (arg) == -1);
      Params copy = params_;
      params_.clear();
      params_.reserve (copy.size() * range);
      for (unsigned i = 0; i < copy.size(); i++) {
        for (unsigned reps = 0; reps < range; reps++) {
          params_.push_back (copy[i]);
        }
      }
      args_.push_back (arg);
      ranges_.push_back (range);
    }

    void insertArguments (const vector<T>& args, const Ranges& ranges)
    {
      Params copy = params_;
      unsigned nrStates = 1;
      for (unsigned i = 0; i < args.size(); i++) {
        assert (indexOf (args[i]) == -1);
        args_.push_back (args[i]);
        ranges_.push_back (ranges[i]);
        nrStates *= ranges[i];
      }
      params_.clear();
      params_.reserve (copy.size() * nrStates);
      for (unsigned i = 0; i < copy.size(); i++) {
        for (unsigned reps = 0; reps < nrStates; reps++) {
          params_.push_back (copy[i]);
        }
      }
   }
};



class Factor : public TFactor<VarId>
{
  public:
    Factor (void) { }

    Factor (const Factor&);

    Factor (VarId, unsigned);

    Factor (const Vars&);

    Factor (VarId, unsigned, const Params&);

    Factor (const Vars&, const Params&,
        unsigned = Util::maxUnsigned());

    Factor (const VarIds&, const Ranges&, const Params&,
        unsigned = Util::maxUnsigned());

    void sumOutAllExcept (VarId);

    void sumOutAllExcept (const VarIds&);

    void sumOut (VarId);

    void sumOutFirstVariable (void);

    void sumOutLastVariable (void);

    void multiply (Factor&);

    void reorderAccordingVarIds (void);

    string getLabel (void) const;

    void print (void) const;

  private:
    void copyFromFactor (const Factor& f);

};

#endif // HORUS_FACTOR_H

