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

    size_t nrArguments (void) const { return args_.size(); }

    size_t size (void) const { return params_.size(); }

    unsigned distId (void) const { return distId_; }

    void setDistId (unsigned id) { distId_ = id; }

    void normalize (void) { LogAware::normalize (params_); }

    void setParams (const Params& newParams)
    {
      params_ = newParams;
      assert (params_.size() == Util::sizeExpected (ranges_));
    }

    size_t indexOf (const T& t) const
    {
      return Util::indexOf (args_, t);
    }

    const T& argument (size_t idx) const
    {
      assert (idx < args_.size());
      return args_[idx];
    }

    T& argument (size_t idx)
    {
      assert (idx < args_.size());
      return args_[idx];
    }

    unsigned range (size_t idx) const
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
        Globals::logDomain ? params_ += g_params
                           : params_ *= g_params;
      } else {
        bool sharedArgs = false;
        vector<size_t> gvarpos;
        for (size_t i = 0; i < g_args.size(); i++) {
          size_t idx = indexOf (g_args[i]);
          if (idx == g_args.size()) {
            ullong newSize = params_.size() * g_ranges[i];
            if (newSize > params_.max_size()) {
              cerr << "error: an overflow occurred on factor multiplication" ;
              cerr << endl;
              abort();
            }
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
          size_t count = 0;
          for (size_t i = 0; i < params_.size(); i++) {
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
          Indexer indexer (ranges_, false);
          while (indexer.valid()) {
            size_t g_li = 0;
            size_t prod = 1;
            for (size_t j = gvarpos.size(); j-- > 0; ) {
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

    void sumOutIndex (size_t idx)
    {
      assert (idx < args_.size());
      assert (args_.size() > 1);
      size_t new_size = params_.size() / ranges_[idx];
      Params newps (new_size, LogAware::addIdenty());
      Params::const_iterator first = params_.begin();
      Params::const_iterator last  = params_.end();
      CutIndexer indexer (ranges_, idx);
      if (Globals::logDomain) {
        while (first != last) {
          newps[indexer] = Util::logSum (newps[indexer], *first++);
          ++ indexer;
        }
      } else {
        while (first != last) {
          newps[indexer] += *first++;
          ++ indexer;
        }
      }
      params_ = newps;
      args_.erase (args_.begin() + idx);
      ranges_.erase (ranges_.begin() + idx);
    }

    void absorveEvidence (const T& arg, unsigned evidence)
    {
      size_t idx = indexOf (arg);
      assert (idx != args_.size());
      assert (evidence < ranges_[idx]);
      Params newps;
      newps.reserve (params_.size() / ranges_[idx]); 
      Indexer indexer (ranges_);
      for (unsigned i = 0; i < evidence; i++) {
        indexer.incrementDimension (idx);
      }
      while (indexer.valid()) {
       newps.push_back (params_[indexer]);
       indexer.incrementExceptDimension (idx);
      }
      params_ = newps;
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
      vector<size_t> positions;
      for (size_t i = 0; i < newArgs.size(); i++) {
        size_t idx = indexOf (newArgs[i]);
        newRanges.push_back (ranges_[idx]);
        positions.push_back (idx);
      }
      size_t N = ranges_.size();
      Params newParams (params_.size());
      for (size_t i = 0; i < params_.size(); i++) {
        size_t li = i;
        // calculate vector index corresponding to linear index
        vector<unsigned> vi (N);
        for (int k = N-1; k >= 0; k--) {
          vi[k] = li % ranges_[k];
          li /= ranges_[k];
        }
        // convert permuted vector index to corresponding linear index
        size_t prod = 1;
        size_t new_li = 0;
        for (int k = N - 1; k >= 0; k--) {
          new_li += vi[positions[k]] * prod;
          prod   *= ranges_[positions[k]];
        }
        newParams[new_li] = params_[i];
      }
      args_   = newArgs;
      ranges_ = newRanges;
      params_ = newParams;
    }

    bool contains (const T& arg) const
    {
      return Util::contains (args_, arg);
    }

    bool contains (const vector<T>& args) const
    {
      for (size_t i = 0; i < args_.size(); i++) {
        if (contains (args[i]) == false) {
          return false;
        }
      }
      return true;
    }

    double& operator[] (size_t idx)
    {
      assert (idx < params_.size());
      return params_[idx];
    }


  protected:
    vector<T>  args_;
    Ranges     ranges_;
    Params     params_;
    unsigned   distId_;
  
  private:
    void insertArgument (const T& arg, unsigned range)
    {
      assert (indexOf (arg) == args_.size());
      Params copy = params_;
      params_.clear();
      params_.reserve (copy.size() * range);
      for (size_t i = 0; i < copy.size(); i++) {
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
      for (size_t i = 0; i < args.size(); i++) {
        assert (indexOf (args[i]) == args_.size());
        args_.push_back (args[i]);
        ranges_.push_back (ranges[i]);
        nrStates *= ranges[i];
      }
      params_.clear();
      params_.reserve (copy.size() * nrStates);
      for (size_t i = 0; i < copy.size(); i++) {
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

    Factor (const VarIds&, const Ranges&, const Params&,
        unsigned = Util::maxUnsigned());

    Factor (const Vars&, const Params&,
        unsigned = Util::maxUnsigned());

    void sumOut (VarId);

    void sumOutAllExcept (VarId);

    void sumOutAllExcept (const VarIds&);

    void sumOutAllExceptIndex (size_t idx);

    void multiply (Factor&);

    void reorderAccordingVarIds (void);

    string getLabel (void) const;

    void print (void) const;

  private:
    void sumOutFirstVariable (void);

    void sumOutLastVariable (void);

    void sumOutArgs (const vector<bool>& mask);
   
    void clone (const Factor& f);

};

#endif // HORUS_FACTOR_H

