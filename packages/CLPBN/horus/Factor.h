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
  
    void randomize (void)
    {
      for (size_t i = 0; i < params_.size(); ++i) {
        params_[i] = (double) std::rand() / RAND_MAX;
      }
    }

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
      if (args_ == g.arguments()) {
        // optimization
        Globals::logDomain
            ? params_ += g.params()
            : params_ *= g.params();
        return;
      }
      unsigned range_prod = 1;
      bool share_arguments = false;
      const vector<T>& g_args = g.arguments();
      const Ranges& g_ranges  = g.ranges();
      const Params& g_params  = g.params();
      for (size_t i = 0; i < g_args.size(); i++) {
        size_t idx = indexOf (g_args[i]);
        if (idx == args_.size()) {
          range_prod *= g_ranges[i];
          args_.push_back (g_args[i]);
          ranges_.push_back (g_ranges[i]);
        } else {
          share_arguments = true;
        }
      }
      if (share_arguments == false) {
        // optimization
        cartesianProduct (g_params.begin(), g_params.end());
      } else {
        extend (range_prod);
        Params::iterator it = params_.begin();
        CutIndexer indexer (args_, ranges_, g_args, g_ranges);
        if (Globals::logDomain) {
          for (; indexer.valid(); ++indexer) {
            *it++ += g_params[indexer];
          }
        } else {
          for (; indexer.valid(); ++indexer) {
            *it++ *= g_params[indexer];
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
        for (; first != last; ++indexer) {
          newps[indexer] = Util::logSum (newps[indexer], *first++);
        }
      } else {
        for (; first != last; ++indexer) {
          newps[indexer] += *first++;
        }
      }
      params_ = newps;
      args_.erase (args_.begin() + idx);
      ranges_.erase (ranges_.begin() + idx);
    }

    void absorveEvidence (const T& arg, unsigned obsIdx)
    {
      size_t idx = indexOf (arg);
      assert (idx != args_.size());
      assert (obsIdx < ranges_[idx]);
      Params newps;
      newps.reserve (params_.size() / ranges_[idx]); 
      Indexer indexer (ranges_);
      for (unsigned i = 0; i < obsIdx; ++i) {
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
        for (unsigned k = N; k-- > 0; ) {
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
    void extend (unsigned range_prod)
    {
      Params backup = params_;
      params_.clear();
      params_.reserve (backup.size() * range_prod);
      Params::const_iterator first = backup.begin();
      Params::const_iterator last  = backup.end();
      for (; first != last; ++first) {
        for (unsigned reps = 0; reps < range_prod; ++reps) {
          params_.push_back (*first);
        }
      }
    }

    void cartesianProduct (
        Params::const_iterator first2,
        Params::const_iterator last2)
    {
      Params backup = params_;
      params_.clear();
      params_.reserve (params_.size() * (last2 - first2));
      Params::const_iterator first1 = backup.begin();
      Params::const_iterator last1  = backup.end();
      Params::const_iterator tmp;
      if (Globals::logDomain) {
        for (; first1 != last1; ++first1) {
          for (tmp = first2; tmp != last2; ++tmp) {
            params_.push_back ((*first1) + (*tmp));
          }
        }
      } else {
        for (; first1 != last1; ++first1) {
          for (tmp = first2; tmp != last2; ++tmp) {
            params_.push_back ((*first1) * (*tmp));
          }
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

