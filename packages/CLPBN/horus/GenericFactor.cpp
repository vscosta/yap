#include <cassert>

#include "GenericFactor.h"
#include "ProbFormula.h"
#include "Indexer.h"


namespace Horus {

template <typename T> const T&
GenericFactor<T>::argument (size_t idx) const
{
  assert (idx < args_.size());
  return args_[idx];
}



template <typename T> T&
GenericFactor<T>::argument (size_t idx)
{
  assert (idx < args_.size());
  return args_[idx];
}



template <typename T> unsigned
GenericFactor<T>::range (size_t idx) const
{
  assert (idx < ranges_.size());
  return ranges_[idx];
}



template <typename T> bool
GenericFactor<T>::contains (const T& arg) const
{
  return Util::contains (args_, arg);
}



template <typename T> bool
GenericFactor<T>::contains (const std::vector<T>& args) const
{
  for (size_t i = 0; i < args.size(); i++) {
    if (contains (args[i]) == false) {
      return false;
    }
  }
  return true;
}



template <typename T> void
GenericFactor<T>::setParams (const Params& newParams)
{
  params_ = newParams;
  assert (params_.size() == Util::sizeExpected (ranges_));
}



template <typename T> double
GenericFactor<T>::operator[] (size_t idx) const
{
  assert (idx < params_.size());
  return params_[idx];
}



template <typename T> double&
GenericFactor<T>::operator[] (size_t idx)
{
  assert (idx < params_.size());
  return params_[idx];
}



template <typename T> GenericFactor<T>&
GenericFactor<T>::multiply (const GenericFactor<T>& g)
{
  if (args_ == g.arguments()) {
    // optimization
    Globals::logDomain
      ? params_ += g.params()
      : params_ *= g.params();
    return *this;
  }
  unsigned range_prod = 1;
  bool share_arguments = false;
  const std::vector<T>& g_args = g.arguments();
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
    MapIndexer indexer (args_, ranges_, g_args, g_ranges);
    if (Globals::logDomain) {
      for (; indexer.valid(); ++it, ++indexer) {
        *it += g_params[indexer];
      }
    } else {
      for (; indexer.valid(); ++it, ++indexer) {
        *it *= g_params[indexer];
      }
    }
  }
  return *this;
}



template <typename T> void
GenericFactor<T>::sumOutIndex (size_t idx)
{
  assert (idx < args_.size());
  assert (args_.size() > 1);
  size_t new_size = params_.size() / ranges_[idx];
  Params newps (new_size, LogAware::addIdenty());
  Params::const_iterator first = params_.begin();
  Params::const_iterator last  = params_.end();
  MapIndexer indexer (ranges_, idx);
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



template <typename T> void
GenericFactor<T>::absorveEvidence (const T& arg, unsigned obsIdx)
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



template <typename T> void
GenericFactor<T>::reorderArguments (const std::vector<T>& new_args)
{
  assert (new_args.size() == args_.size());
  if (new_args == args_) {
    return; // already on the desired order
  }
  Ranges new_ranges;
  for (size_t i = 0; i < new_args.size(); i++) {
    size_t idx = indexOf (new_args[i]);
    assert (idx != args_.size());
    new_ranges.push_back (ranges_[idx]);
  }
  Params newps;
  newps.reserve (params_.size());
  MapIndexer indexer (new_args, new_ranges, args_, ranges_);
  for (; indexer.valid(); ++indexer) {
    newps.push_back (params_[indexer]);
  }
  params_ = newps;
  args_   = new_args;
  ranges_ = new_ranges;
}



template <typename T> void
GenericFactor<T>::extend (unsigned range_prod)
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



template <typename T> void
GenericFactor<T>::cartesianProduct (
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



template class GenericFactor<VarId>;
template class GenericFactor<ProbFormula>;

}  // namespace Horus

