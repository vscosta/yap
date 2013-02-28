#include <cassert>

#include <algorithm>
#include <numeric>

#include "Histogram.h"
#include "Util.h"


namespace Horus {

HistogramSet::HistogramSet (unsigned size, unsigned range)
{
  size_ = size;
  hist_.resize (range, 0);
  hist_[0] = size;
}



void
HistogramSet::nextHistogram()
{
  for (size_t i = hist_.size() - 1; i-- > 0; ) {
    if (hist_[i] > 0) {
      hist_[i] --;
      hist_[i + 1] = maxCount (i + 1);
      clearAfter (i + 1);
      break;
    }
  }
  assert (std::accumulate (hist_.begin(), hist_.end(), 0)
      == (int) size_);
}



unsigned
HistogramSet::operator[] (size_t idx) const
{
  assert (idx < hist_.size());
  return hist_[idx];
}



unsigned
HistogramSet::nrHistograms() const
{
  return HistogramSet::nrHistograms (size_, hist_.size());
}



void
HistogramSet::reset()
{
  std::fill (hist_.begin() + 1, hist_.end(), 0);
  hist_[0] = size_;
}



std::vector<Histogram>
HistogramSet::getHistograms (unsigned N, unsigned R)
{
  HistogramSet hs (N, R);
  unsigned H = hs.nrHistograms();
  std::vector<Histogram> histograms;
  histograms.reserve (H);
  for (unsigned i = 0; i < H; i++) {
    histograms.push_back (hs.hist_);
    hs.nextHistogram();
  }
  return histograms;
}



unsigned
HistogramSet::nrHistograms (unsigned N, unsigned R)
{
  return Util::nrCombinations (N + R - 1, R - 1);
}



size_t
HistogramSet::findIndex (
    const Histogram& h,
    const std::vector<Histogram>& hists)
{
  std::vector<Histogram>::const_iterator it = std::lower_bound (
       hists.begin(), hists.end(), h, std::greater<Histogram>());
  assert (it != hists.end() && *it == h);
  return std::distance (hists.begin(), it);
}



std::vector<double>
HistogramSet::getNumAssigns (unsigned N, unsigned R)
{
  HistogramSet hs (N, R);
  double N_fac = Util::logFactorial (N);
  unsigned H = hs.nrHistograms();
  std::vector<double> numAssigns;
  numAssigns.reserve (H);
  for (unsigned h = 0; h < H; h++) {
    double prod = 0.0;
    for (unsigned r = 0; r < R; r++) {
      prod += Util::logFactorial (hs[r]);
    }
    double res = N_fac - prod;
    numAssigns.push_back (Globals::logDomain ? res : std::exp (res));
    hs.nextHistogram();
  }
  return numAssigns;
}



unsigned
HistogramSet::maxCount (size_t idx) const
{
  unsigned sum = 0;
  for (size_t i = 0; i < idx; i++) {
    sum += hist_[i];
  }
  return size_ - sum;
}



void
HistogramSet::clearAfter (size_t idx)
{
  std::fill (hist_.begin() + idx + 1, hist_.end(), 0);
}



std::ostream&
operator<< (std::ostream& os, const HistogramSet& hs)
{
  os << "#" << hs.hist_;
  return os;
}

}  // namespace Horus

