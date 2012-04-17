#include <cassert>

#include <algorithm>
#include <numeric>

#include "Histogram.h"
#include "Util.h"


HistogramSet::HistogramSet (unsigned size, unsigned range)
{
  size_ = size;
  hist_.resize (range, 0);
  hist_[0] = size;
}



void
HistogramSet::nextHistogram (void)
{
  for (int i = hist_.size() - 2; i >= 0; i--) {
    if (hist_[i] > 0) {
      hist_[i] --;
      hist_[i + 1] = maxCount (i + 1);
      clearAfter (i + 1);
      break;
    }
  }
  assert (std::accumulate (hist_.begin(), hist_.end(), 0) == (int)size_);
}



unsigned
HistogramSet::operator[] (unsigned idx) const
{
  assert (idx < hist_.size());
  return hist_[idx];
}



unsigned
HistogramSet::nrHistograms (void) const
{
  return Util::multichoose (size_, hist_.size());
}



void
HistogramSet::reset (void)
{
  std::fill (hist_.begin() + 1, hist_.end(), 0);
  hist_[0] = size_;
}



vector<Histogram> 
HistogramSet::getHistograms (unsigned N, unsigned R)
{
  HistogramSet hs (N, R);  
  unsigned H = hs.nrHistograms();
  vector<Histogram> histograms;
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
  return Util::multichoose (N, R);
}



unsigned
HistogramSet::findIndex (
    const Histogram& h,
    const vector<Histogram>& hists)
{
  vector<Histogram>::const_iterator it = std::lower_bound (
       hists.begin(), hists.end(), h, std::greater<Histogram>());
  assert (it != hists.end() && *it == h);
  return std::distance (hists.begin(), it);
}



vector<double>
HistogramSet::getNumAssigns (unsigned N, unsigned R)
{
  HistogramSet hs (N, R);
  unsigned N_factorial = Util::factorial (N);
  unsigned H = hs.nrHistograms();
  vector<double> numAssigns;
  numAssigns.reserve (H);
  for (unsigned h = 0; h < H; h++) {
    unsigned prod = 1;
    for (unsigned r = 0; r < R; r++) {
      prod *= Util::factorial (hs[r]);
    }
    numAssigns.push_back (LogAware::tl (N_factorial / prod));
    hs.nextHistogram();
  }
  return numAssigns;
}



ostream& operator<< (ostream &os, const HistogramSet& hs)
{
  os << "#" << hs.hist_;
  return os;
}



unsigned
HistogramSet::maxCount (unsigned idx) const
{
  unsigned sum = 0;
  for (unsigned i = 0; i < idx; i++) {
    sum += hist_[i];
  }
  return size_ - sum;
}
  


void
HistogramSet::clearAfter (unsigned idx)
{
  std::fill (hist_.begin() + idx + 1, hist_.end(), 0);
}

