#ifndef PACKAGES_CLPBN_HORUS_HISTOGRAM_H
#define PACKAGES_CLPBN_HORUS_HISTOGRAM_H

#include <vector>

#include <ostream>

#include "Horus.h"

typedef std::vector<unsigned> Histogram;


class HistogramSet
{
  public:
    HistogramSet (unsigned, unsigned);

    void nextHistogram (void);

    unsigned operator[] (size_t idx) const;

    unsigned nrHistograms (void) const;

    void reset (void);

    static std::vector<Histogram> getHistograms (unsigned, unsigned);

    static unsigned nrHistograms (unsigned, unsigned);

    static size_t findIndex (
        const Histogram&, const std::vector<Histogram>&);

    static std::vector<double> getNumAssigns (unsigned, unsigned);

  private:
    unsigned maxCount (size_t) const;

    void clearAfter (size_t);

    unsigned   size_;
    Histogram  hist_;

    friend std::ostream& operator<< (std::ostream &os, const HistogramSet& hs);

    DISALLOW_COPY_AND_ASSIGN (HistogramSet);
};

#endif // HORUS_HISTOGRAM_H

