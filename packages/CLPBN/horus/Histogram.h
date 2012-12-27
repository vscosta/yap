#ifndef HORUS_HISTOGRAM_H
#define HORUS_HISTOGRAM_H

#include <vector>

#include <ostream>

#include "Horus.h"

using namespace std;

typedef vector<unsigned> Histogram;

class HistogramSet
{
  public:
    HistogramSet (unsigned, unsigned);

    void nextHistogram (void);

    unsigned operator[] (size_t idx) const;

    unsigned nrHistograms (void) const;

    void reset (void);

    static vector<Histogram> getHistograms (unsigned ,unsigned);

    static unsigned nrHistograms (unsigned, unsigned);

    static size_t findIndex (
        const Histogram&, const vector<Histogram>&);

    static vector<double> getNumAssigns (unsigned, unsigned);

    friend std::ostream& operator<< (ostream &os, const HistogramSet& hs);

  private:
    unsigned maxCount (size_t) const;

    void clearAfter (size_t);

    unsigned   size_;
    Histogram  hist_;

    DISALLOW_COPY_AND_ASSIGN (HistogramSet);
};

#endif // HORUS_HISTOGRAM_H

