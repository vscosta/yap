#ifndef YAP_PACKAGES_CLPBN_HORUS_HISTOGRAM_H_
#define YAP_PACKAGES_CLPBN_HORUS_HISTOGRAM_H_

#include <vector>
#include <ostream>

#include "Horus.h"

typedef std::vector<unsigned> Histogram;


namespace Horus {

class HistogramSet {
  public:
    HistogramSet (unsigned, unsigned);

    void nextHistogram();

    unsigned operator[] (size_t idx) const;

    unsigned nrHistograms() const;

    void reset();

    static std::vector<Histogram> getHistograms (unsigned, unsigned);

    static unsigned nrHistograms (unsigned, unsigned);

    static size_t findIndex (
        const Histogram&, const std::vector<Histogram>&);

    static std::vector<double> getNumAssigns (unsigned, unsigned);

  private:
    unsigned maxCount (size_t) const;

    void clearAfter (size_t);

    friend std::ostream& operator<< (std::ostream&, const HistogramSet&);

    unsigned   size_;
    Histogram  hist_;

    DISALLOW_COPY_AND_ASSIGN (HistogramSet);
};

}  // namespace Horus

#endif  // YAP_PACKAGES_CLPBN_HORUS_HISTOGRAM_H_

