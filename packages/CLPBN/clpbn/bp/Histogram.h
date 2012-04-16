#ifndef HORUS_HISTOGRAM_H
#define HORUS_HISTOGRAM_H

#include <vector>
#include <ostream>

using namespace std;

typedef vector<unsigned> Histogram;

class HistogramSet
{
  public:
    HistogramSet (unsigned, unsigned);
      
    void nextHistogram (void);

    unsigned operator[] (unsigned idx) const;
  
    unsigned nrHistograms (void) const;

    void reset (void);

    static vector<Histogram> getHistograms (unsigned ,unsigned);
   
    static unsigned nrHistograms (unsigned, unsigned);

    static unsigned findIndex (
        const Histogram&, const vector<Histogram>&);

    static vector<double> getNumAssigns (unsigned, unsigned);

    friend std::ostream& operator<< (ostream &os, const HistogramSet& hs);
   
  private:
    unsigned maxCount (unsigned) const;

    void clearAfter (unsigned);

    unsigned  size_;
    Histogram hist_;
};

#endif // HORUS_HISTOGRAM_H

