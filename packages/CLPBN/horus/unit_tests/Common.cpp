#include <cstdlib>
#include <cmath>
#include <cassert>

#include <numeric>
#include <functional>
#include <iostream>

#include "Common.h"
#include "Util.h"


namespace Horus {

namespace UnitTests {

const std::string modelFile = "../examples/complex.fg" ;


const std::vector<Params> marginalProbs = {
/* marginals x0 = */ {0.5825521,  0.4174479},
/* marginals x1 = */ {0.648528,   0.351472},
                     {0.03100852, 0.9689915},
                     {0.04565728, 0.503854,  0.4504888},
                     {0.7713128,  0.03128429, 0.1974029},
                     {0.8771822,  0.1228178},
                     {0.05617282, 0.01509834, 0.9287288},
                     {0.08224711, 0.5698616, 0.047964, 0.2999273},
                     {0.1368483,  0.8631517},
/* marginals x9 = */ {0.7529569,  0.2470431}
};


const Params jointProbs = {
  /* P(x0=0, x4=0, x6=0) = */ 0.025463399,
  /* P(x0=0, x4=0, x6=1) = */ 0.0067233122,
  /* P(x0=0, x4=0, x6=2) = */ 0.42069289,
  /* P(x0=0, x4=1, x6=0) = */ 0.0010111473,
  /* P(x0=0, x4=1, x6=1) = */ 0.00027096982,
  /* P(x0=0, x4=1, x6=2) = */ 0.016715682,
  /* P(x0=0, x4=2, x6=0) = */ 0.0062433667,
  /* P(x0=0, x4=2, x6=1) = */ 0.001828545,
  /* P(x0=0, x4=2, x6=2) = */ 0.10360283,
  /* P(x0=1, x4=0, x6=0) = */ 0.017910021,
  /* P(x0=1, x4=0, x6=1) = */ 0.0046988842,
  /* P(x0=1, x4=0, x6=2) = */ 0.29582433,
  /* P(x0=1, x4=1, x6=0) = */ 0.00074648444,
  /* P(x0=1, x4=1, x6=1) = */ 0.00019991076,
  /* P(x0=1, x4=1, x6=2) = */ 0.012340097,
  /* P(x0=1, x4=2, x6=0) = */ 0.0047984062,
  /* P(x0=1, x4=2, x6=1) = */ 0.0013767189,
  /* P(x0=1, x4=2, x6=2) = */ 0.079553004
};



Params
generateRandomParams (Ranges ranges)
{
  Params params;
  unsigned size = std::accumulate (ranges.begin(), ranges.end(),
      1, std::multiplies<unsigned>());
  for (unsigned i = 0; i < size; i++) {
    params.push_back (rand() / double (RAND_MAX));
  }
  Horus::LogAware::normalize (params);
  return params;
}



bool
similiar (double v1, double v2)
{
  const double epsilon = 0.0000001;
  return std::fabs (v1 - v2) < epsilon;
}



bool
similiar (const Params& p1, const Params& p2)
{
  assert (p1.size() == p2.size());
  for (size_t i = 0; i < p1.size(); i++) {
    if (! similiar(p1[i], p2[i])) {
      return false;
    }
  }
  return true;
}

}  // namespace UnitTests

}  // namespace Horus;
