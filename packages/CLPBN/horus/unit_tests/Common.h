#include <vector>
#include <string>

#include "../Horus.h"

#include <cppunit/extensions/HelperMacros.h>


namespace Horus {

namespace UnitTests {

extern const std::string modelFile;

extern const std::vector<Params> marginalProbs;

extern const Params jointProbs;

Params generateRandomParams (Ranges ranges);

bool similiar (double v1, double v2);

bool similiar (const Params& p1, const Params& p2);

}  // namespace UnitTests

}  // namespace Horus;

