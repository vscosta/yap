#include "../CountingBp.h"
#include "../FactorGraph.h"
#include "Common.h"


namespace Horus {

namespace UnitTests {

class CountingBpTest : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE (CountingBpTest);
    CPPUNIT_TEST (testMarginals);
    CPPUNIT_TEST (testJoint);
    CPPUNIT_TEST_SUITE_END();
  public:
    void testMarginals();
    void testJoint();
};



void
CountingBpTest::testMarginals()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  CountingBp solver (fg);
  for (unsigned i = 0; i < marginalProbs.size(); i++) {
    Params params = solver.solveQuery ({i});
    CPPUNIT_ASSERT (similiar (params, marginalProbs[i]));
  }
}



void
CountingBpTest::testJoint()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  CountingBp solver (fg);
  Params params = solver.solveQuery ({0, 4, 6});
  CPPUNIT_ASSERT (similiar (params, jointProbs));
}



CPPUNIT_TEST_SUITE_REGISTRATION (CountingBpTest);

}  // namespace UnitTests

}  // namespace Horus

