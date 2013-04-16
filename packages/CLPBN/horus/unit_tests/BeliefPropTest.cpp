#include "../BeliefProp.h"
#include "../FactorGraph.h"
#include "Common.h"


namespace Horus {

namespace UnitTests {

class BeliefPropTest : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE (BeliefPropTest);
    CPPUNIT_TEST (testMarginals);
    CPPUNIT_TEST (testJoint);
    CPPUNIT_TEST_SUITE_END();
  public:
    void testMarginals();
    void testJoint();
};



void
BeliefPropTest::testMarginals()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  BeliefProp solver (fg);
  for (unsigned i = 0; i < marginalProbs.size(); i++) {
    Params params = solver.solveQuery ({i});
    CPPUNIT_ASSERT (similiar (params, marginalProbs[i]));
  }
}



void
BeliefPropTest::testJoint()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  BeliefProp solver (fg);
  Params params = solver.solveQuery ({0, 4, 6});
  CPPUNIT_ASSERT (similiar (params, jointProbs));
}



CPPUNIT_TEST_SUITE_REGISTRATION (BeliefPropTest);

}  // namespace UnitTests

}  // namespace Horus

