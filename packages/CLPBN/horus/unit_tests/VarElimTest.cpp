#include "../VarElim.h"
#include "../FactorGraph.h"
#include "Common.h"


namespace Horus {

namespace UnitTests {

class VarElimTest : public CppUnit::TestFixture {
    CPPUNIT_TEST_SUITE (VarElimTest);
    CPPUNIT_TEST (testMarginals);
    CPPUNIT_TEST (testJoint);
    CPPUNIT_TEST_SUITE_END();
  public:
    void testMarginals();
    void testJoint();
};



void
VarElimTest::testMarginals()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  VarElim solver (fg);
  for (unsigned i = 0; i < marginalProbs.size(); i++) {
    Params params = solver.solveQuery ({i});
    CPPUNIT_ASSERT (similiar (params, marginalProbs[i]));
  }
}



void
VarElimTest::testJoint()
{
  FactorGraph fg = FactorGraph::readFromLibDaiFormat (modelFile.c_str());
  VarElim solver (fg);
  Params params = solver.solveQuery ({0, 4, 6});
  CPPUNIT_ASSERT (similiar (params, jointProbs));
}



CPPUNIT_TEST_SUITE_REGISTRATION (VarElimTest);

}  // namespace UnitTests

}  // namespace Horus

