#include "../Factor.h"

#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/extensions/TestFactoryRegistry.h>


int main()
{
  CppUnit::TextUi::TestRunner runner;
  CppUnit::TestFactoryRegistry& registry =
      CppUnit::TestFactoryRegistry::getRegistry();
  runner.addTest (registry.makeTest());
  return runner.run() ? 1 : 0;
}

