

option (WITH_CUDD "BDD CUDD package" ON)

if (WITH_CUDD)
#detect cudd setup, as it is shared between different installations.

find_package(CUDD)
#  CUDD_FOUND       - system has CUDD
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#


endif(WITH_CUDD)
