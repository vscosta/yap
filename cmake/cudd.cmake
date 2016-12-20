

option (WITH_CUDD "BDD CUDD package" ON)

if (WITH_CUDD)
#detect cudd setup, as it is shared between different installations.

find_package(CUDD)
#  CUDD_FOUND       - system has CUDD
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#
macro_log_feature (CUDD_FOUND "CUDD"
    "Use CUDD BDD library"
    "http://vlsi.colorado.edu/~fabio/CUDD/" FALSE)

if (CUDD_FOUND)

    set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${CUDD_INCLUDE_DIR} )

endif (CUDD_FOUND)


endif(WITH_CUDD)
