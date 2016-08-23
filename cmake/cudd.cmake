
 
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

check_include_files( cudd.h HAVE_CUDD_H )
check_include_files( "stdio.h;cudd/cudd.h" HAVE_CUDD_CUDD_H )
check_include_files( cuddInt.h HAVE_CUDDINT_H )
check_include_files( "stdio.h;cudd/cudd.h;cudd/cuddInt.h" HAVE_CUDD_CUDDINT_H )
endif (CUDD_FOUND)

endif(WITH_CUDD)
