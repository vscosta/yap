

option (WITH_CUDD "BDD CUDD package" ON)

if (WITH_CUDD)
#detect cudd setup, as it is shared between different installations.

find_package(CUDD)
#  CUDD_FOUND       - system has CUDD
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#

if (CUDD_FOUND)

    set( CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${CUDD_INCLUDE_DIR} )
check_include_files( "stdio.h;cudd.h"  HAVE_CUDD_H )
check_include_files( "stdio.h;mtr.h"  HAVE_MTR_H )
check_include_files( "stdio.h;util.h"  HAVE_UTIL_H )
check_include_files( "stdio.h;epd.h"  HAVE_EPD_H )
check_include_files( "stdio.h;st.h"  HAVE_ST_H )
check_include_files( "stdio.h;cudd/cudd.h" HAVE_CUDD_CUDD_H )
check_include_files( "stdio.h;cudd/mtr.h" HAVE_CUDD_MTR_H )
check_include_files( "stdio.h;cudd/util.h"  HAVE_CUDD_UTIL_H )
check_include_files( "stdio.h;cudd/epd.h"  HAVE_CUDD_EPD_H )
check_include_files( "stdio.h;cudd/st.h"  HAVE_CUDD_ST_H )
configure_file (cmake/cudd_config.h.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/cudd_config.h" )

endif (CUDD_FOUND)


endif(WITH_CUDD)
