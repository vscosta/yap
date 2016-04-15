# - Try to find the CUDD BDD package
# Once done this will define
#
#  CUDD_FOUND       - system has CUDD 
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#
# Based on FindFontconfig Copyright (c) 2006,2007 Laurent Montel, <montel@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

if (NOT DEFINED CUDD_ROOT)
  set(CUDD_ROOT $ENV{CUDD_ROOT})
endif()

# Check if we have cached results in case the last round was successful.

  find_package(PkgConfig)

  
  find_path(CUDD_INCLUDE_DIR
    NAMES cudd.h cudd/cudd.h
    $ENV{CUDD_ROOT}/include
    $ENV{CUDD_ROOT}
    /usr/local/yap/include
    /usr/local/Yap/include
    /usr/local/cudd/include
    /usr/lib/cudd/include
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/include
    /usr/include/
    /sw/include        # Fink
    /opt/local/include # MacPorts
    /opt/csw/include   # Blastwave
    /opt/include
    /usr/freeware/include
    NO_DEFAULT_PATHS
    
    )
  
 

if (${CUDD_ROOT}) 
  set (CUDD_LIB_SEARCH_PATH 
    ${CUDD_ROOT}/lib
    ${CUDD_ROOT}/lib64
    ${CUDD_ROOT}/lib-dbg
    ${CUDD_ROOT}
    ${CUDD_ROOT}/cudd
 )
 endif()


 
set (CUDD_LIB_SEARCH_PATH 
    ${CUDD_LIB_SEARCH_PATH}
     /usr/local/lib/cudd
    /usr/local/cudd/lib
    /usr/lib/cudd
    /usr/lib/cudd/lib
    /usr/lib64/cudd
    /usr/freeware/lib64 )

  find_library(CUDD_LIBRARIES
    NAMES cudd
    PATHS
    ${CUDD_LIB_SEARCH_PATH}
    )
    
 IF (NOT EXISTS ${CUDD_INCLUDE_DIR}/epdInt.h )

find_library(CUDD_DDDMP_LIBRARY
    NAMES dddmp
    PATHS
    ${CUDD_LIB_SEARCH_PATH}
    )

find_library(CUDD_EPD_LIBRARY
    NAMES  epd
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
    )

  find_library(CUDD_ST_LIBRARY
    NAMES cuddst st
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
   )

  
  find_library(CUDD_UTIL_LIBRARY
    NAMES util 
    
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
    NO_DEFAULT_PATHS
    NO_SYSTEM_ENVIRONMENT_PATH
    NO_CMAKE_SYSTEM_PATH
    )
 
   find_library(CUDD_UTIL_LIBRARY
    NAMES cuddutil 
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
     )

  find_library(CUDD_MTR_LIBRARY
    NAMES  mtr
    PATHS
      ${CUDD_LIB_SEARCH_PATH}
    )


set(CUDD_LIBRARIES 
  ${CUDD_LIBRARIES} ${CUDD_ST_LIBRARY} ${CUDD_UTIL_LIBRARY}
  ${CUDD_MTR_LIBRARY}  ${CUDD_EPD_LIBRARY} ${CUDD_DDDMP_LIBRARY} )

endif () # Check for cudd 2.

mark_as_advanced (CUDD_FOUND)

find_package_handle_standard_args(R DEFAULT_MSG CUDD_LIBRARIES CUDD_INCLUDE_DIR )

