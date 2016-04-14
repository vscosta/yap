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

if (NOT CUDD_ROOT)
  set(CUDD_ROOT "" CACHE PATH "Root of Cudd compiled source tree.")
endif()

set (CUDD_FOUND NO CACHE "system Has CUDD" PARENT_SCOPE)

if (NOT CUDD_ROOT)
  set(CUDD_ROOT "" CACHE PATH "Root of Cudd compiled source tree."  PARENT_SCOPE)
endif()

# Check if we have cached results in case the last round was successful.
if ( NOT( CUDD_INCLUDE_DIR AND CUDD_LIBRARIES ) OR NOT CUDD_FOUND )

  set( CUDD_LDFLAGS )
  
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
  
mark_as_advanced (CUDD_INCLUDE_DIR)

if ($ENV{CUDD_ROOT}) 
  set (CUDD_LIB_SEARCH_PATH 
    $ENV{CUDD_ROOT}/lib
    $ENV{CUDD_ROOT}/lib64
    $ENV{CUDD_ROOT}/lib-dbg
    $ENV{CUDD_ROOT}
    $ENV{CUDD_ROOT}/cudd
 )
endif ($ENV{CUDD_ROOT}) 

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

endif()

if ( CUDD_LIBRARIES AND CUDD_INCLUDE_DIR )
    set( CUDD_FOUND ON CACHE CACHE "system Has CUDD" FORCE)
endif()

mark_as_advanced (CUDD_FOUND)
 
endif () # Check for cached values
