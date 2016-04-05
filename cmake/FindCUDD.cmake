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



if (NOT CUDD_ROOT)
  set(CUDD_ROOT "" CACHE PATH "Root of Cudd compiled source tree.")
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

  find_path(CUDD_INT_INCLUDE_DIR
    NAMES cuddInt.h
     NAMES cuddInt.h cudd/cuddInt.h
    $ENV{CUDD_ROOT}/include
    $ENV{CUDD_ROOT}
    /usr/local/yap/include
    /usr/local/Yap/include
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

set (CUDD_INCLUDE_DIR ${CUDD_INCLUDE_DIR} ${CUDD_INT_INCLUDE_DIR})

mark_as_advanced (CUDD_INCLUDE_DIR CUDD_INT_INCLUDE_DIR)

if (ENV{CUDD_ROOT}) 
  set (CUDD_LIB_SEARCH_PATH 
    $ENV{CUDD_ROOT}/lib
    $ENV{CUDD_ROOT}/lib64
    $ENV{CUDD_ROOT}/lib-dbg
    $ENV{CUDD_ROOT}
    $ENV{CUDD_ROOT}/cudd
 )
  endif()

if (CUDD_ROOT) 
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
    /usr/local/lib64/cudd
    /usr/lib/cudd
    /usr/lib64/cudd
    /usr/freeware/lib64 )
    
  find_library(CUDD_CUDD_LIBRARY
    NAMES cudd
    PATHS
    ${CUDD_LIB_SEARCH_PATH}
    )
    
find_library(CUDD_DDMP_LIBRARY
    NAMES ddmp
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
    
    

mark_as_advanced (CUDD_CUDD_LIBRARY CUDD_DDDMP_LIBRARY CUDD_EPD_LIBRARY
  CUDD_ST_LIBRARY CUDD_UTIL_LIBRARY CUDD_MTR_LIBRARY)

set(CUDD_LIBRARIES 
  ${CUDD_CUDD_LIBRARY} ${CUDD_ST_LIBRARY} ${CUDD_UTIL_LIBRARY}
  ${CUDD_MTR_LIBRARY}  ${CUDD_EPD_LIBRARY} ${CUDD_DDDMP_LIBRARY} )
mark_as_advanced (CUDD_LIBRARIES)

if ( CUDD_CUDD_LIBRARY AND CUDD_INCLUDE_DIR )
    set( CUDD_FOUND ON )
  endif ()

mark_as_advanced (CUDD_FOUND)
 
endif () # Check for cached values
