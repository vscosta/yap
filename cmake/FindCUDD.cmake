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

SET( CUDD_FOUND "NO" )

set (CUDD_INCLUDE_SEARCH_PATH 
    ${CMAKE_INSTALL_PREFIX}/include
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
 )



set (CUDD_LIB_SEARCH_PATH 
    ${CMAKE_INSTALL_PREFIX}/lib
    /usr/lib
    /usr/local/lib/cudd
    /usr/local/cudd/lib
    /usr/lib/cudd
    /usr/lib/cudd/lib
    /usr/freeware/lib )

if ($ENV{CUDD_ROOT}) 
  list (APPEND CUDD_LIB_SEARCH_PATH 
    $ENV{CUDD_ROOT}/lib
    $ENV{CUDD_ROOT}/lib-dbg
    $ENV{CUDD_ROOT} )

  list (APPEND CUDD_INCLUDE_SEARCH_PATH 
    $ENV{CUDD_ROOT}/include )
endif()

if (${CUDD_ROOT}) 
  list (APPEND CUDD_LIB_SEARCH_PATH 
    ${CUDD_ROOT}/lib
    ${CUDD_ROOT}/lib-dbg
    ${CUDD_ROOT} )
  list (APPEND CUDD_INCLUDE_SEARCH_PATH 
    ${CUDD_ROOT}/include )
endif()

# Check if we have cached results in case the last round was successful.

  find_package(PkgConfig)
  
  find_path(CUDD_INCLUDE_DIR
    NAMES cudd.h cudd/cudd.h
    ${CUDD_INCLUDE_SEARCH_PATH}
    )
  
 mark_as_advanced(CUDD_INCLUDE_DIR)

if (CUDD_INCLUDE_DIR)

  find_library(CUDD_LIBRARIES
    NAMES cudd
    PATHS
    ${CUDD_LIB_SEARCH_PATH}
    )

    if (CUDD_LIBRARIES)
    
    SET( CUDD_FOUND "YES" )
 
find_library(CUDD_DDDMP_LIBRARY
    NAMES dddmp
    PATHS
    ${CUDD_LIB_SEARCH_PATH}
    )

if (CUDD_DDMP_LIBRARY)
   list( APPEND CUDD_LIBRARIES ${CUDD_DDMP_LIBRARY} )
endif()

find_library(CUDD_EPD_LIBRARY
    NAMES  epd
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
    )

if (CUDD_EPD_LIBRARY)
   list( APPEND CUDD_LIBRARIES ${CUDD_EPD_LIBRARY} )
endif()

  find_library(CUDD_ST_LIBRARY
    NAMES cuddst 
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
   )

if (CUDD_ST_LIBRARY)
   list( APPEND CUDD_LIBRARIES ${CUDD_ST_LIBRARY} )
endif()


  
  find_library(CUDD_UTIL_LIBRARY
    NAMES cuddutil  
    
    PATHS
     ${CUDD_LIB_SEARCH_PATH}
    )
 
if (CUDD_UTIL_LIBRARY)
   list( APPEND CUDD_LIBRARIES ${CUDD_ST_LIBRARY} )
endif()

  find_library(CUDD_MTR_LIBRARY
    NAMES  mtr
    PATHS
      ${CUDD_LIB_SEARCH_PATH}
    )

if (CUDD_MTR_LIBRARY)
   list( APPEND CUDD_LIBRARIES ${CUDD_MTR_LIBRARY} )
endif()

endif()

endif()

 mark_as_advanced(CUDD_LIBRARIES)



mark_as_advanced (CUDD_FOUND)


find_package_handle_standard_args(CUDD DEFAULT_MSG CUDD_LIBRARIES CUDD_INCLUDE_DIR )

