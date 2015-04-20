# - Try to find the CUD BDD RDF parsing library (http://librdf.org/CUDD /)
# Once done this will define
#
#  CUDD_FOUND       - system has CUDD 
#  CUDD_LIBRARIES   - Link these to use CUDD
#  CUDD_INCLUDE_DIR - Include directory for using CUDD
#
#
# Based on FindFontconfig Copyright (c) 2006,2007 Laurent Montel, <montel@kde.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.


MACRO ( FIND_CUDD )

ENDMACRO ()



# Check if we have cached results in case the last round was successful.
if ( NOT( CUDD_INCLUDE_DIR AND CUDD_LIBRARIES ) OR NOT CUDD_FOUND )

  set( CUDD_LDFLAGS )
  
  find_package(PkgConfig)

  
  find_path(CUDD_INCLUDE_DIR
    NAMES cudd.h
    PATHS $ENV{CUDD_DIR}/include/cudd
    $ENV{CUDD_DIR}/include
    $ENV{CUDD_DIR}
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/include/cudd
    /usr/local/include
    /usr/include/cudd
    /usr/include/
    /sw/include        # Fink
    /opt/local/include # MacPorts
    /opt/csw/include   # Blastwave
    /opt/include
    /usr/freeware/include
    
    )

  find_library(CUDD_INTERFACE_LIBRARY
    NAMES cudd
    PATHS
    $ENV{CUDD_DIR}/lib
    $ENV{CUDD_DIR}/lib64
    $ENV{CUDD_DIR}/lib-dbg
    $ENV{CUDD_DIR}
    $ENV{CUDD_DIR}/cudd
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/local/lib64
    /usr/lib
    /usr/lib64
    /usr/local/lib/cudd
    /usr/local/lib64/cudd
    /usr/lib/cudd
    /usr/lib64/cudd
    /usr/freeware/lib64
    )

  if ( CUDD_INTERFACE_LIBRARY AND CUDD_INCLUDE_DIR )
    set( CUDD_FOUND ON )
    list( APPEND CUDD_LIBRARIES ${CUDD_INTERFACE_LIBRARY} )
  endif ()
  
  find_library(CUDD_UTIL_LIBRARY
    NAMES cuddutil util
    PATHS
    $ENV{CUDD_DIR}/lib
    $ENV{CUDD_DIR}/lib64
    $ENV{CUDD_DIR}/lib-dbg
    $ENV{CUDD_DIR}
    $ENV{CUDD_DIR}/util
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/local/lib64
    /usr/lib
    /usr/lib64
    /usr/local/lib/util
    /usr/local/lib64/util
    /usr/lib/util
    /usr/lib64/util
    /usr/freeware/lib64
    )

  if ( CUDD_UTIL_LIBRARY )
    list( APPEND CUDD_LIBRARIES ${CUDD_UTIL_LIBRARY} )
  endif ()

  find_library(CUDD_ST_LIBRARY
    NAMES cuddst st
    PATHS
    $ENV{CUDD_DIR}/lib
    $ENV{CUDD_DIR}/lib64
    $ENV{CUDD_DIR}/lib-dbg
    $ENV{CUDD_DIR}
    $ENV{CUDD_DIR}/st
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/local/lib64
    /usr/lib
    /usr/lib64
    /usr/lib/st
    /usr/lib64/st
    /usr/freeware/lib64
    )

  if ( CUDD_ST_LIBRARY )
    list( APPEND CUDD_LIBRARIES ${CUDD_ST_LIBRARY} )
  endif ()

  find_library(CUDD_EPD_LIBRARY
    NAMES  epd
    PATHS
    $ENV{CUDD_DIR}/lib
    $ENV{CUDD_DIR}/lib64
    $ENV{CUDD_DIR}/lib-dbg
    $ENV{CUDD_DIR}
    $ENV{CUDD_DIR}/epd
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/local/lib64
    /usr/lib
    /usr/lib64
    /usr/lib/epd
    /usr/lib64/epd
    /usr/freeware/lib64
    )

  if ( CUDD_MTR_LIBRARY )
    list( APPEND CUDD_LIBRARIES ${CUDD_MTR_LIBRARY} )
  endif ()

  find_library(CUDD_MTR_LIBRARY
    NAMES  mtr
    PATHS
    $ENV{CUDD_DIR}/lib
    $ENV{CUDD_DIR}/lib64
    $ENV{CUDD_DIR}/lib-dbg
    $ENV{CUDD_DIR}
    $ENV{CUDD_DIR}/mtr
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local/lib
    /usr/local/lib64
    /usr/lib
    /usr/lib64
    /usr/lib/mtr
    /usr/lib64/mtr
    /usr/freeware/lib64
    )

  if ( CUDD_MTR_LIBRARY )
    list( APPEND CUDD_LIBRARIES ${CUDD_MTR_LIBRARY} )
  endif ()


endif () # Check for cached values
