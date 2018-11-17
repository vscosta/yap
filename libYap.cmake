
include (Sources)

set(YAP_STARTUP startup.yss)
## define system

  # Optional libraries that affect compilation
  #


set(CMAKE_TOP_BINARY_DIR ${CMAKE_BINARY_DIR})
set(YAP_PL_SRCDIR ${CMAKE_SOURCE_DIR}/pl)

set(YAP_YAPLIB libYap${CMAKE_SHARED_LIBRARY_SUFFIX})

string(TIMESTAMP YAP_TIMESTAMP)

string( SUBSTRING ${CMAKE_SHARED_LIBRARY_SUFFIX} 1 -1 SO_EXT )

set_property(DIRECTORY PROPERTY CXX_STANDARD 11)

# rpath stuff, hopefully it works

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_LIBDIR};${YAP_INSTALL_LIBDIR}:")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

set_target_properties(libYap
   PROPERTIES OUTPUT_NAME Yap
   )
     MY_set_target_properties(libYap
  PROPERTIES VERSION ${YAP_FULL_VERSION}
  SOVERSION ${YAP_MAJOR_VERSION}.${YAP_MINOR_VERSION}
  )

  if (READLINE_LIBRARIES)
     MY_target_link_libraries(libYap READLINE_LIBRARIES)
 endif()

# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_LIBDIR};${YAP_INSTALL_LIBDIR}" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_LIBDIR};${YAP_INSTALL_LIBDIR}")
ENDIF("${isSystemDir}" STREQUAL "-1")


include_directories (H
    H/generated
    include os OPTYap utf8proc JIT/HPP)
include_directories (BEFORE ${CMAKE_BINARY_DIR} ${CMAKE_TOP_BINARY_DIR})

if (ANDROID)
include_directories (CXX ${CMAKE_SOURCE_DIR}/../generated/src/jni)
endif()

find_package (GMP)
macro_log_feature (GMP_FOUND
  "GNU libgmp (in some cases MPIR"
  "GNU big integers and rationals"
  "http://gmplib.org")
  list(APPEND YAP_SYSTEM_OPTIONS big_numbers)

if (GMP_FOUND)
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBRARIES      - libraries to link with
# GMP_LIBRARY_    #add_executable(test ${SOURCES})
    #config.h needs this (TODO: change in code latter)
    include_directories( ${GMP_INCLUDE_DIRS} )
    target_link_libraries(libYap ${GMP_LIBRARIES} )
endif (GMP_FOUND)

include( Threads )
#
# include OS and I/o stuff
#
# convenience libraries
# OPTYap exports important flags
#
list(APPEND YAP_SYSTEM_OPTIONS "thread support")

#utf-8 is not an option
# we use the nice UTF-8 package
#available at the Julia project

MY_ADD_SUBDIRECTORY ( os )
MY_ADD_SUBDIRECTORY ( OPTYap )
MY_ADD_SUBDIRECTORY ( packages/myddas )
MY_ADD_SUBDIRECTORY ( utf8proc )
MY_ADD_SUBDIRECTORY ( library/dialect/swi/fli )
MY_ADD_SUBDIRECTORY ( CXX )

if (READLINE_LIBS)
  target_link_libraries(libYap ${READLINE_LIBS} )
endif (READLINE_LIBS)


#bootstrap and saved state
add_subDIRECTORY ( pl  )

ADD_SUBDIRECTORY( library)

ADD_SUBDIRECTORY( swi/library "swiLibrary" )
