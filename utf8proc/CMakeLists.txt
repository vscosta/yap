cmake_minimum_required (VERSION 2.8)

if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)

include (utils.cmake)

disallow_intree_builds()

#project (utf8proc C)

# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 3)
set(SO_PATCH 0)

add_definitions (
  -DUTF8PROC_EXPORTS
)

if (NOT MSVC)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -std=c99 -pedantic -Wall")
endif ()

SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${my_cxx_flags}" )
SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${my_cxx_flags}" )

if (YAP_SINGLE_FILE)
  add_library (utf8proc OBJECT
    utf8proc.c
    utf8proc.h
  )
else(YAP_SINGLE_FILE)
add_library (utf8proc SHARED
  utf8proc.c
  utf8proc.h
)


install(TARGETS  utf8proc
  LIBRARY DESTINATION ${libdir}
  ARCHIVE DESTINATION ${libdir})

endif(YAP_SINGLE_FILE)

set_target_properties (utf8proc PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
  SOVERSION ${SO_MAJOR}
)

  set( CMAKE_REQUIRED_INCLUDES  . ${CMAKE_REQUIRED_INCLUDES} )


  set ( YAP_YAPUTF8LIB $<TARGET_FILE_NAME:utf8proc> )
