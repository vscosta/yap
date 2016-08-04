cmake_minimum_required (VERSION 2.8)

set ( UTF8PROC_SOURCES
utf8proc.c )

add_component(utf8proc ${UTF8PROC_SOURCES} )
#set_source_files_properties(${all_c_files} PROPERTIES LANGUAGE CXX) 

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


#if (NOT MSVC)
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -std=c99 -pedantic -Wall")
#endif ()
##SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${my_cxx_flags}" )
#SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${my_cxx_flags}" )

MY_set_target_properties( utf8proc PROPERTIES   POSITION_INDEPENDENT_CODE ON
        )

  set( CMAKE_REQUIRED_INCLUDES  . ${CMAKE_REQUIRED_INCLUDES} )



