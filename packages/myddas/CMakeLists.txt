SET( CMAKE_FIND_FRAMEWORK LAST)
SET( CMAKE_FIND_APPBUNDLE LAST)

 
# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)


set( MYDDAS_SOURCES
  myddas_initialization.c
  myddas_shared.c
  myddas_statistics.c
  myddas_top_level.c
  myddas_wkb2prolog.c )

set( MYDDAS_HEADERS
  myddas.h
  myddas_statistics.h
  myddas_statistics_structs.h
  myddas_structs.h
  myddas_top_level.c
  myddas_types.h
  myddas_wkb2prolog.c )

set( MYDDAS_UTIL_SOURCES
  myddas_util.c
  myddas_initialization.c )

add_subdirectory (mysql)
add_subdirectory (odbc)
add_subdirectory (postgres)
add_subdirectory (sqlite3)

set( MYDDAS_YPP
  pl/myddas.ypp
  pl/myddas_assert_predicates.ypp
  pl/myddas_top_level.ypp
  pl/myddas_errors.ypp
  pl/myddas_prolog2sql.ypp
  pl/myddas_util_predicates.ypp
  pl/myddas_prolog2sql_optimizer.ypp )

set( MYDDAS_PROLOG
  myddas.yap
  myddas_assert_predicates.yap
  myddas_top_level.yap
  myddas_errors.yap
  myddas_prolog2sql.yap
  myddas_util_predicates.yap
  myddas_prolog2sql_optimizer.yap )

function(cpp_compile filename)
  get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(output ${base_abs}.yap)
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${CMAKE_C_COMPILER} -E ${CMAKE_C_FLAGS} -P -E -w -o ${CMAKE_CURRENT_BINARY_DIR} ${filename})
  set_source_files_properties(${output} PROPERTIES GENERATED TRUE)
endfunction()

foreach(file ${MYDDAS_YPP})
  cpp_compile(GENERATED_SOURCES ${file})
endforeach()

source_group("" FILES ${MYDDAS_YPP})
source_group(generated FILES ${MYDDAS_YAP})

  set_property( DIRECTORY ../..
    APPEND PROPERTY COMPILE_DEFINITIONS  USE_MYDDAS=1)

  Add_custom_target (plmyddas ALL DEPENDS ${MYDDAS_YAP} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )


 add_library (myddas OBJECT
   ${MYDDAS_SOURCES}
  ${MYDDAS_UTIL_SOURCES}
   ${MYDDAS_HEADERS}
)

set_target_properties (myddas  PROPERTIES
  POSITION_INDEPENDENT_CODE ON
 )


  
cmake_dependent_option (USE_MYDDAS_Stats
  "enable the MYDDAS library statistics support" OFF
  USE_MYDDAS OFF)
#TODO:

