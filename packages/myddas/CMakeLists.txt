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
  )

  include_directories (. sqlite3)


set( MYDDAS_HEADERS
  myddas.h
  myddas_statistics.h
  myddas_statistics_structs.h
  myddas_structs.h
  myddas_top_level.c
  myddas_types.h )

set( MYDDAS_UTIL_SOURCES
  myddas_util.c
  myddas_initialization.c )

  set_property( GLOBAL
    APPEND PROPERTY COMPILE_DEFINITIONS  USE_MYDDAS=1)

    # internal MYDDAS flag, used to preprocess ypp files.
    set (MYDDAS_FLAGS -DUSE_MYDDAS=1)

add_subdirectory (mysql)
add_subdirectory (odbc)
add_subdirectory (postgres)
add_subdirectory (sqlite3)


    add_subdirectory (pl)


add_library (myddas OBJECT
   ${MYDDAS_SOURCES}
  ${MYDDAS_UTIL_SOURCES}
   ${MYDDAS_HEADERS}
)


add_dependencies(myddas plmyddas)

set_target_properties (myddas  PROPERTIES
  POSITION_INDEPENDENT_CODE ON
 )

cmake_dependent_option (USE_MYDDAS_Stats
  "enable the MYDDAS library statistics support" OFF
  USE_MYDDAS OFF)
