SET(CMAKE_FIND_FRAMEWORK LAST)
SET(CMAKE_FIND_APPBUNDLE LAST)

# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

set(MYDDAS_SOURCES
        myddas_initialization.c
        myddas_shared.c
        myddas_statistics.c
        myddas_top_level.c
        )

set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}    ${CMAKE_CURRENT_SOURCE_DIR}/sqlite3)


set(MYDDAS_HEADERS
        myddas.h
        myddas_statistics.h
        myddas_statistics_structs.h
        myddas_structs.h
        myddas_types.h)

set(MYDDAS_UTIL_SOURCES
        myddas_util.c
        myddas_initialization.c)

add_subdirectory(sqlite3)

if(MYSQL_FOUND)
add_subdirectory(mysql)
       endif()
if (ODBC_FOUND)
add_subdirectory(odbc)
       endif()
if (POSTGRES_FOUND)
add_subdirectory(postgres)
endif()
       set(CXX_SWIG_OUTDIR ${CMAKE_BINARY_DIR}/packages/swig/android)

add_component(myddas
        ${MYDDAS_SOURCES}
        ${MYDDAS_UTIL_SOURCES}
        ${MYDDAS_HEADERS}
        )

SET_PROPERTY(SOURCE ${MYDDAS_SOURCES}
        ${MYDDAS_UTIL_SOURCES}
        ${MYDDAS_HEADERS}  PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

add_subdirectory(pl)


MY_set_target_properties(myddas PROPERTIES
        POSITION_INDEPENDENT_CODE ON
        )

cmake_dependent_option(USE_MYDDAS_Stats
        "enable the MYDDAS library statistics support" OFF
        USE_MYDDAS OFF)

add_dependencies(myddas MYDDAS_PL)