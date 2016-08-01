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

include_directories(. sqlite3)


set(MYDDAS_HEADERS
        myddas.h
        myddas_statistics.h
        myddas_statistics_structs.h
        myddas_structs.h
        myddas_top_level.c
        myddas_types.h)

set(MYDDAS_UTIL_SOURCES
        myddas_util.c
        myddas_initialization.c)

set (MYDDAS_FLAGS -DUSE_MYDDAS=1)
set_property(GLOBAL
        APPEND PROPERTY
        COMPILE_DEFINITIONS
        -DUSE_MYDDAS=1)

include_directories(. sqlite3)
add_subdirectory(sqlite3)

MY_add_subdirectory(mysql)
MY_add_subdirectory(odbc)
MY_add_subdirectory(postgres)


add_component(myddas
        ${MYDDAS_SOURCES}
        ${MYDDAS_UTIL_SOURCES}
        ${MYDDAS_HEADERS}
        )
MY_add_dependencies(myddas plmyddas)
add_subdirectory(pl)

MY_set_target_properties(myddas PROPERTIES
        POSITION_INDEPENDENT_CODE ON
        )

cmake_dependent_option(USE_MYDDAS_Stats
        "enable the MYDDAS library statistics support" OFF
        USE_MYDDAS OFF)
