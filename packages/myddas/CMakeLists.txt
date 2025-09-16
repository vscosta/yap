SET(CMAKE_FIND_FRAMEWORK LAST)
SET(CMAKE_FIND_APPBUNDLE LAST)

# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

add_feature_info(MYDDAS WITH_MYDDAS "MYDDAS release ${SO_MAJOR}.${SO_MINOR}.${SO_PATCH} " )


add_subdirectory(sqlite3)
add_subdirectory(mysql)
add_subdirectory(odbc)
add_subdirectory(postgres)

if (MYSQL_FOUND)
add_definitions( -DMYDDAS_MYSQL=1)
endif ()

if (ODBC_FOUND)
add_definitions( -DMYDDAS_ODBC=1)
endif ()

if (POSTGRES_FOUND)
add_definitions( -DMYDDAS_POSTGRES=1)
endif()

if (SQLite3_FOUND)
add_definitions( -DMYDDAS_SQLITE3=1)
endif()

set(MYDDAS_SOURCES
myddas_initialization.c
myddas_shared.c
myddas_statistics.c
myddas_top_level.c
)

set_property(DIRECTORY
APPEND PROPERTY
INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}    ${CMAKE_CURRENT_SOURCE_DIR}/sqlite3 ${GMP_INCLUDE_DIR})


set(MYDDAS_HEADERS
myddas.h
myddas_statistics.h
myddas_statistics_structs.h
myddas_structs.h
myddas_types.h)

set(MYDDAS_UTIL_SOURCES
myddas_util.c
myddas_initialization.c)

set(CXX_SWIG_OUTDIR ${CMAKE_BINARY_DIR}/packages/swig/android)

add_library(YAPmyddas
${MYDDAS_SOURCES}
${MYDDAS_UTIL_SOURCES}
${MYDDAS_HEADERS}
)

SET_PROPERTY(SOURCE ${MYDDAS_SOURCES}
${MYDDAS_UTIL_SOURCES}
${MYDDAS_HEADERS}  PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

add_subdirectory(pl)

target_link_libraries(YAPmyddas libYap)

set_target_properties(YAPmyddas PROPERTIES
        POSITION_INDEPENDENT_CODE ON
        )

cmake_dependent_option(USE_MYDDAS_Stats
        "enable the MYDDAS library statistics support" OFF
        USE_MYDDAS OFF)

add_dependencies(YAPmyddas MYDDAS_PL)

install(TARGETS YAPmyddas
LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )
