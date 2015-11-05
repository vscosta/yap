
set( YAPMYSQL_SOURCES
  myddas_mysql.c
)

set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

macro_optional_find_package(MYSQL ON)

macro_log_feature (MYSQL_FOUND "MySQL"
  "MYSQL Driver for MYDDAS Data-Base Interface "
  "http://www.mysql.org" FALSE)

  if (MYSQL_FOUND)
    #  MYSQL_INCLUDE_DIR - where to find mysql.h, etc.
    #  MYSQL_LIBRARIES   - List of libraries when using MySQL.
    #  MYSQL_FOUND       - True if MySQL found.
add_definitions (-DMYDDAS_MYSQL=1)
add_library (Yapmysql SHARED ${YAPMYSQL_SOURCES})
target_link_libraries(Yapmysql myddas libYap)
include_directories (${MYSQL_INCLUDE_DIR} ..)
else()
  add_definitions (-DMYDDAS_MYSQL=0)
  endif (MYSQL_FOUND)

set_target_properties (Yapmysql PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
  SOVERSION ${SO_MAJOR}
  )


  install(TARGETS Yapmysql
     LIBRARY  DESTINATION ${libdir}
   )

cmake_dependent_option (USE_MYDDAS_top_level
  "enable the MYDDAS top-level (REPL) support for MySQL" OFF
  'USE_MYDDAS AND MYSQL_FOUND' OFF)
#TODO:
