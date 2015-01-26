
set( MYDDAS_SOURCES
  myddas_mysql.c
  myddas_odbc.c
  myddas_util.c
  myddas_initialization.c
  myddas_shared.c
  myddas_statistics.c
  myddas_top_level.c
  myddas_wkb2prolog.c )

set( MYDDAS_PROLOG
  pl/myddas.ypp
  pl/myddas_assert_predicates.ypp
  pl/myddas_mysql.ypp
  pl/myddas_top_level.ypp
  pl/myddas_errors.ypp
  pl/myddas_prolog2sql.ypp
  pl/myddas_util_predicates.ypp
  pl/myddas_prolog2sql_optimizer.ypp )

function(cpp_compile filename)
  get_filename_component(base ${filename} NAME_WE)
  set(base_abs ${CMAKE_CURRENT_BINARY_DIR}/${base})
  set(output ${base_abs}.yap)
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${CMAKE_C_COMPILER} -E ${CMAKE_C_FLAGS} -P -E -w -o ${CMAKE_CURRENT_BINARY_DIR} ${filename})
  set_source_files_properties(${output} PROPERTIES GENERATED TRUE)
endfunction()

foreach(file ${MYDDAS_PROLOG})
  cpp_compile(GENERATED_SOURCES ${file})
endforeach()

source_group("" FILES ${MYDDAS_PROLOG})
source_group(generated FILES ${GENERATED_SOURCES})

macro_optional_find_package(ODBC ON)
macro_optional_find_package(MySQL ON)
macro_optional_find_package(Sqlite3 ON)

if (ODBC_FOUND OR MYSQL_FOUND or SQLITE3_FOUND)
  add_library (myddas SHARED ${MYDDAS_SOURCES})
  target_link_libraries(myddas libYap)
  if (ODBC_FOUND)
      # ODBC_INCLUDE_DIRECTORIES, where to find sql.h
      # ODBC_LIBRARIES, the libraries to link against to use ODBC
      # ODBC_FOUND.  If false, you cannot build anything that requires MySQL.
      macro_log_feature (ODBC_FOUND "ODBC"
          "Use ODBC Data-Base Interface "
      "http://www.unixodbc.org" FALSE)
      add_definitions (-DMYDDAS_ODBC=1)
      target_link_libraries(myddas ${ODBC_LIBRARIES})
      include_directories (${ODBC_INCLUDE_DIRECTORIES})
  endif (ODBC_FOUND)

  if (SQLITE3_FOUND)
      # SQLITE3_INCLUDE_DIR - Where to find Sqlite 3 header files (directory)
      # SQLITE3_LIBRARIES - Sqlite 3 libraries
      # SQLITE3_LIBRARY_RELEASE - Where the release library is
      # SQLITE3_LIBRARY_DEBUG - Where the debug library is
      # SQLITE3_FOUND - Set to TRUE if we found everything (library, includes and executable)
      macro_log_feature (SQLITE3_FOUND "SQLite3"
          "SQL Light Data-Base "
      "http://www.sqlite3.org" FALSE)
      add_definitions (-DMYDDAS_SQLITE3=1)
      target_link_libraries(myddas ${SQLITE3_LIBRARIES})
      include_directories (${SQLITE3_INCLUDE_DIR})
  endif (SQLITE3_FOUND)

  if (MYSQL_FOUND)
    #  MYSQL_INCLUDE_DIR - where to find mysql.h, etc.
    #  MYSQL_LIBRARIES   - List of libraries when using MySQL.
    #  MYSQL_FOUND       - True if MySQL found.
    macro_log_feature (MYSQL_FOUND "MySQL"
      "Use MYSQL Data-Base Interface "
      "http://www.mysql.org" FALSE)
    add_definitions (-DMYDDAS_MYSQL=1)
    target_link_libraries(myddas ${MYSQL_LIBRARIES})
    include_directories (${MYSQL_INCLUDE_DIR})
  endif (MYSQL_FOUND)

  add_custom_target (plmyddas ALL DEPENDS ${GENERATED_SOURCES} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )
endif (ODBC_FOUND OR MYSQL_FOUND)

cmake_dependent_option (WITH_MYDDAS_Stats
  "enable the MYDDAS library statistics support" OFF
  WITH_MYDDAS OFF)
#TODO:

cmake_dependent_option (WITH_MYDDAS_top_level
  "enable the MYDDAS top-level support to MySQL" OFF
  'WITH_MYDDAS AND MYSQL_FOUND' OFF)
#TODO:
