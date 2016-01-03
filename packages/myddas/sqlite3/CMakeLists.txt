
set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
 )

macro_optional_find_package(SQLITE3 ON)

  macro_log_feature (SQLITE3_FOUND "Sqlite3"
      "Sqlite3 Data-Base "
  "http://www.sqlite3ql.org" FALSE)

if (SQLITE3_FOUND)
  # SQLITE3_INCLUDE_DIRECTORIES, where to find sql.h
  # SQLITE3_LIBRARIES, the libraries to link against to use SQLITE3
  # SQLITE3_FOUND.  If false, you cannot build anything that requires Sqlite3.
  add_library (Yapsqlite3 SHARED ${YAPSQLITE3_SOURCES})
  add_definitions (-DMYDDAS_SQLITE3=1)
  target_link_libraries(Yapsqlite3 ${SQLITE3_LIBRARIES} libYap)
  include_directories (${SQLITE3_INCLUDE_DIRECTORIES}  .. )

  set_target_properties (Yapsqlite3 PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  VERSION "${SO_MAJOR}.${SO_MINOR}.${SO_PATCH}"
  SOVERSION ${SO_MAJOR}
  )

   install(TARGETS  Yapsqlite3
    LIBRARY DESTINATION ${libdir}
    )

endif (SQLITE3_FOUND)
