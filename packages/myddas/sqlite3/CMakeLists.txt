message( "   * Sqlite3 Data-Base (http://www.sqlite3ql.org), distributed with MYDDAS" )

set( YAPSQLITE3_SOURCES
  myddas_sqlite3.c
  src/sqlite3.h
  src/sqlite3ext.h
 )


#sqlite3 is now in the system

set (SQLITE3_FOUND ON CACHE PRIVATE "")

include_directories ( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/..)

if (ANDROID)
  add_definitions(-DSQLITE_FCNTL_MMAP_SIZE=0 )

endif()

add_definitions(-DSQLITE_ENABLE_COLUMN_METADATA=1 )

add_definitions(-DSQLITE_ENABLE_JSON1=1 )

add_definitions(-DSQLITE_ENABLE_RBU=1 )

add_definitions(-DSQLITE_ENABLE_RTREE=1 )

add_definitions(-DSQLITE_ENABLE_FTS5=1 )

SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )


add_library(  Yapsqlite3 OBJECT
  ${YAPSQLITE3_SOURCES} )


if (NOT ANDROID)
add_library(  sqlite4YAP SHARED
   src/sqlite3.c)

set_target_properties(sqlite4YAP
  PROPERTIES
  #  RPATH ${libdir} VERSION ${LIBYAPTAI_FULL_VERSION}
  #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
        )


install(TARGETS sqlite4YAP
        RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
        ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
        LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
        )
endif()

    file (INSTALL  chinook.db test.yap DESTINATION ${libpl})

