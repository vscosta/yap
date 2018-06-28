message( "   * Sqlite3 Data-Base (http://www.sqlite3.org), distributed with MYDDAS" )

set (SQLITE_EXTRA
  chinook.db
  sqlitest.yap)

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


if (ANDROID)
add_library(  YAPsqlite3 OBJECT
  ${YAPSQLITE3_SOURCES} )

else()

  list(APPEND YAPSQLITE3_SOURCES src/sqlite3.c)

  
add_library(  YAPsqlite3 SHARED
  ${YAPSQLITE3_SOURCES})

 set_target_properties(YAPsqlite3
  PROPERTIES
  #  RPATH ${libdir} VERSION ${LIBYAPTAI_FULL_VERSION}
  #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
    POSITION_INDEPENDENT_CODE ON
    PREFIX ""
  )

install(TARGETS YAPsqlite3
        RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
        ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
        LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
        )
endif()

    install(FILES ${SQLITE_EXTRA}
     DESTINATION ${libpl}
        )
