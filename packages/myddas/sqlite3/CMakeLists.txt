

  if (WITH_SQLITE3)
  set (SQLITE_TEST
    sqlitest.yap)

  set (SQLITE_DB    chinook.db)

  set( YAPSQLITE3_SOURCES
    myddas_sqlite3.c
    )

  message( "   * Sqlite3 Data-Base (http://www.sqlite3.org), distributed with MYDDAS" )

  add_to_dir(SQLITE_DB ${YAP_INSTALL_DATADIR})
  add_to_dir( SQLITE_TEST ${YAP_INSTALL_DATADIR})


  #sqlite3 is now in the system

  set (SQLITE3_FOUND ON CACHE STRING "System provides SQLITE3 DB" YES  )

set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES  ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/src   )



  add_definitions(-DSQLITE_ENABLE_COLUMN_METADATA=1 )

  add_definitions(-DSQLITE_ENABLE_JSON1=1 )

  add_definitions(-DSQLITE_ENABLE_RBU=1 )

  add_definitions(-DSQLITE_ENABLE_RTREE=1 )
  add_definitions(-DSQLITE_ENABLE_FTS5=1 )

  SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

message("ql ${EMBEDDED_SQLITE3}")
  message( "   * Sqlite3 Data-Base (http://www.sqlite3.org), distributed with MYDDAS" )
  if ( ANDROID )
 add_definitions(-DSQLITE_FCNTL_MMAP_SIZE=0 )
 file(MAKE_DIRECTORY ${YAP_INSTALL_DATADIR}/myddas)
 file(COPY ${SQLITE_DB} ${SQLITE_TEST}
         DESTINATION ${YAP_INSTALL_DATADIR}/myddas)

      add_library(  YAPsqlite3 OBJECT
      ${YAPSQLITE3_SOURCES} )
  else()


    list(APPEND YAPSQLITE3_SOURCES src/sqlite3.c)

    add_library(  YAPsqlite3 SHARED
      ${YAPSQLITE3_SOURCES})

    target_link_libraries(YAPsqlite3 libYap )

    set_target_properties(YAPsqlite3
      PROPERTIES
      #  RPATH ${CMAKE_INSTALL_LIBDIR} VERSION ${LIBYAPTAI_FULL_VERSION}
      #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
      POSITION_INDEPENDENT_CODE ON
      PREFIX ""
      )

    install(TARGETS YAPsqlite3
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      )
  endif()


  install(FILES ${SQLITE_DB} ${SQLITE_TEST}
    DESTINATION ${YAP_INSTALL_DATADIR}/myddas
          )


endif()
