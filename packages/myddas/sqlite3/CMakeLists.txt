


  if (WITH_SQLite3)
    add_feature_info(  YAPsqlite3 "SQLite3_FOUND	" " SQLite3 DB Manager version ${SQLite3_VERSION}, at ${SQLite3_INCLUDE_DIRS}" )

if (SQLite3_FOUND)


set (SQLITE_TEST
    sqlitest.yap)

  set (SQLITE_DB    chinook.db)

  set( YAPSQLITE3_SOURCES
    myddas_sqlite3.c
    )


if (SQLite3_FOUND)

find_package(SQLite3)


set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES  ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/)


  SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

  if ( ANDROID )
 add_definitions(-DSQLITE_FCNTL_MMAP_SIZE=0 )
 file(MAKE_DIRECTORY ${YAP_INSTALL_DATADIR}/myddas)
 file(COPY ${SQLITE_DB} ${SQLITE_TEST}
         DESTINATION ${YAP_INSTALL_DATADIR}/myddas)

      add_library(  YAPsqlite3 OBJECT
      ${YAPSQLITE3_SOURCES} )
  else()
   add_library(  YAPsqlite3 SHARED
      ${YAPSQLITE3_SOURCES})
endif()

 
    target_link_libraries(YAPsqlite3 libYap YAPmyddas   SQLite::SQLite3)

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
endif()
