
if (WITH_Postgres)

  set( YAPPOSTGRES_SOURCES
    myddas_postgres.c
    )

    add_feature_info(  YAPpostgres "PostgresSQL_FOUND	" " POstgresSQL DB Manager version ${PostgreSQL_VERSION_STRING}, at ${SQLite3_INCLUDE_DIRS} and   ${SQLite3_LIBRARY_DIRS}" )
  if (PostgreSQL_FOUND)

    add_library(YAPpostgres  ${YAPPOSTGRES_SOURCES})
    target_link_libraries(YAPpostgres libYap   ${PostgreSQL_LIBRARIES})
    set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES ${PostgreSQL_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/..  )
 
    set_target_properties (YAPpostgres PROPERTIES
      POSITION_INDEPENDENT_CODE ON
      PREFIX ""
      )

    list (APPEND MYDDAS_FLAGS -DMYDDAS_POSTGRES=1)
    set (MYDDAS_FLAGS ${MYDDAS_FLAGS} ON PARENT_SCOPE)
    set_property(GLOBAL APPEND PROPERTY COMPILE_DEFINITIONS
      -DMYDDAS_POSTGRES=1)

    install(TARGETS  YAPpostgres
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
      )
  endif (PostgreSQL_FOUND)

endif()

