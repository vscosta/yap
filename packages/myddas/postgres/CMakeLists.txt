
if (WITH_POSTGRES)

  set( YAPPOSTGRES_SOURCES
    myddas_postgres.c
    )

  set(SO_MAJOR 1)
  set(SO_MINOR 0)
  set(SO_PATCH 0)

  macro_optional_find_package(PostgreSQL ON)

  if (PostgreSQL_FOUND)
    #  PostgreSQL_FOUND         - Set to true when PostgreSQL is found.
    #  PostgreSQL_INCLUDE_DIRS  - Include directories for PostgreSQL
    #  PostgreSQL_LIBRARY_DIRS  - Link directories for PostgreSQL libraries
    #  PostgreSQL_LIBRARIES     - The PostgreSQL libraries.
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
