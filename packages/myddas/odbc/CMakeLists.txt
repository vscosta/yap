if (WITH_ODBC)
  
  set( YAPODBC_SOURCES
    myddas_odbc.c
    )

  set(SO_MAJOR 1)
  set(SO_MINOR 0)
  set(SO_PATCH 0)

  macro_optional_find_package(ODBC ON)

  if (ODBC_FOUND)
    # ODBC_INCLUDE_DIRECTORIES, where to find sql.h
    # ODBC_LIBRARIES, the libraries to link against to use ODBC
    # ODBC_FOUND.  If false, you cannot build anything that requires Odbc.
    add_library(YAPodbc  ${YAPODBC_SOURCES})
    target_link_libraries(YAPodbc libYap ${ODBC_LIBRARIES})
set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES ${ODBC_INCLUDE_DIRECTORIES} ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}  )
 
    set_target_properties (YAPodbc PROPERTIES
      POSITION_INDEPENDENT_CODE ON
      PREFIX ""
      )

    install(TARGETS  YAPodbc
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
      )

  else()
    add_definitions (-DMYDDAS_ODBC=0)
  endif (ODBC_FOUND)

endif()
