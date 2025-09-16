if (WITH_ODBC)

find_Package(ODBC)

if (ODBC_FOUND)
  set( YAPODBC_SOURCES
    myddas_odbc.c
    )
    add_feature_info(  YAPodbc "ODBC_FOUND	" " ODBC at ${ODBC_DIRS},  ${ODBC_INCLUDE_DIRS}" )

    # ODBC_INCLUDE_DIRS, where to find sql.h
    # ODBC_LIBRARIES, the libraries to link against to use ODBC
    # ODBC_FOUND.  If false, you cannot build anything that requires Odbc.
    add_library(YAPodbc  ${YAPODBC_SOURCES})
    target_link_libraries(YAPodbc libYap ${ODBC_LIBRARIES})
set_property(DIRECTORY
        APPEND PROPERTY
       INCLUDE_DIRECTORIES ${ODBC_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}  )
 
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
