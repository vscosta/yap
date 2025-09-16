if (WITH_ODBC)


if (ODBC_FOUND)
  set( YAPODBC_SOURCES
    myddas_odbc.c
    )
    add_feature_info(  YAPodbc "ODBC_FOUND	" " ODBC at ${ODBC_LIBRARIES},  ${ODBC_INCLUDE_DIRS}" )

    # ODBC_INCLUDE_DIRS, where to find sql.h
    # ODBC_LIBRARIES, the libraries to link against to use ODBC
    # ODBC_FOUND.  If false, you cannot build anything that requires Odbc.
set_property(DIRECTORY
        APPEND PROPERTY
	INCLUDE_DIRECTORIES  ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/)
    add_library(YAPodbc  ${YAPODBC_SOURCES})
    target_link_libraries(YAPodbc libYap ${ODBC_LIBRARIES})
     
       set_target_properties (YAPodbc PROPERTIES
      POSITION_INDEPENDENT_CODE ON
      PREFIX ""
      )

    install(TARGETS  YAPodbc
      LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
      )

  endif (ODBC_FOUND)

endif()
