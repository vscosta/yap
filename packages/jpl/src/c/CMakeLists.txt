# set(CMAKE_MACOSX_RPATH 1)


add_library(jplYap jpl.c)

include_directories (${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_PATH} )


  target_link_libraries(jplYap  libYap ${JNI_LIBRARIES})

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    )

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
	  )
