
add_lib(jplYap  jpl.h jpl.c hacks.h)

include_directories (${JAVA_INCLUDE_DIRS} ${JNI_INCLUDE_DIRS} )

  target_link_libraries(jplYap  libYap ${JNI_LIBRARIES} ${JAVA_LIBRARIES})

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    INSTALL_RPATH_USE_LINK_PATH TRUE )

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
	  )
