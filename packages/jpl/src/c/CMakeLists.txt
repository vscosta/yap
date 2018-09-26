# set(CMAKE_MACOSX_RPATH 1)

add_lib(jplYap  jpl.h jpl.c hacks.h)

include_directories (${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_PATH} )

if (APPLE)
get_filename_component ( JAVA_AWT_DIR ${JAVA_AWT_LIBRARY} DIRECTORY)
find_library (JLI jli ${JAVA_AWT_DIR}/jli)
endif()

  target_link_libraries(jplYap  libYap ${JLI} ${JNI_LIBRARIES})

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    INSTALL_RPATH_USE_LINK_PATH TRUE
    )

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
	  )
