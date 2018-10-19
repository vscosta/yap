# set(CMAKE_MACOSX_RPATH 1)

add_lib(jplYap  jpl.h jpl.c hacks.h)

include_directories (${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_PATH} )



if (APPLE)
  set(CMAKE_MACOSX_RPATH 1)
  get_filename_component ( JAVA_AWT_DIR ${JAVA_AWT_LIBRARY} DIRECTORY)
  get_filename_component ( JAVA_JNI_DIR ${JAVA_JVM_LIBRARY} DIRECTORY)
  find_library (JLI jli ${JAVA_AWT_DIR}/jli)
  find_library (JAL JavaApplicationLauncher FRAMEWORK ONLY PATH /System/Library/PrivateFrameworks)
  find_library (JL JavaLaunching FRAMEWORK ONLY PATH /System/Library/PrivateFrameworks)
  SET(CMAKE_INSTALL_RPATH "${JAVA_AWT_DIR}/jli")
  target_link_libraries(jplYap libYap ${JL} ${JAL} -L${JAVA_AWT_DIR}/jli -ljli )
else()
  target_link_libraries(jplYap  libYap ${JAVA_JVM_LIBRARY})

endif()


  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    )

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
	  )
