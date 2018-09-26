# set(CMAKE_MACOSX_RPATH 1)
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

add_lib(jplYap  jpl.h jpl.c hacks.h)

include_directories (${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_PATH} )

if (MAC)
  target_link_libraries(jplYap  libYap /System/Library/Frameworks/CoreFoundation.framework/Versions/A/CoreFoundation 
	/System/Library/PrivateFrameworks/JavaLaunching.framework/Versions/A/JavaLaunching)
else()
  target_link_libraries(jplYap  libYap ${JAVA_JVM_LIBRARY}  ${JAVA_AWT_LIBRARY})
endif()

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl
    INSTALL_RPATH_USE_LINK_PATH TRUE )

  # set(YAP_SYSTEM_OPTIONS "jpl " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  install(TARGETS  jplYap
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
	  )
