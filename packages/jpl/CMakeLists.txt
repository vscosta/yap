
#CHECK: JavaLibs

set (JPL_SOURCES
  src/c/jpl.c)

if (Java_Development_FOUND)
  
  if (JNI_FOUND)


  add_subdirectory (src/c)

  endif (JNI_FOUND)

  add_subdirectory (src/java)

  install(FILES jpl.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )

endif (Java_Development_FOUND)
