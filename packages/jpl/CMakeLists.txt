#CHECK: JavaLibs

set (JPL_SOURCES
  src/c/jpl.c)

macro_optional_find_package(Java ON)
find_package(Java COMPONENTS Development)
#find_package(Java COMPONENTS Runtime)
macro_log_feature (Java_Development_FOUND "Java"
    "Use Java System"
    "http://www.java.org" FALSE)

if (Java_Development_FOUND)
  #   Java_JAVA_EXECUTABLE    = the full path to the Java runtime
  #   Java_JAVAC_EXECUTABLE   = the full path to the Java compiler
  #   Java_JAVAH_EXECUTABLE   = the full path to the Java header generator
  #   Java_JAVADOC_EXECUTABLE = the full path to the Java documention generator
  #   Java_JAR_EXECUTABLE     = the full path to the Java archiver
  #   Java_VERSION_STRING     = Version of java found, eg. 1.6.0_12
  #   Java_VERSION_MAJOR      = The major version of the package found.
  #   Java_VERSION_MINOR      = The minor version of the package found.
  #   Java_VERSION_PATCH      = The patch version of the package found.
  #   Java_VERSION_TWEAK      = The tweak version of the package found (after '_')
  #   Java_VERSION            = This is set to: $major.$minor.$patch(.$tweak)
  #   JAVA_LIBRARIES           - path to the java library
  #   JAVA_INCLUDE_PATH        - path to where Java.h is found (deprecated)
  #   JAVA_INCLUDE_DIRS        - path to where Java.h is found
  #   JAVA_DEBUG_LIBRARIES     - path to the debug library (deprecated)
  #   JAVALIBS_VERSION_STRING  - version of the Java libs found (since CMake 2.8.8)
  #
  #
  #
  # The Java_ADDITIONAL_VERSIONS variable can be used to specify a list
  # of version numbers that should be taken into account when searching
  # for Java.  You need to set this variable before calling
  # find_package(JavaLibs).
  #
  macro_optional_find_package(JNI ON)
  #   JNI_INCLUDE_DIRS      = the include dirs to use
  #   JNI_LIBRARIES         = the libraries to use
  #   JNI_FOUND             = TRUE if JNI headers and libraries were found.
  #   JAVA_AWT_LIBRARY      = the path to the jawt library
  #   JAVA_JVM_LIBRARY      = the path to the jvm library
  #   JAVA_INCLUDE_PATH     = the include path to jni.h
  #   JAVA_INCLUDE_PATH2    = the include path to jni_md.h
  #   JAVA_AWT_INCLUDE_PATH = the include path to jawt.h

  add_library (jplYap SHARED src/c/jpl.c)

  add_subdirectory (src/java)

  include_directories (${JAVA_INCLUDE_DIRS} ${JNI_INCLUDE_DIRS})

  target_link_libraries(jplYap libYap ${JAVA_LIBRARIES} ${JNI_LIBRARIES})

  set_target_properties(jplYap PROPERTIES
    OUTPUT_NAME jpl )

  install(TARGETS  jplYap
    LIBRARY DESTINATION ${dlls}
    )

  install(FILES jpl.pl
    DESTINATION ${libpl}
    )


endif (Java_Development_FOUND)
