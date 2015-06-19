#CHECK: SWIG
macro_optional_find_package (SWIG ON)
macro_log_feature (SWIG_FOUND "Swig"
  "Use SWIG Documentation System "
  "http://www.swig.org" FALSE)
if (SWIG_FOUND)
  #
  #   SWIG_FOUND - set to true if SWIG is found
  #   SWIG_DIR - the directory where swig is installed
  #   SWIG_EXECUTABLE - the path to the swig executable
  #   SWIG_VERSION   - the version number of the swig executable
  #

  # This is a CMake example for Python

  INCLUDE(${SWIG_USE_FILE})

  FIND_PACKAGE(PythonLibs)
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/CXX)

  set_source_files_properties( yap.i
                               PROPERTIES CPLUSPLUS ON)

SET(CMAKE_SWIG_FLAGS "")

set (CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR}/java )

macro_optional_find_package(Java ON)
find_package(Java COMPONENTS Development)
#find_package(Java COMPONENTS Runtime)
macro_log_feature (Java_Development_FOUND "Java"
    "Use Java System"
    "http://www.java.org" FALSE)
macro_optional_find_package(JNI ON)
    macro_log_feature (JNI_FOUND "JNI"
    "Use Java Native Interface"
    "http://www.java.org" FALSE)


if (Java_Development_FOUND)

    #set (CMAKE_JAVA_CLASS_OUTPUT_PATH java)

    include ( UseJava  )

    SWIG_ADD_MODULE(jSWIG java yap.i )

            SWIG_LINK_LIBRARIES(jSWIG ${JAVA_LIBRARIES} ${JNI_LIBRARIES} Yap++)

            #include( UseJavaClassFileList)
            set_source_files_properties(yap.i PROPERTY CPLUSPLUS ON)

            target_include_directories ( jSWIG
                PUBLIC ${JAVA_INCLUDE_DIRS} ${JNI_INCLUDE_DIRS})

                INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

#set (CMAKE_JAVA_CLASS_OUTPUT_PATH ${CMAKE_CURRENT_DIR}/java )

#include (UseJavaClassFilelist)

add_subdirectory( java )

endif()

endif (SWIG_FOUND)
