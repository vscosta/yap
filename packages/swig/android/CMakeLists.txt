
  # This is a CMake example for Python and Java

  INCLUDE(UseJava)



  #  set(ANDROID_SO_OUTDIR  ${libdir})
  #  set(CMAKE_SWIG_OUTDIR  ${CMAKE_SOURCE_DIR}/android/yaplib/build/generated/source/pt/up/yap/lib)
  #set(CMAKE_SWIG_OUTDIR  ${CMAKE_CURRENT_BINARY_DIR}/derived)


    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
    ${JAVA_INCLUDE_DIRS}
    ${JNI_INCLUDE_DIRS}
      )


    add_custom_command (OUTPUT ${SWIG_CXX}
        COMMAND ${SWIG} -java -outdir ${CMAKE_CURRENT_BINARY_DIR} -c++ -addextern  -I${CMAKE_SOURCE_DIR}/CXX  ${SWIG_SOURCES}  -o ${SWIG_CXX}
 ${SWIG_SOURCES}
      )
