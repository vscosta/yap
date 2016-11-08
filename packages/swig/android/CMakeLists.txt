
  # This is a CMake example for Python and Java


  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/app/src/main/java/pt/up/yap/lib )

    set ( SWIG_SOURCES ../yap.i )
    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
       )

     add_custom_target ( swig ALL
      DEPENDS ${SWIG_CXX}
      )

    add_custom_command (OUTPUT   ${SWIG_CXX}
        COMMAND ${SWIG} -java -outdir ${CMAKE_SWIG_OUTDIR} -c++ -addextern  -I${CMAKE_SOURCE_DIR}/CXX   -o ${SWIG_CXX}${SWIG_SOURCES}
      )

