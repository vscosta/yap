
  # This is a CMake example for Python and Java


  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/app/src/main/java/pt/up/yap/lib )

    set ( SWIG_SOURCES ../yap.i )
    SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

  include_directories (
    ${CMAKE_SOURCE_DIR}/CXX
       )

      add_custom_command (OUTPUT  yap_swig.cpp
        COMMAND ${SWIG} -java -outdir ${CMAKE_SWIG_OUTDIR} -outcurrentdir -addextern  -I${CMAKE_SOURCE_DIR}/CXX   ${SWIG_SOURCES} -o yap_swig.cpp
      )

   add_custom_target ( swig ALL
      DEPENDS yap_swig.cpp
      )

