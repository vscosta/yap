
      # This is a CMake file for SWIG and Android

      set(GENERATED_SOURCE_DIR ${CMAKE_SOURCE_DIR}/../yaplib/src/generated)

      set(JAVA_SWIG_OUTDIR ${GENERATED_SOURCE_DIR}/java/pt/up/yap/lib)

      set(SWIG_SOURCES ${CMAKE_SOURCE_DIR}/packages/swig/yap.i)
      SET_SOURCE_FILES_PROPERTIES(${SWIG_SOURCES} PROPERTIES CPLUSPLUS ON)

      FILE( MAKE_DIRECTORY ${GENERATED_SOURCE_DIR}/assets/Yap/pl)
      FILE( MAKE_DIRECTORY ${GENERATED_SOURCE_DIR}/assets/os)


      include_directories(
              ${CMAKE_CURRENT_SOURCE_DIR}
              ${CMAKE_CURRENT_BINARY_DIR}
              ${CMAKE_SOURCE_DIR}/CXX
              ${CMAKE_SOURCE_DIR}/include
              ${CMAKE_BINARY_DIR}
              ${CMAKE_SOURCE_DIR}/H
              ${CMAKE_SOURCE_DIR}/os
              ${CMAKE_SOURCE_DIR}/OPTYap
              )

  add_custom_command( OUTPUT yapi_swig.cxx yapi_swig.hh
  COMMAND  ${SWIG_EXECUTABLE} -c++ -java -package pt.up.yap.lib -O -outdir ${JAVA_SWIG_OUTDIR}
      -addextern -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/include
      -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/OPTYap
      -I${CMAKE_BINARY_DIR}
          -I${GMP_INCLUDE_DIRS} -DX_API -o yapi_swig.cxx ${SWIG_SOURCES}
 DEPENDS  ${CMAKE_SOURCE_DIR}/CXX/yapi.hh ${SWIG_SOURCES}
          )

  add_custom_command( OUTPUT  streamer_swig.cxx streamer_swig.hh
    COMMAND ${SWIG_EXECUTABLE} -c++ -java -package pt.up.yap.lib -O -outdir ${JAVA_SWIG_OUTDIR} -addextern -I${CMAKE_CURRENT_SOURCE_DIR} -o streamer_swig.cxx streamer.i
 DEPENDS  ${CMAKE_SOURCE_DIR}/CXX/yapi.hh ${CMAKE_CURRENT_SOURCE_DIR}/streamer.i
 )

      add_library(DROID OBJECT
          yapi_swig.cxx  streamer_swig.cxx streamer.cpp
          )
