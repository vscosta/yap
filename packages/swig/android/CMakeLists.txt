include (UseSWIG)

INCLUDE_DIRECTORIES(
  ../../../H
  ../../../H/generated
  ../../../OPTYap
  ../../../include
  ../../../CXX
  .
  ..
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set_property(GLOBAL
        APPEND PROPERTY
        COMPILE_DEFINITIONS
        -Dmmap=mmap64)


  # This is a CMake file for SWIG and Android

  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/src/generated/java/pt/up/yap/lib )

FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/assets)
FILE( MAKE_DIRECTORY ${CMAKE_SWIG_OUTDIR})
  set(SWIG_OUTFILE_DIR    ${CMAKE_CURRENT_BINARY_DIR} )
  set_property(SOURCE ../yap.i PROPERTY CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(../yap.i PROPERTIES SWIG_FLAGS "-O;-package;pt.up.yap.lib;-D__ANDROID__=1")

  set( SWIG_PACKAGE_NAME pt.up.yap.lib )

set (SWIG_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../yap.i )


  add_custom_command( OUTPUT ${SWIG_OUTFILE_DIR}/yapJAVA.cxx
    COMMAND ${SWIG_EXECUTABLE} -c++  -java -O -package "pt.up.yap.lib"  -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/H/generated -I${CMAKE_SOURCE_DIR}/include
     -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/utf8proc -I.././.. -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/packages/python
      -outdir ${CMAKE_SWIG_OUTDIR} -I${GMP_INCLUDE_DIRS} -D__ANDROID__=1 -DX_API=""  -o ${SWIG_OUTFILE_DIR}/yapJAVA.cxx  -oh ${SWIG_OUTFILE_DIR}/yapJAVA.hh ${CMAKE_CURRENT_SOURCE_DIR}/../yap.i
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS  ${SWIG_SOURCES}
  )

ADD_LIBRARY(YAPJava
                  SHARED
                  ${SWIG_OUTFILE_DIR}/yapJAVA.cxx
                 )

target_link_libraries(YAPJava YAP++ libYap android log)

 set_property(SOURCE streamer.i PROPERTY CPLUSPLUS ON)
  SET_SOURCE_FILES_PROPERTIES(streamer.i PROPERTIES SWIG_FLAGS "-O;-package;pt.up.yap.lib;-D__ANDROID__=1")


set (SWIG_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/streamer.i)



  add_custom_command( OUTPUT ${SWIG_OUTFILE_DIR}/streamerJAVA.cxx
    COMMAND ${SWIG_EXECUTABLE} -c++  -java -O -package "pt.up.yap.lib" -O -I${CMAKE_SOURCE_DIR}/H -I${CMAKE_SOURCE_DIR}/H/generated -I${CMAKE_SOURCE_DIR}/include
     -I${CMAKE_SOURCE_DIR}/OPTYap -I${CMAKE_SOURCE_DIR}/os -I${CMAKE_SOURCE_DIR}/utf8proc -I.././.. -I${CMAKE_SOURCE_DIR}/CXX -I${CMAKE_SOURCE_DIR}/packages/python
      -outdir ${CMAKE_SWIG_OUTDIR} -I${GMP_INCLUDE_DIRS} -D__ANDROID__=1 -DX_API=""  -o ${SWIG_OUTFILE_DIR}/streamerJAVA.cxx  -oh ${SWIG_OUTFILE_DIR}/streamerJAVA.hh ${CMAKE_CURRENT_SOURCE_DIR}/streamer.i
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS  ${SWIG_SOURCES}
  )

ADD_LIBRARY(YAPStreamer
                  SHARED          # [TYPE <SHARED|MODULE|STATIC|USE_BUILD_SHARED_LIBS>]
                  ${CMAKE_CURRENT_SOURCE_DIR}/streamer.cpp ${SWIG_OUTFILE_DIR}/streamerJAVA.cxx
                 )

target_link_libraries(YAPStreamer YAP++ libYap android log)

