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

FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/main/java/pt/up/yap/lib )
FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/main/assets)
  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/src/main/java )
  set(SWIG_OUTFILE_OUTDIR    ${CMAKE_CURRENT_SOURCE_DIR} )
  set_property(SOURCE ../yap.i PROPERTY CPLUSPLUS ON)
  set_property(SOURCE streamer.i PROPERTY CPLUSPLUS ON)
  set( SWIG_MODULE_NAME pt.up.yap.lib )
  set ( pllib ${YAP_APP_DIR}/src/main/assets/Yap )

set (SWIG_SOURCES ../yap.i streamer.i streamer.cpp)

add_library(YAPJava SHARED ${YAP_APP_DIR}/src/generated/jni/yap_swig.cpp)

add_library(YAPStreamer SHARED ${YAP_APP_DIR}/src/generated/jni/streamer_swig.cpp streamer.cpp)

target_link_libraries(YAPJava YAP++ android log)

target_link_libraries(YAPStreamer YAP++ android log)
