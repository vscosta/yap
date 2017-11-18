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

FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/java/pt/up/yap/lib )
FILE( MAKE_DIRECTORY ${YAP_APP_DIR}/src/generated/assets)
  set(CMAKE_SWIG_OUTDIR    ${YAP_APP_DIR}/src/generated/java )
  set(SWIG_OUTFILE_OUTDIR    ${CMAKE_CURRENT_SOURCE_DIR} )
  set_property(SOURCE ../yap.i PROPERTY CPLUSPLUS ON)
  set_property(SOURCE streamer.i PROPERTY CPLUSPLUS ON)
  set( SWIG_MODULE_NAME pt.up.yap.lib )

set (SWIG_SOURCES ../yap.i streamer.i streamer.cpp)

SWIG_ADD_LIBRARY(YAPJava
                  SHARED          # [TYPE <SHARED|MODULE|STATIC|USE_BUILD_SHARED_LIBS>]
                 LANGUAGE java
                 SOURCES ../yap.i
                 )

SWIG_ADD_LIBRARY(YAPStreamer
                  SHARED          # [TYPE <SHARED|MODULE|STATIC|USE_BUILD_SHARED_LIBS>]
                 LANGUAGE java
                 SOURCES streamer.i streamer.cpp streamer.h
                 )

target_link_libraries(YAPJava YAP++ android log)

target_link_libraries(YAPStreamer YAP++ android log)



install(TARGETS  YAPJava  YAPStreamer
  RUNTIME DESTINATION ${dlls}
  LIBRARY DESTINATION c{libdir}
  ARCHIVE DESTINATION ${libdir}
  )
