

  include_directories (../H ../include ../OPTYap . cwalk  ${GMP_INCLUDE_DIR} )

set (YAPOS_HEADERS
        getw.h

        iopreds.h
        yapio.h
        YapEncoding.h
        )

set (YAPOS_SOURCES
absf.c
  alias.c
  assets.c
  charsio.c
  chartypes.c
  console.c
  files.c
  fmem.c
 # fmemopen.c
 #android/fmemopen.c
  #  android/fopencookie.c
   #   android/open_memstream.c
  format.c
  iopreds.c
  #open_memstream.c
  pipes.c
  random.c
  readterm.c
readline/readline.c
readutil.c
  sig.c
  sockets.c
  streams.c
  sysbits.c
  time.c
  writeterm.c
  ypsocks.c
  ypstdio.c
 cwalk/cwalk.c
 )


SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

set (POSITION_INDEPENDENT_CODE TRUE)

list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/os/readline/cmake)

if (WITH_READLINE)
include(libreadline)
endif()

 
  add_component (libYAPOs
    ${YAPOS_SOURCES}

    )


configure_file(${CMAKE_SOURCE_DIR}/os/YapIOConfig.h.cmake ${CMAKE_BINARY_DIR}/YapIOConfig.h) 


  set (YAPOS_PL_SOURCES
      chartypes.yap
      edio.yap
       os.yap
      yio.yap
  )



add_to_dir(YAPOS PL_SOURCES ${YAP_INSTALL_DATADIR}/os)

   install (FILES ${YAPOS_PL_SOURCES}
    DESTINATION ${YAP_INSTALL_DATADIR}	 )



