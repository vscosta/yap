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
  mem.c
  open_memstream.c
  pipes.c
  readline.c
  random.c
  readterm.c
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

  include_directories (../H ../include ../OPTYap . cwalk  ${GMP_INCLUDE_DIR} ${PROJECT_BINARY_DIR} PARENT_SCOPE)

SET_PROPERTY(DIRECTORY PROPERTY COMPILE_DEFINITIONS YAP_KERNEL=1 )

set (POSITION_INDEPENDENT_CODE TRUE)

  add_component (libYAPOs
    ${YAPOS_SOURCES}
    )

  MY_set_target_properties(libYAPOs
    PROPERTIES
    #  RPATH ${CMAKE_INSTALL_LIBDIR} VERSION ${LIBYAPTAI_FULL_VERSION}
    #  SOVERSION ${LIBYAPTAI_MAJOR_VERSION}.${LIBYAPTAI_MINOR_VERSION}
    POSITION_INDEPENDENT_CODE TRUE
    )


  #set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GMP_LIBRARIES} )

  set (YAPOS_PL_SOURCES
      edio.yap
      chartypes.yap
      yio.yap
  )

add_to_dir(YAPOS PL_SOURCES ${YAP_INSTALL_DATADIR}/os)

   install (FILES ${YAPOS_PL_SOURCES}
    DESTINATION ${YAP_INSTALL_DATADIR}/os )
