
# Sources Section
#

set(LIBSWI_OS_MAJOR_VERSION 0)
set(LIBSWI_OS_MINOR_VERSION 60)
set(LIBSWI_OS_PATCH_VERSION 0)
set(LIBSWI_OS_FULL_VERSION
  ${LIBSWI_OS_MAJOR_VERSION}.${LIBSWI_OS_MINOR_VERSION}.${LIBSWI_OS_PATCH_VERSION})


set(LIBSWI_OS_SOURCES
  pl-buffer.c
  pl-codelist.c
  pl-ctype.c
  pl-dtoa.c
  pl-error.c
  pl-file.c
  pl-files.c
  pl-fmt.c
  pl-glob.c
  pl-option.c
  pl-nt.c
  pl-os.c
  pl-privitf.c
  pl-prologflag.c
  pl-read.c
  pl-rl.c
  pl-stream.c
  pl-string.c
  pl-table.c
  pl-tai.c
  pl-text.c
  pl-utf8.c
  pl-write.c
  )


if (WIN32)
  set(LIBSWI_OS_SOURCES
    ${LIBSWI_OS_SOURCES}
    os/windows/uxnt.c
    )
endif (WIN32)

#
# include libtai package as an independent library
#
add_subdirectory (libtai)

# ugh
set (POSITION_INDEPENDENT_CODE TRUE)

add_component (libswi_os 
  ${LIBSWI_OS_SOURCES}
  $<TARGET_OBJECTS:libyaptai>
  )

set_target_properties(libswi_os
  PROPERTIES
  #  RPATH ${CMAKE_INSTALL_LIBDIR} VERSION ${LIBSWI_OS_FULL_VERSION}
  #  SOVERSION ${LIBSWI_OS_MAJOR_VERSION}.${LIBSWI_OS_MINOR_VERSION}
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME swi_os
  )



