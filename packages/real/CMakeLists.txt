
# PROJECT ( YAP_REAL C )


# LIBR_FOUND
# LIBR_HOME
# LIBR_INCLUDE_DIRS
# LIBR_DOC_DIR
# LIBR_LIBRARIES
#
macro_optional_find_package (LibR ON)

macro_log_feature (LIBR_FOUND "R"
  "Use R Environment"
  "http://www.r.org" FALSE)

if (LIBR_FOUND)

  set (REAL_SOURCES
    real.c
)

set (REAL_PL
  real.pl
)

add_to_group( pl_library REAL_PL )

include_directories (
      ${CMAKE_CURRENT_BINARY_DIR}
      ${LIBR_INCLUDE_DIRS}
      )

    set (CMAKE_REQUIRED_INCLUDES
        ${CMAKE_CURRENT_BINARY_DIR}
        ${LIBR_INCLUDE_DIRS}
    )

  add_library (real SHARED ${REAL_SOURCES})

  target_link_libraries (real ${LIBR_LIBRARIES}   libYap)

  check_include_files( "stdio.h;R.h" HAVE_R_H )
  check_include_files( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )

  configure_file ("rconfig.h.cmake" "rconfig.h" )

 install(TARGETS  real
    RUNTIME DESTINATION ${dlls}
    ARCHIVE DESTINATION ${dlls}
    LIBRARY DESTINATION ${dlls}
    )

  install(FILES real.pl
    DESTINATION ${libpl}
    )


endif (LIBR_FOUND)
