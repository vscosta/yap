
PROJECT ( YAP_REAL C )

macro_optional_find_package (R ON)

  macro_log_feature (R_HOME "R"
    "Use R Environment"
    "http://www.r.org" FALSE)

if (R_HOME)

  set (REAL_SOURCES
    real.c
    real.pl
)

include_directories (
      ${R_INCLUDE_DIR}
      ${CMAKE_CURRENT_BINARY_DIR}
      )

    set (CMAKE_REQUIRED_INCLUDES
        ${R_INCLUDE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}
    )


  #R_COMMAND           - Path to R command
  #  R_HOME              - Path to 'R home', as reported by R
  #  R_INCLUDE_DIR       - Path to R include directory
  #  R_LIBRARY_BASE      - Path to R library
  #  R_LIBRARY_BLAS      - Path to Rblas / blas library
  #  R_LIBRARY_LAPACK    - Path to Rlapack / lapack library
  #  R_LIBRARY_READLINE  - Path to readline library
  #  R_LIBRARIES         - Array of: R_LIBRARY_BASE, R_LIBRARY_BLAS, R_LIBRARY_LAPACK, R_LIBRARY_BASE [, R_LIBRARY_READLINE]

  add_library (real SHARED ${REAL_SOURCES})

  target_link_libraries (real ${R_LIBRARIES} libYap )

  set_target_properties (real PROPERTIES PREFIX "")

  check_include_files( R.h HAVE_R_H )
  check_include_files ( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )

  configure_file ("rconfig.h.cmake" "rconfig.h" )

 install(TARGETS  real
    LIBRARY DESTINATION ${dlls}
    )

  install(FILES real.pl
    DESTINATION ${libpl}
    )


endif (R_HOME)
