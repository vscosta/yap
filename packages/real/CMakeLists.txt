
# PROJECT ( YAP_REAL C )

macro_optional_find_package (R ON)

  macro_log_feature (R_FOUND "R"
    "Use R Environment"
    "http://www.r.org" FALSE)

if (R_FOUND)

  set (REAL_SOURCES
    real.c
    real.pl
)

include_directories (
      ${CMAKE_CURRENT_BINARY_DIR}
      ${R_INCLUDE_DIR}
      )

    set (CMAKE_REQUIRED_INCLUDES
        ${CMAKE_CURRENT_BINARY_DIR}
        ${R_INCLUDE_DIR}
    )


# The module defines the following variables:
#  R_FOUND - System has R
#  R_EXECUTABLE     - The R interpreter
#  R_INCLUDE_DIR    - the R include directory
#  R_LIBRARIES      - The libraries needed to use R
#  R_VERSION_STRING - R version

  add_library (real SHARED ${REAL_SOURCES})

  target_link_libraries (real ${R_LIBRARIES}  ${CMAKE_DL_LIBS} libYap)

  set_target_properties (real PROPERTIES PREFIX "")

  check_include_files( R.h HAVE_R_H )
  check_include_files ( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )

  configure_file ("rconfig.h.cmake" "rconfig.h" )

 install(TARGETS  real
    LIBRARY DESTINATION ${dlls}
    ARCHIVE DESTINATION ${dlls}
    )

  install(FILES real.pl
    DESTINATION ${libpl}
    )


endif (R_FOUND)
