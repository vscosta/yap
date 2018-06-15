
# PROJECT ( YAP_REAL C )


#
# - This module locates an installed R distribution.
#
# Defines the following:
#  R_COMMAND           - Path to R command
#  R_HOME              - Path to 'R home', as reported by R
#  R_INCLUDE_DIR       - Path to R include directory
#  R_LIBRARY_BASE      - Path to R library
#  R_LIBRARY_BLAS      - Path to Rblas / blas library
#  R_LIBRARY_LAPACK    - Path to Rlapack / lapack library
#  R_LIBRARY_READLINE  - Path to readline library
#  R_LIBRARIES         - Array of: R_LIBRARY_BASE, R_LIBRARY_BLAS, R_LIBRARY_LAPACK, R_LIBRARY_BASE [, R_LIBRARY_READLINE]
#
#  VTK_R_HOME          - (deprecated, use R_HOME instead) Path to 'R home', as reported by R
#
# Variable search order:
#   1. Attempt to locate and set R_COMMAND
#                                                   If unsuccessful, generate error and prompt user to manually set R_COMMAND
#   2. Use R_COMMAND to set R_HOME
#   3. Locate other libraries in the priority:
#     1. Within a user-built instance of R at R_HOME
#     2. Within an installed instance of R
#     3. Within external system libraries
#

if (R_LIBRARIES AND R_INCLUDE_DIR)
set_package_properties(R PROPERTIES
        DESCRIPTION "The R Project for Statistical Computing."
 
       URL "https://www.r-project.org/")
add_lib(real  ${REAL_SOURCES})
  target_link_libraries (real ${R_LIBRARIES}   libYap)
include_directories (
     ${CMAKE_CURRENT_BINARY_DIR}
      ${R_INCLUDE_DIR}
      )

    list (APPEND CMAKE_REQUIRED_INCLUDES
        ${CMAKE_CURRENT_BINARY_DIR}
        ${R_INCLUDE_DIR}
    )

  check_include_files( "stdio.h;R.h" HAVE_R_H )
  check_include_files( "R.h,;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )

  configure_file ("rconfig.h.cmake" "rconfig.h" )

 install(TARGETS  real
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    )

  install(FILES real.pl
    DESTINATION ${libpl}
    )

  endif()
