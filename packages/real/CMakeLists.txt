
# PROJECT ( YAP_REAL C )



# LIBR_FOUND
# LIBR_HOME
# LIBR_INCLUDE_DIRS
# LIBR_DOC_DIR
# LIBR_LIBRARIES

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
