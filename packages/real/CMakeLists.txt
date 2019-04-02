
# PROJECT ( YAP_REAL C )

  set(YAP4R_SOURCES
    yap4r/man/yap4r-package.Rd
yap4r/R/zzz.R
yap4r/NAMESPACE
yap4r/DESCRIPTION
yap4r/src/yap4r.cpp
yap4r/src/RcppExports.cpp
)

set(REAL_SOURCES real.c)

# LIBR_FOUND
# LIBR_HOME
# LIBLIBR_INCLUDE_DIRSS
# LIBR_DOC_DIR
# LIBR_LIBRARIES

if (LIBR_LIBRARIES AND LIBR_INCLUDE_DIRS)
set_package_properties(R PROPERTIES
        DESCRIPTION "The R Project for Statistical Computing."
 
       URL "https://www.r-project.org/")


foreach(f ${YAP4R_SOURCES})
  add_custom_command(OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/${f}
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${f} ${CMAKE_CURRENT_BINARY_DIR}/${f}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${f}
    )
  list(APPEND OUTS ${CMAKE_CURRENT_BINARY_DIR}/${f} )
endforeach()

add_custom_target(YAP4R
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
 DEPENDS ${OUTS} 
 )

add_library(real  ${REAL_SOURCES})
  target_link_libraries (real ${LIBR_LIBRARIES}   libYap)
include_directories (
  ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}
      ${LIBR_INCLUDE_DIRS}
      )

    list (APPEND CMAKE_REQUIRED_INCLUDES
      ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_SOURCE_DIR}/include
        ${LIBR_INCLUDE_DIRS}
    )

  check_include_files( "stdio.h;R.h" HAVE_R_H )
  check_include_files( "R.h,;Rembeddred.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )

  configure_file ("rconfig.h.cmake" "rconfig.h" )
  configure_file ("yap4r/src/Makevars.in" "yap4r/src/Makevars" )

  
  add_dependencies(real YAP4R)
  
 install(TARGETS  real
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

  install(FILES real.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )

  endif()
