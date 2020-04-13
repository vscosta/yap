
# PROJECT ( YAP_REAL C )

  set(YAP4R_SOURCES
    yap4r/man/yap4r-package.Rd
yap4r/R/zzz.R
yap4r/NAMESPACE
yap4r/DESCRIPTION
yap4r/src/yap4r.cpp
)

set(REAL_SOURCES real.c)

# LIBR_FOUND
# LIBR_HOME
# LIBLIBR_INCLUDE_DIRSS
# LIBR_DOC_DIR
# LIBR_LIBRARIES


# This code sets the following variables
# R_EXEC      - path to R executable
# R_LIBS_USER - path to directory of user's R packages (defined only if R is found) 
#
# It also defines the following help functions if R is found:
# FIND_R_PACKAGE(package)            - sets R_<PACKAGE> to ON if package is installed 
# INSTALL_R_PACKAGE(package)         - installs package in ${R_LIBS_USER}
# FIND_OR_INSTALL_R_PACKAGE(package) - finds package and installs it, if not found

find_program(R_EXEC R)


find_package_handle_standard_args(R DEFAULT_MSG R_EXEC)



if (LIBR_LIBRARIES AND LIBR_INCLUDE_DIRS)
set_package_properties(R PROPERTIES
        DESCRIPTION "The R Project for Statistical Computing."
 
       URL "https://www.r-project.org/")




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
  check_include_files( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )
  check_include_file(  "Rcpp" HAVE_RCPP)

  configure_file ("rconfig.h.cmake" "Rconfig.h" )
  configure_file ("yap4r/src/Makevars.in" "yap4r/src/Makevars" )

  add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp 
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/R/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/src/yap4r.cpp ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src
    )

  if(HAVE_RCPP)

# add_custom_target(YAP4R 
# COMMAND ${R_EXEC} --slave -e "library(Rcpp); compileAttributes('yap4r')"
# COMMAND ${R_EXEC} CMD build yap4r
#  VERBATIM
#  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
# DEPENDS real  ${YAP4R_SOURCES}
# )

 

endif()

install(TARGETS  real
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

  install(FILES real.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )

endif()
