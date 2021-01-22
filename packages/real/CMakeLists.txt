
# PROJECT ( YAP_REAL C )

  set(YAP4R_SOURCES
    yap4r/man/yap4r-package.Rd
yap4r/R/zzz.R
yap4r/NAMESPACE
yap4r/DESCRIPTION
yap4r/src/yap4r.cpp
)

set(REAL_SOURCES real.c real.pl)

# This module defines the following variables:
#
#   R_INCLUDE_DIRS: include directories for R
#   R_LIBRARIES: libraries to link against R
#   R_PACKAGES_DIRS: directories with installed R packages
#   R_FOUND: true if R has been found and can be used



if (R_LIBRARIES AND R_INCLUDE_DIRS)
set_package_properties(R PROPERTIES
        DESCRIPTION "The R Project for Statistical Computing."

       URL "https://www.r-project.org/")




add_library(real  ${REAL_SOURCES})
  target_link_libraries (real ${R_LIBRARIES}   libYap)
include_directories (
  ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}
      ${R_INCLUDE_DIRS}
      )

    list (APPEND CMAKE_REQUIRED_INCLUDES
      ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_SOURCE_DIR}/include
        ${R_INCLUDE_DIRS}
    )	

if (APPLE)
set(PKG_LIBS ${CMAKE_SHARED_LINKER_FLAGS} -L${YAP_LIBDIeR} -L${YAP_DLLDIR} -lreal -lYAP++ -lYap
	 )
else()
set (PKG_LIBS ${CMAKE_SHARED_LINKER_FLAGS}
    -Wl,-rpath=${YAP_LIBDIR} -Wl,-rpath=${YAP_DLLDIR}
         -L${YAP_LIBDIR} -L${YAP_DLLDIR} -lreal -lYAP++ -lYap
	 )
endif()

check_include_files( "stdio.h;R.h" HAVE_R_H )
  check_include_files( "R.h;Rembedded.h" HAVE_R_EMBEDDED_H )
  check_include_files( "Rembedded.h;Rinterface.h" HAVE_R_INTERFACE_H )
  check_include_files(  "Rcpp.h" HAVE_RCPP_H)

  configure_file ("rconfig.h.cmake" "Rconfig.h" )

                


set (INTERMEDIATE_FILES  ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src/yap4r.cpp 
 )


  add_custom_command(OUTPUT  ${INTERMEDIATE_FILES}
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/man/yap4r-package.Rd ${CMAKE_CURRENT_BINARY_DIR}/yap4r/man
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/R/zzz.R ${CMAKE_CURRENT_BINARY_DIR}/yap4r/R
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/NAMESPACE ${CMAKE_CURRENT_BINARY_DIR}/yap4r
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/DESCRIPTION ${CMAKE_CURRENT_BINARY_DIR}/yap4r
    COMMAND ${CMAKE_COMMAND} -E copy  ${CMAKE_CURRENT_SOURCE_DIR}/yap4r/src/yap4r.cpp ${CMAKE_CURRENT_BINARY_DIR}/yap4r/src
    )

 add_custom_target(YAP4R
 COMMAND ${R_EXECUTABLE} -vanilla -slave -e  "library(Rcpp);compileAttributes('yap4r')" 
 COMMAND ${R_EXECUTABLE}  CMD build  yap4r
 VERBATIM
 DEPENDS  ${INTERMEDIATE_FILES}
 )

install(CODE "execute_process(
COMMAND ${R_EXECUTABLE} CMD INSTALL yap4r)"
)




install(TARGETS  real
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

  install(FILES real.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )

endif()
