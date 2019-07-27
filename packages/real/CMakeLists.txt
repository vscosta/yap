
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

if(R_EXEC)
	# R_LIBS_USER is always defined within R, even if it is not explicitly set by the user
	execute_process(COMMAND "${R_EXEC}" "--slave" "-e" 
		"print(Sys.getenv(\"R_LIBS_USER\"))"
		OUTPUT_VARIABLE _rlibsuser
		OUTPUT_STRIP_TRAILING_WHITESPACE)
	string(REGEX REPLACE "[ ]*(R_LIBS_USER)[ ]*\n\"(.*)\"" "\\2"
		R_LIBS_USER ${_rlibsuser})
	
	function(find_r_package package)
		string(TOUPPER ${package} package_upper)
		if(NOT R_${package_upper})
			if(ARGC GREATER 1 AND ARGV1 STREQUAL "REQUIRED")
				set(${package}_FIND_REQUIRED TRUE)
			endif()
			execute_process(COMMAND "${R_EXEC}" "--slave" "-e" 
				"library('${package}')"
				RESULT_VARIABLE _${package}_status 
				ERROR_QUIET OUTPUT_QUIET)
			if(NOT _${package}_status)
				set(R_${package_upper} TRUE CACHE BOOL 
					"Whether the R package ${package} is installed")
			endif(NOT _${package}_status)
		endif(NOT R_${package_upper})
		find_package_handle_standard_args(R_${package} DEFAULT_MSG R_${package_upper})
	endfunction(find_r_package)
	
	function(install_r_package package)
		message(STATUS "Installing R package ${package}...")
		execute_process(COMMAND "${R_EXEC}" "--slave" "-e"
			"install.packages('${package}','${R_LIBS_USER}','http://cran.r-project.org')"
			ERROR_QUIET OUTPUT_QUIET)
		message(STATUS "R package ${package} has been installed in ${R_LIBS_USER}")
	endfunction(install_r_package)
	
	function(find_or_install_r_package package)
		find_r_package(${package})
		string(TOUPPER ${package} package_upper)
		if(NOT R_${package_upper})
			install_r_package(${package})
		endif()
	endfunction(find_or_install_r_package)

find_package_handle_standard_args(R DEFAULT_MSG R_EXEC)



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

	function(find_or_install_r_package Rcpp)
	find_r_package(${package})
		string(TOUPPER ${package} package_upper)
		if(NOT R_${package_upper})
			install_r_package(${package})
		endif()
	endfunction(find_or_install_r_package)
endif(R_EXEC)

add_custom_target(YAP4R
COMMAND "${R_EXEC}" "--slave" "-e" "library\\(Rcpp\\) \\&\\& compileAttributes\\(\\"yap4r\\"\\)"
; "${R_EXEC}" "CMD" "build" "yap4r"
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
 DEPENDS ${OUTS} 
 )
  add_dependencies(real YAP4R)
  
 install(TARGETS  real
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

  install(FILES real.pl
    DESTINATION ${YAP_INSTALL_DATADIR}
    )


endif(R_EXEC)
