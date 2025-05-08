# FindRcpp.cmake
# - Try to find Rcpp
#
# The following variables are optionally searched for defaults
#  Rcpp_ROOT_DIR:  Base directory where all Rcpp components are found
#
# Once done this will define
#  Rcpp_FOUND - System has Rcpp
#  Rcpp_INCLUDE_DIRS - The Rcpp include directories
#  Rcpp_LIBRARIES - The libraries needed to use Rcpp

#set(Rcpp_ROOT_DIR "" CACHE PATH "Folder containing Rcpp")

if (NOT R_FOUND)
	find_package(R REQUIRED)
endif ()

# ask R for the package directories
if (NOT LIBR_PACKAGE_DIRS)
	execute_process(
			COMMAND ${R_COMMAND} "--slave" "--no-save" "-e" " cat(paste(.libPaths(), sep='', collapse=';'))"
			OUTPUT_VARIABLE LIBR_PACKAGE_DIRS
	)
	if (LIBR_PACKAGE_DIRS)
		message(STATUS "Detected R package library directories: ${LIBR_PACKAGE_DIRS}")
	endif ()
endif ()

find_path(Rcpp_INCLUDE_DIR "Rcpp.h"
		PATHS ${Rcpp_ROOT_DIR} ${LIBR_PACKAGE_DIRS} /usr/lib/R/site-library/Rcpp /usr/lib64/R/library/Rcpp
		PATH_SUFFIXES include Rcpp/include
		NO_DEFAULT_PATH)
find_package_handle_standard_args(Rcpp REQUIRED_VARS Rcpp_INCLUDE_DIR)
if (Rcpp_FOUND)
	set(Rcpp_INCLUDE_DIRS ${Rcpp_INCLUDE_DIR})
	mark_as_advanced(Rcpp_INCLUDE_DIR)
endif ()

add_library(rcpp INTERFACE)
target_include_directories(rcpp SYSTEM INTERFACE ${R_INCLUDE_DIRS} ${Rcpp_INCLUDE_DIRS})
target_link_libraries(rcpp INTERFACE -Wl,--start-group ${R_LIBRARIES} -Wl,--end-group)
