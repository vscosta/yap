# vim: set ts=2 shiftwidth=2 expandtab:
# - Find GMP/MPIR libraries and headers
# This module defines the following variables:
#
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBARIES      - libraries to link with
# GMP_LIBARY_DLL    - library DLL to install. Only available on WIN32.
# GMP_LIBRARIES_DIR - the directory the library we link with is found in.

find_path(GMP_INCLUDE_DIRS NAMES gmp.h
          PATHS "$ENV{PROGRAMFILES}/mpir/include"
          DOC "The gmp include directory"
)

if(WIN32)
  if(CMAKE_BUILD_TYPE STREQUAL "Debug" AND MSVC)
	set(MPIR_LIB "mpird")
  else()
    set(MPIR_LIB "mpir")
  endif()

  find_library(GMP_LIBRARIES NAMES ${MPIR_LIB}
                PATHS "$ENV{PROGRAMFILES}/mpir/lib"
                DOC "The MPIR library"
  )
  find_library(GMP_LIBRARY_DLL NAMES ${MPIR_LIB}
				PATHS "$ENV{PROGRAMFILES}/mpir/bin"
				DOC "The MPIR library DLL"
  )
else(WIN32)
  find_library(GMP_LIBRARIES NAMES gmp
                DOC "The GMP library"
  )
endif(WIN32)

get_filename_component(GMP_LIBRARIES_DIR "${GMP_LIBRARIES}" PATH)

# handle the QUIET and REQUIRED arguments and set GMP_FOUND to TRUE if
# all listed variables are true
include(FindPackageHandleStandardArgs)
if(WIN32)
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_LIBRARY_DLL GMP_INCLUDE_DIRS)
else()
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_INCLUDE_DIRS)
endif()
