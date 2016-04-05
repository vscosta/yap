#
# - Find GMP/MPIR libraries and headers
# This module defines the following variables:
#
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBRARIES      - libraries to link with
# GMP_LIBRARY_DLL    - library DLL to install. Only available on WIN32.
# GMP_LIBRARIES_DIR - the directory the library we link with is found in.


if(MVC)
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	set(GMP_NAME "mpird")
  else()
    set(GMP_NAME "mpir")
  endif()
else()
  set(GMP_NAME gmp)
endif()

find_path(GMP_INCLUDE_DIRS
        NAMES ${GMP_NAME}.h
        PATHS  $ENV{GMP_ROOT}/include
         $ENV{PROGRAMFILES}/mpir/include
)

  find_library(GMP_LIBRARIES NAMES ${GMP_NAME}
                PATHS  $ENV{GMP_ROOT} $ENV{PROGRAMFILES}/mpir/lib $ENV{GMP_ROOT}/lib ${CMAKE_INSTALL_PREFIX}/lib
  )

  find_library(GMP_LIBRARY_DLL NAMES ${GMP_NAME}
				PATHS "$ENV{PROGRAMFILES}/mpir/bin"
				DOC "The MPIR library DLL"
  )

get_filename_component(GMP_LIBRARIES_DIR "${GMP_LIBRARIES}" PATH CACHE)

# handle the QUIET and REQUIRED arguments and set GMP_FOUND to TRUE if
# all listed variables are true
include(FindPackageHandleStandardArgs)
if(WIN32)
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_LIBRARY_DLL GMP_INCLUDE_DIRS)
else()
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_INCLUDE_DIRS)
endif()

mark_as_advanced(GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_INCLUDE_DIRS)
