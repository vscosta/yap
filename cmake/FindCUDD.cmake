# Try to find CUDD headers and libraries.
#
# Usage of this module as follows:
#
# find_package(CUDD)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
# CUDD_ROOT Set this variable to the root installation of
# libcudd if the module has problems finding the
# proper installation path.
#
# Variables defined by this module:
#
# CUDD_FOUND System has CUDD libraries and headers
# CUDD_LIBRARIES The CUDD library
# CUDD_INCLUDE_DIR The location of CUDD headers

# Get hint from environment variable (if any)
if(NOT CUDD_ROOT AND DEFINED ENV{CUDD_ROOT})
  set(CUDD_ROOT "$ENV{CUDD_ROOT}" CACHE PATH "CUDD base directory location (optional, used for nonstandard installation paths)")
  mark_as_advanced(CUDD_ROOT)
endif()

# Search path for nonstandard locations
if(CUDD_ROOT)
  set(CUDD_INCLUDE_PATH PATHS "${CUDD_ROOT}/include" NO_DEFAULT_PATH)
  set(CUDD_LIBRARY_PATH PATHS "${CUDD_ROOT}/lib" NO_DEFAULT_PATH)
endif()

# Search path for nonstandard locations
if(CUDD_ROOT_DIR)
  set(CUDD_INCLUDE_PATH PATHS "${CUDD_ROOT_DIR}/include" NO_DEFAULT_PATH)
  set(CUDD_LIBRARY_PATH PATHS "${CUDD_ROOT_DIR}/lib" NO_DEFAULT_PATH)
endif()

find_path(CUDD_INCLUDE_DIR NAMES cudd.h cudd/cudd.h HINTS ${CUDD_INCLUDE_PATH})
find_library(CUDD_LIBRARIES NAMES cudd CUDDVC-2.5.0 HINTS ${CUDD_LIBRARY_PATH})


include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(CUDD DEFAULT_MSG CUDD_LIBRARIES CUDD_INCLUDE_DIR)

mark_as_advanced(CUDD_ROOT CUDD_LIBRARIES CUDD_INCLUDE_DIR)

