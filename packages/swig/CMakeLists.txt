
#
#   SWIG_FOUND - set to true if SWIG is found
#   SWIG_DIR - the directory where swig is installed
#   SWIG_EXECUTABLE - the path to the swig executable
#   SWIG_VERSION   - the version number of the swig executable
#

# This is a CMake example for Python and Java

set (SOURCES yap.i)

if (ANDROID)
  add_subdirectory(android)
else(ANDROID)
 add_subdirectory(R)
 add_subdirectory(java)
endif(ANDROID)

set_property( DIRECTORY  APPEND PROPERTY COMPILE_DEFINITIONS   SWIGYAP=1)


