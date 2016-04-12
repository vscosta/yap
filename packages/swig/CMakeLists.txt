#CHECK: SWIG
find_host_package (SWIG)
macro_log_feature (SWIG_FOUND "Swig"
  "Use SWIG Documentation System "
"http://www.swig.org" ON)

if (SWIG_FOUND)
  #
  #   SWIG_FOUND - set to true if SWIG is found
  #   SWIG_DIR - the directory where swig is installed
  #   SWIG_EXECUTABLE - the path to the swig executable
  #   SWIG_VERSION   - the version number of the swig executable
  #

  # This is a CMake example for Python and Java

  INCLUDE(${SWIG_USE_FILE})

if (ANDROID)
add_subdirectory(android)
else(ANDROID)
add_subdirectory(python)
add_subdirectory(java)
endif(ANDROID)

endif (SWIG_FOUND)
