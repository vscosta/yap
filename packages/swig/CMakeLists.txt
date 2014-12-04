#CHECK: SWIG
macro_optional_find_package (SWIG ON)  
macro_log_feature (SWIG_FOUND "Swig" 
  "Use SWIG Documentation System " 
  "http://www.swig.org" FALSE)
if (SWIG_FOUND)
  #
  #   SWIG_FOUND - set to true if SWIG is found
  #   SWIG_DIR - the directory where swig is installed
  #   SWIG_EXECUTABLE - the path to the swig executable
  #   SWIG_VERSION   - the version number of the swig executable
  #


endif (SWIG_FOUND)

