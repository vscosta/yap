#CHECK: DOXYGEN
macro_optional_find_package (Doxygen ON)
macro_log_feature (DOXYGEN_FOUND "Doxygen" 
  "Use DOXYGEN Documentation System " 
  "http://www.doxygen.org" FALSE)
if (DOXYGEN_FOUND)
  #
  #    DOXYGEN_EXECUTABLE     = The path to the doxygen command.
  #    DOXYGEN_FOUND          = Was Doxygen found or not?
  #    DOXYGEN_VERSION        = The version reported by doxygen --version
  #
  #
  #
  # ::
  #
  #    DOXYGEN_DOT_EXECUTABLE = The path to the dot program used by doxygen.
  #    DOXYGEN_DOT_FOUND      = Was Dot found or not?
endif (DOXYGEN_FOUND)
