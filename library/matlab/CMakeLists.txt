macro_optional_find_package (Matlab OFF)
macro_log_feature (MATLAB_FOUND "matlab" 
  "MATLAB A Language for Technical Computing" 
  "http://matlab.com")
if (MATLAB_FOUND)
  #   MATLAB_INCLUDE_DIR: include path for mex.h, engine.h
  #   MATLAB_LIBRARIES:   required libraries: libmex, etc
  #   MATLAB_MEX_LIBRARY: path to libmex.lib
  #   MATLAB_MX_LIBRARY:  path to libmx.lib
  #   MATLAB_ENG_LIBRARY: path to libeng.lib

  add_library (matlab SHARED matlab.c)

  set_target_properties (matlab PROPERTIES prefix "")
  
  include_directories (${MATLAB_INCLUDE_DIR})
  target_link_libraries(matlab libYap $(MATLAB_LIBRARIES) )

  install(TARGETS  matlab
    RUNTIME DESTINATION ${dlls}
    ARCHIVE DESTINATION ${dlls} )

endif (MATLAB_FOUND)

