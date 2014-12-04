macro_optional_find_package (Matlab OFF)
macro_log_feature (MATLAB_FOUND "matlab" 
  "MATLAB Distributed Parallel Programming" 
  "http://matlab.com")
if (MATLAB_FOUND)
  #   MATLAB_INCLUDE_DIR: include path for mex.h, engine.h
  #   MATLAB_LIBRARIES:   required libraries: libmex, etc
  #   MATLAB_MEX_LIBRARY: path to libmex.lib
  #   MATLAB_MX_LIBRARY:  path to libmx.lib
  #   MATLAB_ENG_LIBRARY: path to libeng.lib
endif (MATLAB_FOUND)

