macro_optional_find_package (Matlab OFF)

if (MATLAB_FOUND)
  #   MATLAB_INCLUDE_DIR: include path for mex.h, engine.h
  #   MATLAB_LIBRARIES:   required libraries: libmex, etc
  #   MATLAB_MEX_LIBRARY: path to libmex.lib
  #   MATLAB_MX_LIBRARY:  path to libmx.lib
  #   MATLAB_ENG_LIBRARY: path to libeng.lib

  add_li (matlab matlab.c)

  set_target_properties (matlab PROPERTIES prefix "")
  
  include_directories (${MATLAB_INCLUDE_DIR})
  target_link_libraries(matlab libYap $(MATLAB_LIBRARIES) )

  install(TARGETS  matlab
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )

endif (MATLAB_FOUND)

