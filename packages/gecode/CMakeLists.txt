#TODO: change macro_log_feature to check both WITH_{$name} and ${name}_FOUND
#TODO: improve FindGecode.cmake to be similar to other Find*.cmake
#CHECK: GECODE
macro_optional_find_package (Gecode ON)
macro_log_feature (GECODE_FOUND "libgecode" 
  "Use Generic Constraint Development Environment (GECODE) library" 
  "http://www.gecode.org" FALSE)
if (GECODE_FOUND)
  #DEFINES:
  #   GECODE_INCLUDE_DIR
  #   GECODE_VERSION
  #   GECODE_LIBRARIES

  #set (EXTRALIBS gecode)

  #include_directories (${GECODE_INCLUDE_DIR})
  #set (EXTRALIBS ${EXTRALIBS} ${GECODE_LIBRARIES})
endif (GECODE_FOUND)
