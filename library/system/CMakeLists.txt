
add_library (sys SHARED sys.c)

target_link_libraries(sys libYap)

set_target_properties (sys PROPERTIES  PREFIX "")

install(TARGETS  sys
  LIBRARY DESTINATION ${dlls} )

