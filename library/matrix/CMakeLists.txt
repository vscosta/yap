
add_library (matrix SHARED matrix.c)

target_link_libraries(matrix libYap)

set_target_properties (matrix PROPERTIES PREFIX "")

install(TARGETS  matrix
  LIBRARY DESTINATION ${dlls} )

