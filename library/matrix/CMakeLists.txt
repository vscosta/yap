
add_library(matrix matrix.c)

target_link_libraries(matrix libYap)

set_target_properties (matrix PROPERTIES PREFIX "")

install(TARGETS  matrix
  RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
  )

