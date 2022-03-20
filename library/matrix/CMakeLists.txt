
add_library(matrix matrix.c)

target_link_libraries(matrix libYap)

set_target_properties (matrix PROPERTIES PREFIX "")

install(TARGETS  matrix
  RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
        LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
  )

