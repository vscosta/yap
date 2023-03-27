
add_library(YAPMatrix matrix.c)

target_link_libraries(YAPMatrix libYap)

#set_target_properties (YAPMatrix PROPERTIES PREFIX "")

install(TARGETS  YAPMatrix
  RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}
        LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
  )

  install (FILES  ../matrix.yap DESTINATION ${YAP_INSTALL_DATADIR})

  
