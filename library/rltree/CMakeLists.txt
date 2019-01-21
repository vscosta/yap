
set ( RLTREE_SOURCES
  range_list.c  
  range_list.h
  )

add_library(yap_rl  yap_rl.c  ${RLTREE_SOURCES})

target_link_libraries(yap_rl libYap)

set_target_properties (yap_rl PROPERTIES PREFIX "")

install(TARGETS  yap_rl
  LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )

