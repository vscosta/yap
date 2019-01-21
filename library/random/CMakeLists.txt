	
set( LIBRANDOM_SOURCES yap_random.c)
add_library(yap_random ${LIBRANDOM_SOURCES})

target_link_libraries(yap_random libYap)

set_target_properties (yap_random PROPERTIES PREFIX "")

MY_install(TARGETS  yap_random
  LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )
