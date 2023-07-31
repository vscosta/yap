	
set( LIBRANDOM_SOURCES yap_random.c)
add_library(YAPSimpleRandom ${LIBRANDOM_SOURCES})

target_link_libraries(YAPSimpleRandom libYap)

# set_target_properties (YAPSimpleRandom PROPERTIES PREFIX "")

MY_install(TARGETS  YAPSimpleRandom
  LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )
