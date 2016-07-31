	
set( LIBRANDOM_SOURCES yap_random.c)
add_external (yap_random ${LIBRANDOM_SOURCES})
if (ANDROID)
else()

target_link_libraries(yap_random libYap)

set_target_properties (yap_random PROPERTIES PREFIX "")
endif()

MY_install(TARGETS  yap_random
  LIBRARY DESTINATION ${dlls}
  ARCHIVE DESTINATION ${dlls} )
