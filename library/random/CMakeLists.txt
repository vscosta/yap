
add_library (yap_random SHARED yap_random.c)

target_link_libraries(yap_random libYap)

set_target_properties (yap_random PROPERTIES PREFIX "")

install(TARGETS  yap_random
  LIBRARY DESTINATION ${dlls} )

