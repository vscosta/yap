set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

set ( TRIES_SOURCES
  core_tries.c
  base_tries.c      
  tries.c
  )

add_library(tries ${TRIES_SOURCES})

target_link_libraries(tries libYap)

set_target_properties (tries PROPERTIES PREFIX "")

install(TARGETS  tries
        LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR})


set ( ITRIES_SOURCES
  core_tries.c
  base_itries.c      
  itries.c
  )

if (ANDROID OR WIN32)
   add_component ( otries ${TRIES_SOURCES} )
endif()
add_library(itries ${ITRIES_SOURCES})

target_link_libraries(itries libYap)

set_target_properties (itries PROPERTIES PREFIX "")

install(TARGETS  itries
        LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR} )

