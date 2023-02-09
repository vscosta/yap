
set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

#SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})



# RDF converter

find_package (Raptor2)

IF (RAPTOR2_FOUND)
#  RAPTOR2_FOUND - System has Raptor2
#  RAPTOR2_INCLUDE_DIR - The Raptor2 include directory
#  RAPTOR2_LIBRARIES - The libraries needed to use Raptor2
#  RAPTOR2_RAPPER_EXECUTABLE - Raptor2's "rapper" RDF processing tool

  INCLUDE_DIRECTORIES(
    ${RAPTOR2_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    )



  SET ( RAPTOR2_SOURCES
    raptor_yap.c
    )

  add_library(RAPTOR2  ${RAPTOR2_SOURCES} )

  if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(RAPTOR2
      ${RAPTOR2_LIBRARIES}
      libYap
      )
  else()
    TARGET_LINK_LIBRARIES(RAPTOR2
      ${RAPTOR2_LIBRARIES}
      ${YAP_LIBRARY}
      )
  endif()

  check_include_files( raptor2/raptor2.h HAVE_RAPTOR2_RAPTOR2_H )

  check_include_files( raptor.h HAVE_RAPTOR2_H )

  set_target_properties (RAPTOR2 PROPERTIES PREFIX "")

  install(TARGETS  RAPTOR2
          LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}

    )

  INSTALL(FILES raptor.yap DESTINATION ${YAP_INSTALL_DATADIR})
  INSTALL(FILES rdf.yap DESTINATION ${YAP_INSTALL_DATADIR})

  configure_file (raptor_config.h.cmake   raptor_config.h)

ENDIF (RAPTOR2_FOUND)
