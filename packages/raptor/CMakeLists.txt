
SET ( YAP_RAPTOR_VERSION 0.1)

#SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})


if (NOT YAP_FOUND)
  FIND_PACKAGE(YAP REQUIRED)
ENDIF (NOT YAP_FOUND)
IF (NOT YAP_FOUND)
  MESSAGE (SEND_ERROR "YAP was not found!")
ENDIF (NOT YAP_FOUND)

INCLUDE_DIRECTORIES(
  ../../H
  ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/CXX
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/H
    ${CMAKE_SOURCE_DIR}/OPTYap
  )

  SET ( XML_SOURCES
    xml2pl.cpp pugixml.cpp
    )


if (WIN32)
  ADD_LIBRARY(xml4yap OBJECT ${XML_SOURCES} )
  
else (WIN32)
  add_library(xml4yap  ${XML_SOURCES} )
  set_property(TARGET xml4yap   PROPERTY CXX_STANDARD 11)
  if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(xml4yap
      YAP++
      )
  else()
    TARGET_LINK_LIBRARIES(xml4yap
      ${YAP_LIBRARY}
      YAP++
      )
  endif()
  
   install(TARGETS  xml4yap
           LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
     ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
   )
endif(WIN32)
 
  INSTALL(FILES xml4yap.yap DESTINATION ${YAP_INSTALL_DATADIR})


  
macro_optional_find_package (Raptor2 ${WITH_Raptor2})

IF (RAPTOR2_FOUND)
  #  RAPTOR2_FOUND       - system has RAPTOR2
  #  RAPTOR2_LIBRARIES   - Link these to use RAPTOR2
  #  RAPTOR2_INCLUDE_DIR - Include directory for using RAPTOR2
  #  RAPTOR2_DEFINITIONS - Compiler switches required for using RAPTOR2

  INCLUDE_DIRECTORIES(
    ${RAPTOR2_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
q    ${CMAKE_SOURCE_DIR}/CXX
    )

  SET ( RAPTOR2_SOURCES
    raptor_yap.c
    )

  add_library(RAPTOR2  ${RAPTOR2_SOURCES} )

  if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(RAPTOR2
      ${RAPTOR2_LIBRARY}
      libYap
      )
  else()
    TARGET_LINK_LIBRARIES(RAPTOR2
      ${RAPTOR2_LIBRARY}
      ${YAP_LIBRARY}
      )
  endif()

  check_include_files( raptor2/raptor2.h HAVE_RAPTOR2_RAPTOR2_H )

  check_include_files( raptor.h HAVE_RAPTOR_H )

  set_target_properties (RAPTOR2 PROPERTIES PREFIX "")

  install(TARGETS  RAPTOR2
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}

    )

  INSTALL(FILES raptor.yap DESTINATION ${YAP_INSTALL_DATADIR})
  INSTALL(FILES rdf.yap DESTINATION ${YAP_INSTALL_DATADIR})

  configure_file (raptor_config.h.cmake   raptor_config.h)

ENDIF (RAPTOR2_FOUND)

