CMAKE_MINIMUM_REQUIRED ( VERSION 2.8 )

PROJECT ( YAP_RAPTOR C )

SET ( YAP_RAPTOR_VERSION 0.1)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})


if (NOT YAP_FOUND)
  FIND_PACKAGE(YAP REQUIRED)
ENDIF (NOT YAP_FOUND)
IF (NOT YAP_FOUND)
  MESSAGE (SEND_ERROR "YAP was not found!")
ENDIF (NOT YAP_FOUND)

macro_optional_find_package (LibXml2 ON)
IF (LIBXML2_FOUND)
  #   LIBXML2_FOUND - System has LibXml2
#   LIBXML2_INCLUDE_DIR - The LibXml2 include directory
#   LIBXML2_LIBRARIES - The libraries needed to use LibXml2
#   LIBXML2_DEFINITIONS - Compiler switches required for using LibXml2
#   LIBXML2_XMLLINT_EXECUTABLE - The XML checking tool xmllint coming with LibXml2
#   LIBXML2_VERSION_STRING - the version of LibXml2 found (since CMake 2.8.8)


INCLUDE_DIRECTORIES(
  ../../H
  ${LIBXML2_INCLUDE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  )

  SET ( LIBXML2_SOURCES
    xml2_yap.c
    )
if (WIN32)
  ADD_LIBRARY(libxml2 OBJECT ${LIBXML2_SOURCES} )
  set(YAP_SYSTEM_OPTIONS "libxml2 " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
  set( CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBXML2_LIBRARIES} )
  
else (WIN32)
  add_library(libxml2  ${LIBXML2_SOURCES} )

  if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(libxml2
      ${LIBXML2_LIBRARIES}
      libYap
      )
  else()
    TARGET_LINK_LIBRARIES(libxml2
      ${LIBXML2_LIBRARIES}
      ${YAP_LIBRARY}
      )
  endif()
  
   install(TARGETS  libxml2
           LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
     ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
   )
endif(WIN32)

  check_include_files( libxml2/libxml2.h HAVE_LIBXML2_LIBXML2_H )

  check_include_files( libxml2.h HAVE_LIBXML2_H )

  set_target_properties (libxml2 PROPERTIES PREFIX "")

 
  INSTALL(FILES xml2.yap DESTINATION ${YAP_INSTALL_DATADIR})

ENDIF (LIBXML2_FOUND)

macro_optional_find_package (RAPTOR ON)

IF (RAPTOR_FOUND)
  #  RAPTOR_FOUND       - system has Raptor
  #  RAPTOR_LIBRARIES   - Link these to use Raptor
  #  RAPTOR_INCLUDE_DIR - Include directory for using Raptor
  #  RAPTOR_DEFINITIONS - Compiler switches required for using Raptor

  INCLUDE_DIRECTORIES(
    ${RAPTOR_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    )

  SET ( RAPTOR_SOURCES
    raptor_yap.c
    )

  add_library(raptor  ${RAPTOR_SOURCES} )

  if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(raptor
      ${RAPTOR_LIBRARY}
      libYap
      )
  else()
    TARGET_LINK_LIBRARIES(raptor
      ${RAPTOR_LIBRARY}
      ${YAP_LIBRARY}
      )
  endif()

  check_include_files( raptor2/raptor2.h HAVE_RAPTOR2_RAPTOR2_H )

  check_include_files( raptor.h HAVE_RAPTOR_H )

  set_target_properties (raptor PROPERTIES PREFIX "")

  install(TARGETS  raptor
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}

    )

  INSTALL(FILES raptor.yap DESTINATION ${YAP_INSTALL_DATADIR})
  INSTALL(FILES rdf.yap DESTINATION ${YAP_INSTALL_DATADIR})

  configure_file ("raptor_config.h.cmake" "raptor_config.h" )

ENDIF (RAPTOR_FOUND)

