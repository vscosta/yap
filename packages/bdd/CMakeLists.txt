CMAKE_MINIMUM_REQUIRED ( VERSION 2.8 )

PROJECT ( YAP_BDD C )

SET ( YAP_BDD_VERSION 0.1)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

SET( CMAKE_FIND_FRAMEWORK LAST)
SET( CMAKE_FIND_APPBUNDLE LAST)

if (NOT YAP_FOUND)
  FIND_PACKAGE(YAP REQUIRED)
ENDIF (NOT YAP_FOUND)
IF (NOT YAP_FOUND)
  MESSAGE (SEND_ERROR "YAP was not found!")
ENDIF (NOT YAP_FOUND)

macro_optional_find_package (CUDD ON)
macro_log_feature (CUDD_FOUND "CUDD"
  "Use CUDD Library"
  "http://vlsi.colorado.edu/~fabio/CUDD/" FALSE)
IF (CUDD_FOUND)
  #  CUDD_FOUND       - system has Cudd
  #  CUDD_LIBRARIES   - Link these to use Cudd
  #  CUDD_INCLUDE_DIR - Include directory for using Cudd

 SET ( BDD_SOURCES
    cudd.c
    )

  ADD_LIBRARY(cudd SHARED ${BDD_SOURCES} )

  INCLUDE_DIRECTORIES(
    ${CUDD_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    )

  check_include_files( "stdio.h;cuddInt.h" HAVE_CUDDINT_H )
   check_include_files( "stdio.h;cudd/cuddInt.h" HAVE_CUDD_CUDDINT_H )
  check_include_files( "stdio.h;dddmpInt.h" HAVE_DDDMPINT_H )
   check_include_files( "stdio.h;dddmp/dddmpInt.h" HAVE_DDDMP_DDDMPINT_H )
  check_include_files( "stdio.h;cuddObj.hh" HAVE_CUDDOBJ_HH )
   check_include_files( "stdio.h;cudd/cuddObj.hh" HAVE_CUDD_CUDDOBJ_HH )

  configure_file ("${PROJECT_SOURCE_DIR}/cudd_config.h.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/cudd_config.h" )


   if(DEFINED YAP_MAJOR_VERSION)
    TARGET_LINK_LIBRARIES(cudd
      ${CUDD_LIBRARIES}
      libYap
      )
  else()
    TARGET_LINK_LIBRARIES(cudd
      ${CUDD_LIBRARIES}
      ${YAP_LIBRARY}
      )
  endif()

  set_target_properties (cudd PROPERTIES PREFIX "")

  add_subdirectory(simplecudd)
  add_subdirectory(simplecudd_lfi)
  set(YAP_SYSTEM_OPTIONS "cudd " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)

  install(TARGETS  cudd
    LIBRARY DESTINATION ${dlls}
    ARCHIVE DESTINATION ${dlls}

    )

  INSTALL(FILES bdd.yap DESTINATION ${libpl})
  INSTALL(FILES ddnnf.yap DESTINATION ${libpl})
  INSTALL(FILES simpbool.yap DESTINATION ${libpl})
  INSTALL(FILES trie_sp.yap DESTINATION ${libpl})

ENDIF (CUDD_FOUND)

SET (CUDD_FOUND_EXPORT ${CUDD_FOUND} PARENT_SCOPE)
SET (CUDD_LIBRARIES_EXPORT ${CUDD_LIBRARIES} PARENT_SCOPE)
SET (CUDD_INCLUDE_DIR_EXPORT ${CUDD_INCLUDE_DIR} ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)
