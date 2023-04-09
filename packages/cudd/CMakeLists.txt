
list (APPEND CMAKE_PREFIX_PATH ${CUDD_ROOT})

if (CUDD_FOUND)
  #  CUDD_FOUND       - system has Cudd
  #  CUDD_LIBRARIES   - Link these to use Cudd
  #  CUDD_INCLUDE_DIRS - Include directory for using Cudd

   set ( BDD_SOURCES
    cudd.c
    )
  
  add_library(YAPCudd SHARED ${BDD_SOURCES} )

  include_directories(
    .
    ${CUDD_INCLUDE_DIRS}
    ${CMAKE_CURRENT_BINARY_DIR}
    )



    TARGET_LINK_LIBRARIES(YAPCudd
      ${CUDD_LIBRARIES}
      m
      libYap
      )
                     
  install(TARGETS  YAPCudd
    LIBRARY DESTINATION ${YAP_INSTALL_DLLDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_DLLDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_DLLDIR}

    )

  add_subDIRECTORY(simplecudd)
  add_subDIRECTORY(simplecudd_lfi)

   configure_file(${CMAKE_CURRENT_LIST_DIR}/include/cudd/cudd_config.h.in
    cudd_config.h)

  install(FILES  ${CUDD_LIBRARIES}
			 DESTINATION ${YAP_INSTALL_DLLDIR}
			     )

#  INSTALL(FILES bdd.yap DESTINATION ${YAP_INSTALL_DATADIR})

endif()
         
