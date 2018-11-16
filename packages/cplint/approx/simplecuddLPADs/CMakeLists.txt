set (SRC
  general.c
  general.h
  ProblogBDD.c
  simplecudd.h
  simplecudd.c
  )

  
add_executable (LPADbdd
  ${SRC})

set_target_properties (LPADbdd PROPERTIES
  OUTPUT_NAME LPADBDD
  )

target_link_libraries(LPADbdd
        m
  ${CUDD_LIBRARIES}    
  libYap    
 )

  INCLUDE_DIRECTORIES( 
    ${CUDD_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    )
 
  install(TARGETS LPADbdd
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

