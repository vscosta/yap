set (SRC
  general.c
  problogbdd.c
  problogmath.c
  simplecudd.c
  )

  
add_executable (Problogbdd
  ${SRC})

set_target_properties (Problogbdd PROPERTIES
  OUTPUT_NAME simplecudd
  )

target_link_libraries(Problogbdd
  ${CUDD_LIBRARIES}
        m
  libYap    
 )

  INCLUDE_DIRECTORIES( 
    ${CUDD_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    )
 
  install(TARGETS Problogbdd
    RUNTIME DESTINATION ${bindir}
    LIBRARY DESTINATION ${dlls}
    )

