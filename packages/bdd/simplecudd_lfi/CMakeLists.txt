set (SRC
  adterror.c
  allocate.c
  general.c
  iqueue.c
  pqueue.c
  problogbdd_lfi.c
  problogmath.c
  simplecudd.c
  )

 
add_executable (Problogbdd-Lfi
  ${SRC})

set_target_properties (Problogbdd-Lfi PROPERTIES
  OUTPUT_NAME simplecudd_lfi
  )

target_link_libraries(Problogbdd-Lfi
  ${CUDD_LIBRARIES}
  m
 )

  INCLUDE_DIRECTORIES( 
    ${CUDD_INCLUDE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/..
    ../include
    )
 
  install(TARGETS Problogbdd-Lfi
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    )

