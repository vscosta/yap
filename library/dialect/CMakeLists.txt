set (DIALECTS_PL
  bprolog.yap
  commons.yap
  hprolog.yap
  swi.yap
  )

add_custom_target( dialects
DEPENDS visited
SOURCES ${DIALECTS_PL} )

add_custom_command (OUTPUT visited
COMMAND ${CMAKE_COMMAND} -E touch visited
DEPENDS ${DIALECTS_PL}  ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )

install(FILES ${DIALECTS_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}/dialect
  )
