#code used to emulate the support routines in the YAP kernel...

set (HELPER_PL
preds.yap
  )

add_custom_target (ytest_helpers SOURCES ${HELPER_PL} ) # WORKING_DIRECTORY ${CMAKE_BINARY_DIR} )


install(FILES ${HELPER_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}/ytest
  )
