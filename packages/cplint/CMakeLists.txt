
# Be sure to also update these in Makefile!
set(SO_MAJOR 1)
set(SO_MINOR 0)
set(SO_PATCH 0)

IF (CUDD_FOUND)

  INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CUDD_INCLUDE_DIR}
    )

  set( CPLINT_SOURCES
    cplint.h
    cplint_yap.c
    cplint_Prob.c )

  set( BDDEM_SOURCES
    slipcase/bddem.c
    cplint.h
    )

  set (CPLINT_PROGRAMS
    lpadvel.pl
    lpadclpbn.pl
    lpadsld.pl
    lpad.pl
    cpl.pl
    approx/bestfirst.pl
    approx/bestk.pl
    approx/deepdyn.pl
    approx/deepit.pl
    approx/exact.pl
    approx/exact_mem.pl
    approx/montecarlo.pl
    approx/params.pl
    approx/parsing.pl
    approx/utility.pl
    approx/tptree_lpad.pl
    )

  set (CPLINT_SEMANTICS_PROGRAMS
    semlpadsld.pl
    semlpad.pl
    semcpl.pl
    slg.pl
    )

  set (CPLINT_LEARNING_PROGRAMS
    em/em.pl
    em/inference.pl
    lemur/dv_lemur.pl
    lemur/inference_lemur.pl
    lemur/lemur.pl
    lemur/revise_lemur.pl
    lemur/slipcover_lemur.pl
    rib/inference_ib.pl
    rib/rib.pl
    slipcase/inference_sl.pl
    slipcase/revise.pl
    slipcase/slipcase.pl
    slipcover/revise_sl.pl
    slipcover/slipcover.pl
    )

  set (CPLINT_TEST_PROGRAMS
    testcpl.pl
    testlpad.pl
    testlpadclpbn.pl
    testlpadsld_gbfalse.pl
    testlpadsld_gbtrue.pl
    testlpadsldit.pl
    testlpadslditc.pl
    testlpadslditr.pl
    testlpadvel.pl
    testsemcpl.pl
    testsemlpad.pl
    testsemlpadsld.pl
    )

  set (CPLINT_EXAMPLE_PROGRAMS
    examples/alarm.cpl
    examples/coin.cpl
    examples/coin.uni
    examples/coin2.cpl
    examples/coin2.uni
    examples/dice.cpl
    examples/dice.uni
    examples/ex.cpl
    examples/ex.uni
    examples/exapprox.cpl
    examples/exapprox.uni
    examples/exist.cpl
    examples/exist.uni
    examples/exist1.cpl
    examples/exist1.uni
    examples/exrange.cpl
    examples/exrange.uni
    examples/female.cpl
    examples/hiv.cpl
    examples/hiv.uni
    examples/invalid.cpl
    examples/invalid.uni
    examples/light.cpl
    examples/light.uni
    examples/mendel.cpl
    examples/mendel.uni
    examples/mendels.cpl
    examples/mendels.uni
    examples/paper_ref.cpl
    examples/paper_ref_not.cpl
    examples/paper_ref_simple.cpl
    examples/school.cpl
    examples/school_simple.cpl
    examples/school_simple.uni
    examples/student.cpl
    examples/student.uni
    examples/threesideddice.cpl
    examples/threesideddice.uni
    examples/throws.cpl
    examples/throws.uni
    examples/trigger.cpl
    examples/trigger.uni
    examples/twosideddice.cpl
    examples/win.cpl
    examples/win.uni
    )

  INCLUDE_DIRECTORIES(
    ${CUDD_INCLUDE_DIR_EXPORTS}
    ${CMAKE_CURRENT_BINARY_DIR}/../bdd
    )

  add_library(bddem
    ${BDDEM_SOURCES}
    )

  TARGET_LINK_LIBRARIES(bddem
    ${CUDD_LIBRARIES}
    libYap
          m
    )

  set_target_properties (bddem  PROPERTIES
    PREFIX ""
    POSITION_INDEPENDENT_CODE ON
    )

  install(TARGETS  bddem
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
          RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
  )


  add_library(cplint
    ${CPLINT_SOURCES}
   )

 set_target_properties (cplint  PROPERTIES
   PREFIX ""
   POSITION_INDEPENDENT_CODE ON
   )

 TARGET_LINK_LIBRARIES(cplint
      ${CUDD_LIBRARIES}
      libYap
      )

    add_subDIRECTORY ( approx/simplecuddLPADs )

  install(TARGETS  cplint
          LIBRARY DESTINATION ${YAP_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${YAP_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${YAP_INSTALL_LIBDIR}
    )

  INSTALL(FILES ${CPLINT_PROGRAMS} DESTINATION ${YAP_INSTALL_DATADIR})
  INSTALL(FILES ${CPLINT_LEARNING_PROGRAMS} DESTINATION ${YAP_INSTALL_DATADIR}/cplint)
  INSTALL(FILES ${CPLINT_EXAMPLE_PROGRAMS} DESTINATION ${YAP_INSTALL_DATADIR}/cplint/examples)
  INSTALL(FILES ${CPLINT_TEST_PROGRAMS} DESTINATION ${YAP_INSTALL_DATADIR}/cplint/examples)
  INSTALL(FILES ${CPLINT_SEMANTICS_PROGRAMS} DESTINATION ${YAP_INSTALL_DATADIR})

ENDIF()
