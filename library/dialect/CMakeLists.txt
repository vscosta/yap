set (DIALECTS_PL
  bprolog.yap   
  commons.yap   
  hprolog.yap   
  swi.yap
  )

add_subdirectory(swi)

install(FILES ${DIALECTS_PL}
  DESTINATION ${libpl}/dialect
  )
