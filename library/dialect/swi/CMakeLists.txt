set (SDIALECTS_PL
  INDEX.pl   
  listing.pl   
  syspred_options.pl   
  )


install(FILES ${SDIALECTS_PL}
  DESTINATION ${libpl}/dialect/swi
  )
  