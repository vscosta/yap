set (SDIALECTS_PL
  INDEX.pl
  listing.pl
  syspred_options.pl
  )

add_SubDirectory( fli )

install(FILES ${SDIALECTS_PL}
  DESTINATION ${libpl}/dialect/swi
  )
