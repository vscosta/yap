set (LIBRARY_PL_VLP
  	clpfd.pl
	clp_distinct.pl
	simplex.pl
	)

install(FILES ${LIBRARY_PL_CLP}
  DESTINATION ${libpl}
  )

  add_to_group( pl_library LIBRARY_PL_CLP )
