set (LIBRARY_PL_CLP
  	clpfd.pl 
	clp_distinct.pl 
	simplex.pl
	)

add_to_group(   LIBRARY_PL_CLP pl_library  )

install(FILES ${LIBRARY_PL_CLP}
  DESTINATION ${libpl}
  )




