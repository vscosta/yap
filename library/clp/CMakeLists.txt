set (LIBRARY_PL_CLP
  	clpfd.pl 
	clp_distinct.pl 
	simplex.pl
	)

add_to_dir(   LIBRARY_PL_CLP ${YAP_INSTALL_DATADIR} )

install(FILES ${LIBRARY_PL_CLP}
  DESTINATION ${YAP_INSTALL_DATADIR}
  )




