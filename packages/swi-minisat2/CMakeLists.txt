
set (PROGRAMS
	cnf.pl
	minisat.pl
	)

set (EXAMPLE_PROGRAMS 
	examples/adder.pl 
	examples/pearl_examples.pl
	)

install(FILES
  ${PROGRAMS}
  DESTINATION ${libpl}
  )

add_subDIRECTORY (C)

