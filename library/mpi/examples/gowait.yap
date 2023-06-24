
:- use_module(library(mpi)).
:- use_module(library(system)).

:- initialization(main).

main:-
	mpi_init,
	mpi_comm_size(S),
	mpi_comm_rank(R),
	(R == 0->
	 sleep(2),
	 mpi_send(ciao,1,201),
	 mpi_isend(ciao,1,201,Op),
	 mpi_wait(Op),
	 writeln(R:sent(ciao))
	;
	 mpi_recv(0,_201,H),
	 writeln(R:S:done_recv),
	 test(H)
	),
	mpi_barrier,
	mpi_finalize.

test(H):-
	mpi_comm_rank(R),
	(mpi_irecv(0,201,1024,Op)->
	mpi_wait(Op,Data),
	 writeln(R:got(Data))
	;
	 writeln(no),
	 test(H)
	).


