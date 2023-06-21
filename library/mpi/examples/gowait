
:- use_module(library(mpi)).
:- use_module(library(system)).

:- initialization(main).

main:-
	writeln(user_error,main),
	mpi_init,
	write(after_init),nl,
	mpi_comm_size(S),
	mpi_comm_rank(R),
	writeln(user_error,[R,S]),
	(R == 0->
	 sleep(2),
	 write(wait_end),nl,
	 mpi_send(ciao,1,201),
	 mpi_isend(ciao,1,201,Op),
	 mpi_wait(Op,_),
	 write(after_send),nl
	;
	 mpi_recv(0,_201,H),
	 write(recv),nl,
	 test(H)
	),
	mpi_finalize.

test(H):-
	(mpi_irecv(0,201,1024,Op)->
	mpi_wait_recv(Op,_,Data),
	 write(c(S,Data)),nl
	;
	 write(no),nl,
	 test(H)
	).


