#!/usr/local/bin/yap -L -- *
% called with 
% mpirun -np 2 bash gowait.
% prints
% ------
% main
% main
% after_init
% after_init
% [0,2]
% [1,2]
% irecv
% wait_end
% after_send
% c(535755152,)

:- use_module(library(lam_mpi)).
:- use_module(library(system)).
main:-
	write(main),nl,
	mpi_init,
	write(after_init),nl,
	mpi_comm_size(S),
	mpi_comm_rank(R),
	write([R,S]),nl,
	(R == 0->
	 sleep(2),
	 write(wait_end),nl,
	 mpi_send(ciao,1,201),
	 write(after_send),nl
	;
	 mpi_irecv(0,_201,H),
	 write(irecv),nl,
	 test(H)
	),
	mpi_finalize.

test(H):-
	(mpi_wait_recv(H,S,Data)->
	 write(c(S,Data)),nl
	;
	 write(no),nl,
	 test(H)
	).

:-main.
