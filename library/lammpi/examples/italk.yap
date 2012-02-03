:- use_module(library(lam_mpi)).

:- initialization(main).

main :-
       mpi_init,
       mpi_comm_size(2),
       mpi_comm_rank(Rank),
       do_comm(Rank),
       mpi_finalize.

do_comm(0) :-
	   between(1,10,I),
	   NI is I*10,
	   gen_list(NI,List),
	   mpi_isend(List, 1, I, Handle),
	   T =.. [f|List],
	   mpi_isend(T, 1, I, Handle2),
	   mpi_wait(Handle2, _),
	   mpi_wait(Handle, _),
	   fail.
do_comm(0) :-
	   between(1,10,I),
	   NI is 2.3*I,
	   mpi_send(NI, 1, I),
	   fail.
do_comm(0).
do_comm(1) :-
	   between(1,10,I),
	   mpi_irecv(0, I, Handle),
	   mpi_irecv(0, I, Handle1),
	   mpi_wait_recv(Handle1, _, _T),
	   mpi_wait_recv(Handle, _, _List),
	   writeln(I:_T),
	   writeln(I:_List),
	   fail.
do_comm(1) :-
	   between(1,10,I),
	   mpi_recv(0, I, T),
	   writeln(I:T),
	   fail.
do_comm(1).

gen_list(0,[]) :- !.
gen_list(I,I.List) :-
          I1 is I-1,
          gen_list(I1,List).

