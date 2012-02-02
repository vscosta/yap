
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
	   NI is I*1000,
	   gen_list(NI,List),
	   mpi_send(List, 1, I),
	   T =.. [f|List],
	   mpi_send(T, 1, I),
	   writeln(sent:I),
	   fail.
do_comm(0) :-
	   between(1,10,I),
	   NI is 2.3*I,
	   mpi_send(NI, 1, I),
	   fail.
do_comm(0).
do_comm(1) :-
	   between(1,10,I),
	   mpi_recv(0, I, List),
%	   writeln(I:List),
	   mpi_recv(0, I, T),
%	   writeln(I:T),
	   writeln(received:I),
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

