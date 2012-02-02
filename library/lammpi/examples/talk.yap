
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
	   gen_list(I,List),
	   mpi_send(List, 1, I),
	   fail.
do_comm(0).
do_comm(1) :-
	   between(1,10,I),
	   mpi_recv(0, I, List),
	   writeln(I:List),
	   fail.
do_comm(1).

gen_list(0,[]) :- !.
gen_list(I,I.List) :-
          I1 is I-1,
          gen_list(I1,List).

