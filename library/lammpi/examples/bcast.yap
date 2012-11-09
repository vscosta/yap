
:- use_module(library(lam_mpi)).

:- initialization(main).

main :-
       mpi_init,
       writeln(ok1),
       mpi_comm_size(N),
       writeln(ok2:N),
       mpi_comm_rank(Rank),
       do_comm(Rank),
       mpi_finalize.

do_comm(Rank) :-
	   between(1,10,I),
	   NI is I*10,
	   gen_list(NI,List),
	   mpi_bcast2(0, List),
	   mpi_barrier,
	   format('Rank=~d Msg=~w~n',[Rank,List]),
	   fail.
do_comm(_).

gen_list(0,[]) :- !.
gen_list(I,[I|List]) :-
          I1 is I-1,
          gen_list(I1,List).

