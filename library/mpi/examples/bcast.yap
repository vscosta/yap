
:- use_module(library(mpi)).

:- initialization(main).

main :-
       mpi_init,
mpi_comm_size(N),
       mpi_comm_rank(Rank),
    do_comm(N,Rank),
    mpi_barrier,
    mpi_finalize.


do_comm(_N,Rank) :-
	   NI is Rank*10,
	   (Rank==0
	   ->
	   gen_list(NI,List)
	   ;
	   true
	   ),
	   mpi_bcast(0, List),
	   format('Rank=~d Msg=~w~n',[Rank,List]),
	   fail.
do_comm(_,_).
	
gen_list(0,[]) :- !.
gen_list(I,[I|List]) :-
          I1 is I-1,
          gen_list(I1,List).

