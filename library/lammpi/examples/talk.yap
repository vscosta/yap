
:- use_module(library(lam_mpi)).

:- initialization(main).

main :-
yap_flag(gc_trace,verbose),
    mpi_init,
       mpi_comm_rank(Rank),
       do_comm(Rank),
       mpi_finalize.

do_comm(0) :-
    get_code(_),
	   between(1,10,I),
	   NI is I*100000,
	   gen_list(NI,List),
	   mpi_send(List, 1, I),
	 %  T =.. [f|List],
	 %  mpi_send(T, 1, I),
	 %  writeln(sent:I),
	   fail.
do_comm(0) :-
	   between(1,10,I),%	   writeln(I:List),

	   NI is 2.3*I,
		   mpi_send(NI, 1, I),
	   fail.
do_comm(1) :-
	   between(1,10,I),
	   mpi_recv(0, I, _List),
	   writeln(recv:I),
	   % mpi_recv(0, I, _T),
	   % writeln(recv:I),
	   fail.
do_comm(1) :-
	   between(1,10,I),
	   mpi_recv(0, I, T),
	   writeln(rec:I),
	   fail.
do_comm(_).

gen_list(0,[]) :- !.
gen_list(I,I.List) :-
          I1 is I-1,
          gen_list(I1,List).

