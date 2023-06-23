%% wait_end
%% after_send
%% c(535755152,)
%%

:- use_module(library(mpi)).
:- use_module(library(system)).

main :-
   mpi_init,
   mpi_comm_size(2),
   mpi_comm_rank(R1),
    main(R1),
    main2(R1),
    main3(R1),
    mpi_barrier.

main(0):-
	!,
    mpi_send([88],1,1),
    writeln(sent0).
main(1):-	
    mpi_recv(_,_S  ,Data),
    writeln((Data)).


main2(0):-
	!,

    length(L,256),
    mpi_send(L,1,1),
    writeln(sent).
main2(1):-
    mpi_recv(_,_S  ,Data),
    length(Data,SzData),
    writeln(SzData).	 



main3(0):-
	!,

    length(L,25600),
    mpi_send(L,1,1),
    writeln(sent).
main3(1):-
    mpi_recv(_,S  ,Data),
    length(Data,SzData),
    writeln(SzData).	 


:- initialization main.


