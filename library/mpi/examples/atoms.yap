%%		#!/usr/local/bin/yap -L -- *

%% called with 
%% `mpirun -np 2 bash gowait`
%% prints
%% ```
%% main
%% main
%% after_init
%% after_init
%% [0,2]
%% [1,2]
%% irecv
%% wait_end
%% after_send
%% c(535755152,)
%% ```

:- use_module(library(mpi)).
:- use_module(library(system)).

main(0):-
	!,
	format('synch send~n',[]),
	mpi_send([88],1,1),
writeln(sent).
main(1):-
writeln(k),ctrace(mpi_recv(_,S  ,Data)),
	 writeln(recv(S,Data)).
	 


:- initialization
   mpi_init,
   mpi_comm_size(2),
   mpi_comm_rank(R1),
   main(R1).	  
