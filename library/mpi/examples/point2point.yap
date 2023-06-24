%% wait_end
%% after_send
%% c(535755152,)
%%

:- use_module(library(mpi)).
:- use_module(library(system)).

:- mpi_init.

main :-
    mpi_comm_size(Size),
    mpi_comm_rank(R1),
    copy_sync(R1,10),
    copy_sync(R1,100),
    copy_sync(R1,1000),
    copy_sync(R1,10000),
    copy_sync(R1,100000),
%    copy_sync(R1,10000000),
    copy_async(R1,10),
   mpi_finalize.

copy_sync(I,N):-
I==0,
    length(L,N),
    J is (I+1)  mod 2,
    mpi_send(L,J,1).
copy_sync(I,N):-
I==1,
    mpi_recv(_J,1,L),
length(L,N),
    writeln(sent=N).


copy_async(I,N):-
    length(L,N),
    J is (I+1) mod 2,
    mpi_irecv(J,1,1024,Req),
    mpi_isend(L,J,1,Req2),
	mpi_wait(Req2),
       mpi_wait(Req,LL),
       length(LL,NN),
        writeln(N=NN).


:- initialization main.


