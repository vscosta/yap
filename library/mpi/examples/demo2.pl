%% demo2.pl -- Stasinos Konstantopoulos
%% konstant@let.rug.nl, Tue Feb 12 2002
%%

%%
%% This the calculation that needs to be performed, in this case
%% the sum of [From..To]
%%

calc( From, From, Acc, Res ) :- !,
	Res is Acc + From.
calc( From, To, Acc, Res ) :- !,
	Acc1 is Acc + To,
	To1 is To - 1,
	calc( From, To1, Acc1, Res ).


%%
%% We'll pretend the preprocessing was more complicated, and have the
%% root broadcast how many numbers each processor should do.
%% Each processor must then figure out which ones to do and call calc/4.
%%

do(0, Num) :-
	!,
	mpi_bcast( Num, 0 ),
	format( 'Proc 0: broadcast ~q.~n', [Num] ).
do(Rank, _) :-
	!,
	mpi_bcast( Num, 0 ),
	format( 'Proc ~q: had ~q broadcast from 0.~n', [Rank, Num] ).


%%
%% This is the entry point
%%

start(Num) :-
	mpi_open( Rank, NumProc, ProcName ),
	format( 'Rank: ~q NumProc: ~q, ProcName: ~q~n', [Rank,NumProc,ProcName] ),
	do( Rank, Num ),
	format( 'Rank ~q finished!~n', [Rank] ).
