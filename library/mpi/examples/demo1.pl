%% demo1.pl -- Stasinos Konstantopoulos
%% konstant@let.rug.nl, Thu Jan 24 2002
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
%% This spreads the work among the processors
%% and collects the results.
%%

do(0, Num) :-
	!,
	Half is Num // 2,
	format( 'Proc 0: Calculating ~q..~q~n', [1, Half] ),
	calc( 1, Half, 0, R1 ),
	format( 'Proc 0: Done! (~q)~n', [R1] ),
	mpi_receive( R2, Source, Tag ), % Receive any Source, any Tag
	format( 'Proc ~q said: ~q (Tag: ~q)~n', [Source,R2,Tag] ),
	% mpi_receive( R2, 1, 1 ), % Be more particular
	Res is R1 + R2,
	format( 'Sum(1..~q) = ~q~n', [Num,Res] ).
do(1, Num) :-
	!,
	Half is Num // 2,
	format( 'Proc 1: Calculating ~q..~q~n', [Half,Num] ),
	calc( Half, Num, 0, Res ),
	format( 'Proc 1: Done! (~q)~n', [Res] ),
	mpi_send( Res, 0, 1 ).


%%
%% This is the entry point
%%

start(Num) :-
	mpi_open( Rank, NumProc, ProcName ),
	format( 'Rank: ~q NumProc: ~q, ProcName: ~q~n', [Rank,NumProc,ProcName] ),
	do( Rank, Num ),
	format( 'Rank ~q finished!~n', [Rank] ).
