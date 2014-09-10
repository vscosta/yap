%% demo2.pl -- Stasinos Konstantopoulos
%% konstant@let.rug.nl, Tue Feb 12 2002
%%

:- use_module(library(mpi)).

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
	mpe_create_event(Ev1),
	mpe_create_event(Ev2),
	format( "Ev1 == ~q, Ev2 == ~q~n", [Ev1,Ev2] ),
	mpe_create_state(Ev1,Ev2,state1,red),
	format( "1 AA~n", [] ),
	mpe_log(Ev1,0,event1),
	format( "2 AA~n", [] ),
	mpi_bcast( Num, 0 ),
	format( 'Proc 0: broadcast ~q.~n', [Num] ),
	mpe_log(Ev2,0,event2).
do(Rank, _) :-
	!,
	mpe_create_event(Ev1),
	mpe_create_event(Ev2),
	format( "Ev1 == ~q, Ev2 == ~q~n", [Ev1,Ev2] ),
	mpe_log(Ev1,0,event1),
	mpi_bcast( Num, 0 ),
	format( 'Proc ~q: had ~q broadcast from 0.~n', [Rank, Num] ),
	mpe_log(Ev2,0,event2).


%%
%% This is the entry point
%%

start(Msg) :-
	mpi_open( Rank, NumProc, ProcName ),
	format( 'Rank: ~q NumProc: ~q, ProcName: ~q~n', [Rank,NumProc,ProcName] ),
	(mpe_open -> true ;
	    assert_static(mpe_create_event(dummy)),
	    assert_static(mpe_create_state(_,_,_,_)),
	    assert_static(mpe_log(_,_,_)),
	    assert_static(mpe_close(_))
	),
	do(Rank, Msg),
	format( 'Rank ~q finished!~n', [Rank] ),
	mpe_close( demo2 ).

