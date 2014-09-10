%% demo1.pl -- Stasinos Konstantopoulos
%% konstant@let.rug.nl, Thu Jan 24 2002
%%

:- use_module(library(mpi)).

% make the `floor' operator return integer values
:- set_prolog_flag(language, iso).


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

do(0, NumProc):-
	!,
	% broadcast task
	mpi_bcast(10, 0),
	set_value(n, NumProc),
	set_value(acc, 0),
	repeat,
	  mpi_receive(T, Source, Tag),
	  format( '0: Proc ~q said: ~q (Tag: ~q)~n', [Source,T,Tag] ),
	  % accumulate results
	  get_value(acc, Acc),
	  NewAcc is Acc + T,
	  set_value(acc, NewAcc),
	  % processors still left
	  get_value(n, Counter),
	  NewCounter is Counter - 1,
	  set_value(n, NewCounter),
	  NewCounter == 1,
	!,
	format( '0: Result: ~q.~n', [NewAcc]).


do(Rank, NumProc):-
	!,
	% catch the task broadcast
	mpi_bcast(Job, 0),
	From is floor(Job * (Rank - 1) / (NumProc - 1)),
	To is floor(Job * Rank / (NumProc - 1)) - 1,
	format( '~q: I am calculating ~q..~q.~n', [Rank,From,To] ),
	% do the job
	calc( From, To, 0, Result ),
	format( '~q: sending ~q to 0. (Tag: 1)~n', [Rank,Result] ),
	% send back the results
	mpi_send(Result, 0, 1).


%%
%% This is the entry point
%%

start:-
	mpi_open(Rank, NumProc, ProcName),
	format('Rank: ~q NumProc: ~q, ProcName: ~q~n', [Rank,NumProc,ProcName]),
	do(Rank, NumProc),
	format( 'Rank ~q finished!~n', [Rank] ),
	mpi_close.
