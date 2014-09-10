%% demo1_mpe.pl
%%
%% This file was originally created on 3/7/2003
%% by Stasinos Konstantopoulos <konstant@let.rug.nl>
%% as part of the YAP Prolog distribution.
%%
%% This file is in the Public Domain.

:- use_module(library(mpi)).
:- use_module(library(mpe)).


%% demo1_mpe.pl is the same as demo1.pl, except
%% that MPE is used to log MPI activity.

% make the `floor' operator return integer values
:- set_prolog_flag(language, iso).


%%
%% This the calculation that needs to be performed, in this case
%% the sum of [From..To]
%%

calc(From, From, Acc, Res) :- !,
	Res is Acc + From.
calc(From, To, Acc, Res) :- !,
	Acc1 is Acc + To,
	To1 is To - 1,
	calc(From, To1, Acc1, Res).

%%
%% The master node sends teh task definition to
%% the workers and then collects the results
%%

do(0, NumProc):-
	!,

	% processing state
	mpe_create_event(Ev1),
	mpe_create_event(Ev2),
	mpe_create_state(Ev1,Ev2,processing,green),

	% bcasting state
	mpe_create_event(Ev3),
	mpe_create_event(Ev4),
	mpe_create_state(Ev3,Ev4,bcasting,red),

	% sending/recving state
	mpe_create_event(Ev5),
	mpe_create_event(Ev6),
	mpe_create_state(Ev5,Ev6,'sending/receiving',brown),

	% pretend that the other nodes do not have
	% access to the task definition.
	% retrieve it and broadcast it.
	get_value(from, From),
	get_value(to, To),
	mpe_log(Ev3,0,event3),
	mpi_bcast(msg(From,To), 0),
	mpe_log(Ev4,0,event4),

	% loop to collect and accumulate partial results
	% from the workers
	set_value(n, NumProc),
	set_value(acc, 0),
	repeat,
	  mpe_log(Ev5,0,event5),
	  mpi_receive(T, Source, Tag),
	  mpe_log(Ev6,0,event6),
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
	format('0: Sum(~q..~q) = ~q.~n', [From,To,NewAcc]).


%%
%% The workers hear from the master what needs to
%% be done, do the work and then send the results back.
%%

do(Rank, NumProc):-
	!,

	% processing state
	mpe_create_event(Ev1),
	mpe_create_event(Ev2),
	mpe_create_state(Ev1,Ev2,processing,green),

	% bcasting state
	mpe_create_event(Ev3),
	mpe_create_event(Ev4),
	mpe_create_state(Ev3,Ev4,bcasting,red),

	% sending/recving state
	mpe_create_event(Ev5),
	mpe_create_event(Ev6),
	mpe_create_state(Ev5,Ev6,'sending/receiving',brown),

	% catch the task broadcast
	mpe_log(Ev3,0,event3),
	mpi_bcast(Msg, 0),
	mpe_log(Ev4,0,event4),
	Msg = msg(From,To),
	format( '~q: All are calculating ~q..~q.~n', [Rank,From,To] ),
	MyFrom is floor(To * (Rank - 1) / (NumProc - 1)) + From,
	MyTo is floor(To * Rank / (NumProc - 1)) + From - 1,
	format( '~q: I am calculating ~q..~q.~n', [Rank,MyFrom,MyTo] ),
	% do the job
	mpe_log(Ev1,0,event1),
	calc( MyFrom, MyTo, 0, Result ),
	mpe_log(Ev2,0,event2),
	format( '~q: sending ~q to 0. (Tag: 1)~n', [Rank,Result] ),
	% send back the results
	mpe_log(Ev5,0,event5),
	mpi_send(Result, 0, 1),
	mpe_log(Ev6,0,event6).


%%
%% This is the entry point
%%

start(From, To):-
	% store the task definition
	set_value(from, From),
	set_value(to, To),

	mpi_open(Rank, NumProc, ProcName),
	format('Rank: ~q NumProc: ~q, ProcName: ~q~n', [Rank,NumProc,ProcName]),
	mpe_open,
	do(Rank, NumProc),
	format( 'Rank ~q finished!~n', [Rank] ),
	mpe_close( demo1_mpe ),
	mpi_close.
