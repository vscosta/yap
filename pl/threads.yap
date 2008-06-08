/*************************************************************************
*                                                                        *
*  YAP Prolog                                                            *
*                                                                        *
*  Yap Prolog was developed at NCCUP - Universidade do Porto             *
*                                                                        *
*  Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997      *
*                                                                        *
**************************************************************************
*                                                                        *
* File:		threads.yap                                                  *
* Last rev:	8/2/88                                                       *
* mods:                                                                  *
* comments:	support threads                                              *
*                                                                        *
*************************************************************************/

:- meta_predicate
	thread_create(:,-,+),
	thread_create(:,-),
	thread_create(:),
	thread_at_exit(:),
	thread_signal(+,:),
	with_mutex(+,:).

:- initialization('$init_thread0').

'$init_thread0' :-
	no_threads, !,
	recorda('$thread_alias', [0|main], _).
'$init_thread0' :-
	'$record_thread_info'(0, main, [0, 0, 0], false, true, '$init_thread0'),
	recorda('$thread_defaults', [0, 0, 0, false, true], _),
	'$new_mutex'(QId),
	assert('$global_queue_mutex'(QId)),
	'$create_thread_mq'(0),
	'$new_mutex'(Id),
	assert('$with_mutex_mutex'(Id)).

'$top_thread_goal'(G, Detached) :-
	'$thread_self'(Id),
	(Detached == true -> '$detach_thread'(Id) ; true),
	'$current_module'(Module),
	% always finish with a throw to make sure we clean stacks.
	'$system_catch'((G -> throw('$thread_finished'(true)) ; throw('$thread_finished'(false))),Module,Exception,'$close_thread'(Exception,Detached)).

'$close_thread'(Status, Detached) :-
	'$thread_zombie_self'(Id0), !,
	'$close_thread'(Status, Detached, Id0).
'$close_thread'(Status, Detached) :- !,
	% zombie_self failed as it the thread was messages pending
	'$close_thread'(Status, Detached).


'$close_thread'('$thread_finished'(Status), Detached, Id0) :- !,
	recorda('$thread_exit_status', [Id0|Status], _),
	'$run_at_thread_exit'(Id0),
	(	Detached == true ->
		'$erase_thread_info'(Id0)
	;	true
	).
%	format(user_error,'closing thread ~w~n',[v([Id0|Status])]).	
'$close_thread'(Exception, Detached, Id0) :-
	 (	recorded('$thread_exit_status', [Id0|_], R), erase(R), fail
	 ;	recorda('$thread_exit_status', [Id0|exception(Exception)], _)
	 ),
	'$run_at_thread_exit'(Id0),
	(	Detached == true ->
		'$erase_thread_info'(Id0)
	;	true
	).

thread_create(Goal) :-
	G0 = thread_create(Goal),
	'$check_callable'(Goal, G0),
	'$thread_options'([detached(true)], [], Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	'$record_thread_info'(Id, [Stack, Trail, System], Detached, AtExit),
	'$create_thread_mq'(Id),
	(
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id)
	->
	 true
	;
	 recorda('$thread_exit_status', [Id|exception(resource_error(memory))],_)
	).

thread_create(Goal, Id) :-
	G0 = thread_create(Goal, Id),
	'$check_callable'(Goal, G0),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'([], [], Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	'$record_thread_info'(Id, [Stack, Trail, System], Detached, AtExit),
	'$create_thread_mq'(Id),
	(
	 '$create_thread'(Goal, Stack, Trail, System, Detached, Id)
	->
	 true
	;
	 recorda('$thread_exit_status', [Id|exception(resource_error(memory))],_)
	).

thread_create(Goal, Id, Options) :-
	G0 = thread_create(Goal, Id, Options),
	'$check_callable'(Goal,G0),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'(Options, Alias, Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	(	var(Alias) ->
		'$record_thread_info'(Id, [Stack, Trail, System], Detached, AtExit)
	;	'$record_thread_info'(Id, Alias, [Stack, Trail, System], Detached, AtExit, G0)
	),
	'$create_thread_mq'(Id),
	(
	 '$create_thread'(Goal, Stack, Trail, System, Detached, Id)
	->
	 true
	;
	 recorda('$thread_exit_status', [Id|exception(resource_error(memory))],_)
	).

'$erase_thread_info'(Id) :-
	recorded('$thread_exit_status', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_alias',[Id|_],R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_sizes', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_detached', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_at_exit', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_exit_hook', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(_).


'$thread_options'(V, _, _, _, _, _, _, G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$thread_options'([], _, Stack, Trail, System, Detached, AtExit, _) :-
	recorded('$thread_defaults', [DefaultStack, DefaultTrail, DefaultSystem, DefaultDetached, DefaultAtExit], _),
	( var(Stack) -> Stack = DefaultStack; true ),
	( var(Trail) -> Trail = DefaultTrail; true ),
	( var(System) -> System = DefaultSystem; true ),
	( var(Detached) -> Detached = DefaultDetached; true ),
	( var(AtExit) -> AtExit = DefaultAtExit; true ).
'$thread_options'([Opt|Opts], Alias, Stack, Trail, System, Detached, AtExit, G0) :-
	'$thread_option'(Opt, Alias, Stack, Trail, System, Detached, AtExit, G0),
	'$thread_options'(Opts, Alias, Stack, Trail, System, Detached, AtExit, G0).

'$thread_option'(Option, _, _, _, _, _, _, G0) :- var(Option), !,
	'$do_error'(instantiation_error,G0).
'$thread_option'(alias(Alias), Alias, _, _, _, _, _, G0) :- !,
	( \+ atom(Alias) -> '$do_error'(type_error(atom,Alias),G0) ; true ).
'$thread_option'(stack(Stack), _, Stack, _, _, _, _, G0) :- !,
	( \+ integer(Stack) -> '$do_error'(type_error(integer,Stack),G0) ; true ).
'$thread_option'(trail(Trail), _, _, Trail, _, _, _, G0) :- !,
	( \+ integer(Trail) -> '$do_error'(type_error(integer,Trail),G0) ; true ).
'$thread_option'(system(System), _, _, _, System, _, _, G0) :- !,
	( \+ integer(System) -> '$do_error'(type_error(integer,System),G0) ; true ).
'$thread_option'(detached(Detached), _, _, _, _, Detached, _, G0) :- !,
	( Detached \== true, Detached \== false -> '$do_error'(domain_error(thread_option,Detached+[true,false]),G0) ; true ).
'$thread_option'(at_exit(AtExit), _, _, _, _, _, M:AtExit, G0) :- !,
	'$current_module'(M),
	( \+ callable(AtExit) -> '$do_error'(type_error(callable,AtExit),G0) ; true ).
'$thread_option'(Option, _, _, _, _, _, _, G0) :-
	'$do_error'(domain_error(thread_option,Option),G0).

'$record_thread_info'(_, Alias, _, _, _, Goal) :-
	recorded('$thread_alias', [_|Alias], _), !,
	'$do_error'(permission_error(create,thread,alias(Alias)), Goal).
'$record_thread_info'(Id, Alias, Sizes, Detached, AtExit, _) :-
	recorda('$thread_alias', [Id|Alias], _),
	'$record_thread_info'(Id, Sizes, Detached, AtExit).

'$record_thread_info'(Id, Sizes, Detached, AtExit) :-
	recorda('$thread_sizes', [Id|Sizes], _),
	recorda('$thread_detached', [Id|Detached], _),
	(	AtExit == true ->
		true
	;	recorda('$thread_at_exit', [Id|AtExit], _)
	).

% vsc: ?????
thread_defaults(Defaults) :-
	nonvar(Defaults), !,
	'$do_error'(type_error(variable, Defaults), thread_defaults(Defaults)).
thread_defaults([stack(Stack), trail(Trail), system(System), detached(Detached), at_exit(AtExit)]) :-
	recorded('$thread_defaults',[Stack, Trail, System, Detached, AtExit], _).

thread_default(Default) :-
	var(Default), !,
	recorded('$thread_defaults', Defaults, _),
	'$thread_default'(Default, Defaults).
thread_default(stack(Stack)) :- !,
	recorded('$thread_defaults',[Stack, _, _, _, _], _).
thread_default(trail(Trail)) :- !,
	recorded('$thread_defaults',[_, Trail, _, _, _], _).
thread_default(system(System)) :- !,
	recorded('$thread_defaults',[_, _, System, _, _], _).
thread_default(detached(Detached)) :- !,
	recorded('$thread_defaults',[_, _, _, Detached, _], _).
thread_default(at_exit(AtExit)) :- !,
	recorded('$thread_defaults',[_, _, _, _, AtExit], _).
thread_default(Default) :-
	'$do_error'(type_error(thread_option,Default),thread_default(Default)).

'$thread_default'(stack(Stack), [Stack, _, _, _, _]).
'$thread_default'(trail(Trail), [_, Trail, _, _, _]).
'$thread_default'(stack(System), [_, _, System, _, _]).
'$thread_default'(detached(Detached), [_, _, _, Detached, _]).
'$thread_default'(at_exit(AtExit), [_, _, _, _, AtExit]).

thread_set_defaults(V) :- var(V), !,
	'$do_error'(instantiation_error, thread_set_defaults(V)).
thread_set_defaults([Default| Defaults]) :- !,
	'$thread_set_defaults'([Default| Defaults], thread_set_defaults([Default| Defaults])).
thread_set_defaults(T) :-
	'$do_error'(type_error(list, T), thread_set_defaults(T)).

'$thread_set_defaults'([], _).
'$thread_set_defaults'([Default| Defaults], G) :- !,
	'$thread_set_default'(Default, G),
	'$thread_set_defaults'(Defaults, G).

thread_set_default(V) :- var(V), !,
	'$do_error'(instantiation_error, thread_set_default(V)).
thread_set_default(Default) :-
	'$thread_set_default'(Default, thread_set_default(Default)).

'$thread_set_default'(stack(Stack), G) :-
	\+ integer(Stack), !,
	'$do_error'(type_error(integer, Stack), G).
'$thread_set_default'(stack(Stack), G) :-
	Stack < 0, !,
	'$do_error'(domain_error(not_less_than_zero, Stack), G).
'$thread_set_default'(stack(Stack), _) :- !,
	recorded('$thread_defaults', [_, Trail, System, Detached, AtExit], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached, AtExit], _).

'$thread_set_default'(trail(Trail), G) :-
	\+ integer(Trail), !,
	'$do_error'(type_error(integer, Trail), G).
'$thread_set_default'(trail(Trail), G) :-
	Trail < 0, !,
	'$do_error'(domain_error(not_less_than_zero, Trail), G).
'$thread_set_default'(trail(Trail), _) :- !,
	recorded('$thread_defaults', [Stack, _, System, Detached, AtExit], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached, AtExit], _).

'$thread_set_default'(system(System), G) :-
	\+ integer(System), !,
	'$do_error'(type_error(integer, System), G).
'$thread_set_default'(system(System), G0) :-
	System < 0, !,
	'$do_error'(domain_error(not_less_than_zero, System), G0).
'$thread_set_default'(system(System), _) :- !,
	recorded('$thread_defaults', [Stack, Trail, _, Detached, AtExit], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached, AtExit], _).

'$thread_set_default'(detached(Detached), G) :-
	Detached \== true, Detached \== false, !,
	'$do_error'(type_error(boolean, Detached), G).
'$thread_set_default'(detached(Detached), _) :- !,
	recorded('$thread_defaults', [Stack, Trail, System, _, AtExit], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached, AtExit], _).

'$thread_set_default'(at_exit(AtExit), G) :-
	\+ callable(AtExit), !,
	'$do_error'(type_error(callable, AtExit), G).
'$thread_set_default'(at_exit(AtExit), _) :- !,
	recorded('$thread_defaults', [Stack, Trail, System, Detached, _], Ref),
	erase(Ref),
	'$current_module'(M),
	recorda('$thread_defaults', [Stack, Trail, System, Detached, M:AtExit], _).

'$thread_set_default'(Default, G) :-
	'$do_error'(domain_error(thread_default, Default), G).

thread_self(Id) :-
	nonvar(Id), \+ integer(Id), \+ atom(Id), !,
	'$do_error'(domain_error(thread_or_alias, Id), thread_self(Id)).
thread_self(Id) :-
	'$thread_self'(Id0),
	'$thread_id_alias'(Id0, Id).

/* Exit status may be either true, false, exception(Term), or exited(Term) */
thread_join(Id, Status) :-
	nonvar(Status), !,
	'$do_error'(type_error(variable,Status),thread_join(Id, Status)).
thread_join(Id, Status) :-
	'$check_thread_or_alias'(Id, thread_join(Id, Status)),
	'$thread_id_alias'(Id0, Id),
	'$thread_join'(Id0),
	recorded('$thread_exit_status', [Id0|Status], _),
	'$erase_thread_info'(Id0),
	'$thread_destroy'(Id0).

thread_cancel(Id) :-
	(Id == main; Id == 0), !,
	'$do_error'(permission_error(cancel, thread, main), thread_cancel(Id)).
thread_cancel(Id) :-
	thread_signal(Id, throw(error(thread_cancel(Id),thread_cancel(Id)))).

thread_detach(Id) :-
	'$check_thread_or_alias'(Id, thread_detach(Id)),
	'$thread_id_alias'(Id0, Id),
	(
	 recorded('$thread_detached', [Id0|_], R),
	 erase(R),
	 fail
	;
	 recordz('$thread_detached', [Id0|true], _),
	 fail
	;
	'$detach_thread'(Id0)
	),
	(	recorded('$thread_exit_status', [Id0|_], _) ->
		'$erase_thread_info'(Id0),
		'$thread_destroy'(Id0)
	;
		'$thread_unlock'(Id0)
	).

thread_exit(Term) :-
	var(Term), !,
	'$do_error'(instantiation_error, thread_exit(Term)).
thread_exit(Term) :-
	'$close_thread'('$thread_finished'(exited(Term)), Detached).

'$run_at_thread_exit'(Id0) :-
	recorded('$thread_at_exit',[Id0|AtExit],R), erase(R),
	catch(once(AtExit), _, fail),
	fail.
'$run_at_thread_exit'(Id0) :-
	recorded('$thread_exit_hook',[Id0|Hook],R), erase(R),
	catch(once(Hook),_,fail),
	fail.
'$run_at_thread_exit'(Id0) :-
	message_queue_destroy(Id0).

thread_at_exit(Goal) :-
	'$check_callable'(Goal,thread_at_exit(Goal)),
	'$thread_self'(Id0),
	recordz('$thread_exit_hook',[Id0|Goal],_).

current_thread(Id, Status) :-
	thread_property(Id, status(Status)).


'$thread_id_alias'(Id, Alias) :-
	recorded('$thread_alias', [Id|Alias], _), !.
'$thread_id_alias'(Id, Id).


'$mutex_id_alias'(Id, Alias) :-
	recorded('$mutex_alias', [Id|Alias], _), !.
'$mutex_id_alias'(Id, Id).


mutex_create(Mutex) :-
	(	atom(Mutex) ->
		mutex_create(_, [alias(Mutex)])
	;	mutex_create(Mutex, [])
	).

mutex_create(Id, Options) :-
	nonvar(Id), !,
	'$do_error'(type_error(variable, Id), mutex_create(Id, Options)).
mutex_create(Id, Options) :-
	Goal = mutex_create(Id, Options),
	'$mutex_options'(Options, Alias, Goal),
	(	atom(Alias) ->
		(	recorded('$mutex_alias',[_| Alias], _) ->
			'$do_error'(permission_error(create, mutex, Alias), Goal)
		;	'$new_mutex'(Id),
			recorda('$mutex_alias', [Id| Alias], _)
		)
	;	'$new_mutex'(Id),
		recorda('$mutex_alias', [Id| Id], _)
	).	

'$mutex_options'(Var, _, Goal) :-
	var(Var), !,
	'$do_error'(instantiation_error, Goal).
'$mutex_options'([], _, _) :- !.
'$mutex_options'([Option| Options], Alias, Goal) :- !,
	'$mutex_option'(Option, Alias, Goal),
	'$mutex_options'(Options, Alias, Goal).
'$mutex_options'(Options, _, Goal) :-
	'$do_error'(type_error(list, Options), Goal).

'$mutex_option'(Var, _, Goal) :-
	var(Var), !,
	'$do_error'(instantiation_error, Goal).
'$mutex_option'(alias(Alias), Alias, Goal) :- !,
	(	atom(Alias) ->
		true
	;	'$do_error'(type_error(atom, Alias), Goal)
	).
'$mutex_option'(Option, _, Goal) :-
	'$do_error'(domain_error(mutex_option, Option), Goal).

/*
mutex_create(V) :-
	var(V), !,
	'$new_mutex'(V),
	recorda('$mutex_alias',[V|V],_).
mutex_create(A) :-
	atom(A),
	recorded('$mutex_alias',[_|A],_), !,
	'$do_error'(permission_error(create,mutex,A),mutex_create(A)).
mutex_create(A) :-
	atom(A), !,
	'$new_mutex'(Id),
	recorda('$mutex_alias',[Id|A],_).
mutex_create(V) :-
	'$do_error'(type_error(atom,V),mutex_create(V)).
*/
	
mutex_destroy(Mutex) :-
	'$check_mutex_or_alias'(Mutex, mutex_destroy(Mutex)),
	'$mutex_id_alias'(Id, Mutex),
	'$destroy_mutex'(Id),
	'$erase_mutex_info'(Id).

'$erase_mutex_info'(Id) :-
	recorded('$mutex_alias',[Id|_],R),
	erase(R),
	fail.
'$erase_mutex_info'(_).

mutex_lock(V) :-
	var(V), !,
	'$do_error'(instantiation_error,mutex_lock(V)).
mutex_lock(A) :-
	recorded('$mutex_alias',[Id|A],_), !,
	'$lock_mutex'(Id).
mutex_lock(A) :-
	atom(A), !,
	mutex_create(A),
	mutex_lock(A).
mutex_lock(V) :-
	'$do_error'(type_error(atom,V),mutex_lock(V)).
	
mutex_trylock(V) :-
	var(V), !,
	'$do_error'(instantiation_error,mutex_trylock(V)).
mutex_trylock(A) :-
	recorded('$mutex_alias',[Id|A],_), !,
	'$trylock_mutex'(Id).
mutex_trylock(A) :-
	atom(A), !,
	mutex_create(A),
	mutex_trylock(A).
mutex_trylock(V) :-
	'$do_error'(type_error(atom,V),mutex_trylock(V)).
	
mutex_unlock(Mutex) :-
	'$check_mutex_or_alias'(Mutex, mutex_unlock(Mutex)),
	'$mutex_id_alias'(Id, Mutex),
	( '$unlock_mutex'(Id) ->
	    true
	;
	    '$do_error'(permission_error(unlock,mutex,Mutex),mutex_unlock(Mutex))
	).

mutex_unlock_all :-
	'$thread_self'(Tid),
	'$unlock_all_thread_mutexes'(Tid).

'$unlock_all_thread_mutexes'(Tid) :-
	recorded('$mutex_alias',[Id|_],_),
	'$mutex_info'(Id, NRefs, Tid),
	NRefs > 0,
	'$mutex_unlock_all'(Id),
	fail.
'$unlock_all_thread_mutexes'(_).

'$mutex_unlock_all'(Id) :-
	'$mutex_info'(Id, NRefs, _),
	NRefs > 0,
	'$unlock_mutex'(Id),
	'$mutex_unlock_all'(Id).

with_mutex(M, G) :-
	var(M), !,
	'$do_error'(instantiation_error,with_mutex(M, G)).
with_mutex(M, G) :-
	var(G), !,
	'$do_error'(instantiation_error,with_mutex(M, G)).
with_mutex(M, G) :-
	\+ callable(G), !,
	'$do_error'(type_error(callable,G),with_mutex(M, G)).
with_mutex(M, G) :-
	atom(M), !,
	'$with_mutex_mutex'(WMId),
	'$lock_mutex'(WMId),
	(	recorded('$mutex_alias',[Id|M],_) ->
		true
	;	'$new_mutex'(Id),
		recorda('$mutex_alias',[Id|M],_)
	),
	'$lock_mutex'(Id),
	'$unlock_mutex'(WMId),
	(	catch('$execute'(G), E, ('$unlock_mutex'(Id), throw(E))) ->
		'$unlock_mutex'(Id)
	;	'$unlock_mutex'(Id),
		fail
	).
with_mutex(M, G) :-
	'$do_error'(type_error(atom,M),with_mutex(M, G)).

current_mutex(M, T, NRefs) :-
	recorded('$mutex_alias',[Id|M],_),
	'$mutex_info'(Id, NRefs, T).

mutex_property(Mutex, Prop) :-
	(	nonvar(Mutex) ->
		'$check_mutex_or_alias'(Mutex, mutex_property(Mutex, Prop))
	;	recorded('$mutex_alias', [_|Mutex], _)
	),
	'$check_mutex_property'(Prop, mutex_property(Mutex, Prop)),
	'$mutex_id_alias'(Id, Mutex),
	'$mutex_property'(Id, Prop).

'$mutex_property'(Id, alias(Alias)) :-
	recorded('$mutex_alias', [Id|Alias], _),
	Id \= Alias.
'$mutex_property'(Id, status(Status)) :-
	'$mutex_info'(Id, Count, HoldingThread),
	(	Count =:= 0 ->
		Status = unlocked
	;	% Count > 0,
		'$thread_id_alias'(HoldingThread, Alias),
		once((Thread = Alias; Thread = HoldingThread)),
		Status = locked(Thread, Count)
	).


message_queue_create(Id, Options) :-
	nonvar(Id), !,
	'$do_error'(type_error(variable, Id), message_queue_create(Id, Options)).
message_queue_create(Id, Options) :-
	var(Options), !,
	'$do_error'(instantiation_error, message_queue_create(Id, Options)).
message_queue_create(Id, []) :- !,
	'$global_queue_mutex'(QMutex),
	'$new_mutex'(Mutex),
	'$cond_create'(Cond),
	'$mq_new_id'(Id, NId, Key),
	recorda('$queue',q(Id,Mutex,Cond,NId,Key), _),
	'$unlock_mutex'(QMutex).
message_queue_create(Id, [alias(Alias)]) :-
	var(Alias), !,
	'$do_error'(instantiation_error, message_queue_create(Id, [alias(Alias)])).
message_queue_create(Id, [alias(Alias)]) :-
	\+ atom(Alias), !,
	'$do_error'(type_error(atom,Alias), message_queue_create(Id, [alias(Alias)])).
message_queue_create(Id, [alias(Alias)]) :- !,
	'$global_queue_mutex'(QMutex),
	'$lock_mutex'(QMutex),
	'$new_mutex'(Mutex),
	'$cond_create'(Cond),
	(	recorded('$queue', q(Alias,_,_,_,_), _) ->
		'$unlock_mutex'(QMutex),
		'$do_error'(permission_error(create,queue,alias(Alias)),message_queue_create(Id, [alias(Alias)]))
	;	recorded('$thread_alias', [_|Alias], _) ->
		'$unlock_mutex'(QMutex),
		'$do_error'(permission_error(create,queue,alias(Alias)),message_queue_create(Id, [alias(Alias)]))
	;	'$mq_new_id'(Id, NId, Key),
		recorda('$queue',q(Alias,Mutex,Cond,NId,Key), _),
		'$unlock_mutex'(QMutex)
	).
message_queue_create(Id, [Option| _]) :-
	'$do_error'(domain_error(queue_option, Option), message_queue_create(Id, [Option| _])).
message_queue_create(Id, Options) :-
	'$do_error'(type_error(list, Options), message_queue_create(Id, Options)).

message_queue_create(Id) :-
	(	var(Id) ->		% ISO DTR
		message_queue_create(Id, [])
	;	atom(Id) ->		% old behavior
		message_queue_create(_, [alias(Id)])
	;	'$do_error'(type_error(variable, Id), message_queue_create(Id))
	).

'$create_thread_mq'(TId) :-
	'$global_queue_mutex'(QMutex),
	'$new_mutex'(Mutex),
	'$cond_create'(Cond),
	'$mq_new_id'(TId, TId, Key),
	recorda('$queue', q(TId,Mutex,Cond,TId,Key), _),
	'$unlock_mutex'(QMutex).

'$mq_new_id'(Id, Id, AtId) :-
	integer(Id), !,
	\+ recorded('$queue', q(_,_,_,Id,_), _),
	atomic_concat('$queue__',Id,AtId),
	!.
'$mq_new_id'(_, Id, AtId) :-
	'$integers'(Id),
	\+ recorded('$queue', q(_,_,_,Id,_), _),
	atomic_concat('$queue__',Id,AtId),
	!.

'$integers'(-1).
'$integers'(I) :-
	'$integers'(I1),
	I is I1-1.

	
message_queue_destroy(Name) :-
	var(Name), !,
	'$do_error'(instantiation_error,message_queue_destroy(Name)).
message_queue_destroy(Queue) :-
	'$global_queue_mutex'(QMutex),
	'$lock_mutex'(QMutex),
	recorded('$queue',q(Queue,Mutex,Cond,_,QKey),R), !,
	erase(R),
	'$cond_destroy'(Cond),
	'$destroy_mutex'(Mutex),
	'$unlock_mutex'(QMutex),
	'$clean_mqueue'(QKey).
message_queue_destroy(Queue) :-
	'$global_queue_mutex'(QMutex),
	'$unlock_mutex'(QMutex),
	atomic(Queue), !,
	'$do_error'(existence_error(message_queue,Queue),message_queue_destroy(Queue)).
message_queue_destroy(Name) :-
	'$do_error'(type_error(atom,Name),message_queue_destroy(Name)).

'$clean_mqueue'(Queue) :-
	recorded(Queue,_,R),
	erase(R),
	fail.
'$clean_mqueue'(_).


message_queue_property(Id, Prop) :-
	(	nonvar(Id) ->
		'$check_message_queue_or_alias'(Id, message_queue_property(Id, Prop))
	;	recorded('$queue', q(Id,_,_,_,_), _)
	),
	'$check_message_queue_property'(Prop, message_queue_property(Id, Prop)),
	'$message_queue_id_alias'(Id0, Id),
	'$message_queue_property'(Id0, Prop).

'$check_message_queue_or_alias'(Term, Goal) :-
	var(Term), !,
	'$do_error'(instantiation_error, Goal).
'$check_message_queue_or_alias'(Term, Goal) :-
	\+ atom(Term),
	Term \= '$message_queue'(_), !,
	'$do_error'(domain_error(queue_or_alias, Term), Goal).
'$check_message_queue_or_alias'('$message_queue'(I), Goal) :-
	\+ recorded('$queue', q(_,_,_,I,_), _), !,
	'$do_error'(existence_error(queue, '$message_queue'(I)), Goal).
'$check_message_queue_or_alias'(Term, Goal) :-
	atom(Term),
	\+ recorded('$queue', q(Term,_,_,_,_), _), !,
	'$do_error'(existence_error(queue, Term), Goal).
'$check_message_queue_or_alias'(_, _).

'$message_queue_id_alias'(Id, Alias) :-
	recorded('$queue', q(Alias,_,_,Id,_), _), !.
'$message_queue_id_alias'(Id, Id).

'$check_message_queue_property'(Term, _) :-
	var(Term), !.
'$check_message_queue_property'(alias(_), _) :- !.
'$check_message_queue_property'(size(_), _) :- !.
'$check_message_queue_property'(max_size(_), _) :- !.
'$check_message_queue_property'(Term, Goal) :-
	'$do_error'(domain_error(queue_property, Term), Goal).

'$message_queue_property'(Id, alias(Alias)) :-
	recorded('$queue', q(Alias,_,_,Id,_), _).


thread_send_message(Term) :-
	'$thread_self'(Id),
	thread_send_message(Id, Term).

thread_send_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_send_message(Queue,Term)).
thread_send_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_), !,
	thread_send_message(Id, Term).
thread_send_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$lock_mutex'(QMutex),
	recorded('$queue',q(Queue,Mutex,Cond,_,Key),_), !,
	'$lock_mutex'(Mutex),
	'$unlock_mutex'(QMutex),
	recordz(Key,Term,_),
	'$cond_broadcast'(Cond),
	'$unlock_mutex'(Mutex).
thread_send_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$unlock_mutex'(QMutex),
	'$do_error'(existence_error(queue,Queue),thread_send_message(Queue,Term)).

thread_get_message(Term) :-
	'$thread_self'(Id),
	thread_get_message(Id, Term).

thread_get_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_get_message(Queue,Term)).
thread_get_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_), !,
	thread_get_message(Id, Term).
thread_get_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$lock_mutex'(QMutex),
	recorded('$queue',q(Queue,Mutex,Cond,_,Key),_), !,
	'$lock_mutex'(Mutex),
	'$unlock_mutex'(QMutex),
	'$thread_get_message_loop'(Key, Term, Mutex, Cond).
thread_get_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$unlock_mutex'(QMutex),
	'$do_error'(existence_error(message_queue,Queue),thread_get_message(Queue,Term)).


'$thread_get_message_loop'(Key, Term, Mutex, _) :-
	recorded(Key,Term,R), !,
	erase(R),
	'$unlock_mutex'(Mutex).
'$thread_get_message_loop'(Key, Term, Mutex, Cond) :-
	'$cond_wait'(Cond, Mutex),
	'$thread_get_message_loop'(Key, Term, Mutex, Cond).

thread_peek_message(Term) :-
	'$thread_self'(Id),
	thread_peek_message(Id, Term).

thread_peek_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_peek_message(Queue,Term)).
thread_peek_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_), !,
	thread_peek_message(Id, Term).
thread_peek_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$lock_mutex'(QMutex),
	recorded('$queue',q(Queue,Mutex,_,_,Key),_), !,
	'$lock_mutex'(Mutex),
	'$unlock_mutex'(QMutex),
	'$thread_peek_message2'(Key, Term, Mutex).
thread_peek_message(Queue, Term) :-
	'$global_queue_mutex'(QMutex),
	'$unlock_mutex'(QMutex),
	'$do_error'(existence_error(message_queue,Queue),thread_peek_message(Queue,Term)).


'$thread_peek_message2'(Key, Term, Mutex) :-
	recorded(Key,Term,_), !,
	'$unlock_mutex'(Mutex).
'$thread_peek_message2'(_, _, Mutex) :-
	'$unlock_mutex'(Mutex),
	fail.

thread_local(X) :-
	'$current_module'(M),
	'$thread_local'(X,M).

'$thread_local'(X,M) :- var(X), !,
	'$do_error'(instantiation_error,thread_local(M:X)).
'$thread_local'(Mod:Spec,_) :- !,
	'$thread_local'(Spec,Mod).
'$thread_local'([], _) :- !.
'$thread_local'([H|L], M) :- !, '$thread_local'(H, M), '$thread_local'(L, M).
'$thread_local'((A,B),M) :- !, '$thread_local'(A,M), '$thread_local'(B,M).
'$thread_local'(X,M) :- !,
	'$thread_local2'(X,M).

'$thread_local2'(A/N, Mod) :- integer(N), atom(A), !,
	functor(T,A,N),
	(Mod \= idb -> '$flags'(T,Mod,F,F) ; true),
	( '$install_thread_local'(T,Mod) -> true ;
	   F /\ 0x08002000 =\= 0 -> '$do_error'(permission_error(modify,dynamic_procedure,A/N),thread_local(Mod:A/N)) ;
	   '$do_error'(permission_error(modify,static_procedure,A/N),thread_local(Mod:A/N))
	).
'$thread_local2'(X,Mod) :- 
	'$do_error'(type_error(callable,X),thread_local(Mod:X)).


thread_sleep(Time) :-
	var(Time), !,
	'$do_error'(instantiation_error,thread_sleep(Time)).
thread_sleep(Time) :-
	integer(Time), !,
	(	Time > 0 ->
		'$thread_sleep'(Time,0,_,_)
	;	true
	).
thread_sleep(Time) :-
	float(Time), !,
	(	Time > 0.0 ->
		STime is integer(float_integer_part(Time)),
		NTime is integer(float_fractional_part(Time))*1000000000,
		'$thread_sleep'(STime,NTime,_,_)
	;	true
	).
thread_sleep(Time) :-
	'$do_error'(type_error(number,Time),thread_sleep(Time)).


thread_signal(Id, Goal) :-
	'$check_thread_or_alias'(Id, thread_signal(Id, Goal)),
	'$check_callable'(Goal, thread_signal(Id, Goal)),
	'$thread_id_alias'(Id0, Id),
	(	recorded('$thread_signal', [Id0| _], R), erase(R), fail
	;	true
	),
	recorda('$thread_signal', [Id0| Goal], _),
	'$signal_thread'(Id0).

'$thread_gfetch'(G) :-
	'$thread_self'(Id),
	recorded('$thread_signal',[Id|G],R),
	erase(R).


thread_property(Prop) :-
	'$check_thread_property'(Prop, thread_property(Prop)),
	'$thread_self'(Id),
	'$thread_property'(Id, Prop).

thread_property(Id, Prop) :-
	(	nonvar(Id) ->
		'$check_thread_or_alias'(Id, thread_property(Id, Prop))
	;	recorded('$thread_sizes', [Id| _], _)
	),
	'$check_thread_property'(Prop, thread_property(Id, Prop)),
	'$thread_id_alias'(Id0, Id),
	'$thread_property'(Id0, Prop).

'$thread_property'(Id, alias(Alias)) :-
	recorded('$thread_alias', [Id|Alias], _).
'$thread_property'(Id, status(Status)) :-
	(	recorded('$thread_exit_status', [Id|Exit], _) ->
		Status = Exit
	;	Status = running
	).
'$thread_property'(Id, detached(Detached)) :-
	recorded('$thread_detached', [Id|Detached], _).
'$thread_property'(Id, at_exit(AtExit)) :-
	recorded('$thread_at_exit', [Id|AtExit], _).
'$thread_property'(Id, stack(Stack)) :-
	recorded('$thread_sizes', [Id, Stack, _, _], _).
'$thread_property'(Id, trail(Trail)) :-
	recorded('$thread_sizes', [Id, _, Trail, _], _).
'$thread_property'(Id, system(System)) :-
	recorded('$thread_sizes', [Id, _, _, System], _).


threads :-
	format(user_error,'------------------------------------------------------------------------~n',[]),
	format(user_error, '~t~a~48+~n', 'Thread  Detached  Status'),
	format(user_error,'------------------------------------------------------------------------~n',[]),
	'$thread_property'(Id, detached(Detached)),
	'$thread_property'(Id, status(Status)),
	'$thread_id_alias'(Id, Alias),
	format(user_error,'~t~q~30+~33|~w~42|~q~n', [Alias, Detached, Status]),
	fail.
threads :-
	format(user_error,'------------------------------------------------------------------------~n',[]).


'$check_thread_or_alias'(Term, Goal) :-
	var(Term), !,
	'$do_error'(instantiation_error, Goal).
'$check_thread_or_alias'(Term, Goal) :-
	\+ integer(Term), \+ atom(Term), !,
	'$do_error'(domain_error(thread_or_alias, Term), Goal).
'$check_thread_or_alias'(Term, Goal) :-
	atom(Term), \+ recorded('$thread_alias',[_|Term],_), !,
	'$do_error'(existence_error(thread, Term), Goal).
'$check_thread_or_alias'(Term, Goal) :-
	integer(Term), \+ '$valid_thread'(Term), !,
	'$do_error'(existence_error(thread, Term), Goal).
'$check_thread_or_alias'(_,_).

'$check_thread_property'(Term, _) :-
	var(Term), !.
'$check_thread_property'(alias(_), _) :- !.
'$check_thread_property'(detached(_), _) :- !.
'$check_thread_property'(at_exit(_), _) :- !.
'$check_thread_property'(status(_), _) :- !.
'$check_thread_property'(stack(_), _) :- !.
'$check_thread_property'(trail(_), _) :- !.
'$check_thread_property'(system(_), _) :- !.
'$check_thread_property'(Term, Goal) :-
	'$do_error'(domain_error(thread_property, Term), Goal).

'$check_mutex_or_alias'(Term, Goal) :-
	var(Term), !,
	'$do_error'(instantiation_error, Goal).
'$check_mutex_or_alias'(Term, Goal) :-
	\+ integer(Term), \+ atom(Term), !,
	'$do_error'(domain_error(mutex_or_alias, Term), Goal).
'$check_mutex_or_alias'(Term, Goal) :-
	atom(Term), \+ recorded('$mutex_alias',[_|Term],_), !,
	'$do_error'(existence_error(mutex, Term), Goal).
'$check_mutex_or_alias'(Term, Goal) :-
%	integer(Term), \+ '$valid_mutex'(Term), !,
	integer(Term), \+ recorded('$mutex_alias',[Term|_],_), !,
	'$do_error'(existence_error(mutex, Term), Goal).
'$check_mutex_or_alias'(_,_).

'$check_mutex_property'(Term, _) :-
	var(Term), !.
'$check_mutex_property'(alias(_), _) :- !.
'$check_mutex_property'(status(Status), Goal) :- !,
	(	var(Status) ->
		true
	;	Status = unlocked ->
		true
	;	Status = locked(_, _) ->
		true
	;	'$do_error'(domain_error(mutex_property, status(Status)), Goal)
	).
'$check_mutex_property'(Term, Goal) :-
	'$do_error'(domain_error(mutex_property, Term), Goal).
