/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		threads.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	support threads						 *
*									 *
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
	no_threads, !.
'$init_thread0' :-
	'$create_mq'(0),
	'$record_thread_info'(0, main, [0, 0, 0], false, '$init_thread0'),
	recorda('$thread_defaults', [0, 0, 0, false], _),
	'$new_mutex'(Id),
	recorda('$with_mutex_mutex',Id,_).

'$top_thread_goal'(G, Detached) :-
	'$thread_self'(Id),
	(Detached == true -> '$detach_thread'(Id) ; true),
	'$current_module'(Module),
	'$system_catch'((G,'$close_thread'(Detached,true) ; '$close_thread'(Detached,false)),Module,Exception,'$thread_exception'(Exception,Detached)).

'$close_thread'(Detached, Status) :-
	'$thread_self'(Id0),
	(Detached == true ->
	    true
	;
	    recorda('$thread_exit_status', [Id0|Status], _)
	),
	'$run_at_thread_exit'(Id0).

'$thread_exception'(Exception,Detached) :-
	'$thread_self'(Id0),
	(Detached == true ->
	    true
	;
	    recorda('$thread_exit_status', [Id0|exception(Exception)], _)
	),
	'$run_at_thread_exit'(Id0).

thread_create(Goal) :-
	G0 = thread_create(Goal),
	'$check_callable'(Goal, G0),
	'$thread_options'([detached(true)], [], Stack, Trail, System, Detached, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	'$record_thread_info'(Id, [Stack, Trail, System], true),
	'$create_mq'(Id),	
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id).

thread_create(Goal, OutId) :-
	G0 = thread_create(Goal, Id),
	'$check_callable'(Goal, G0),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'([], [], Stack, Trail, System, Detached, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	'$record_thread_info'(Id, [Stack, Trail, System], Detached),
	'$create_mq'(Id),	
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id),
	OutId = Id.

thread_create(Goal, OutId, Options) :-
	G0 = thread_create(Goal, Id, Options),
	'$check_callable'(Goal,G0),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'(Options, Alias, Stack, Trail, System, Detached, G0),
	'$thread_new_tid'(Id),
	'$erase_thread_info'(Id),
	(	var(Alias) ->
		'$record_thread_info'(Id, [Stack, Trail, System], Detached)
	;	'$record_thread_info'(Id, Alias, [Stack, Trail, System], Detached, G0)
	),
	'$create_mq'(Id),	
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id),
	OutId = Id.

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
	recorded('$thread_exit_hook', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(_).


'$thread_options'(V, _, _, _, _, _, G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$thread_options'([], _, Stack, Trail, System, Detached, _) :-
	recorded('$thread_defaults', [DefaultStack, DefaultTrail, DefaultSystem, DefaultDetached], _),
	( var(Stack) -> Stack = DefaultStack; true ),
	( var(Trail) -> Trail = DefaultTrail; true ),
	( var(System) -> System = DefaultSystem; true ),
	( var(Detached) -> Detached = DefaultDetached; true ).
'$thread_options'([Opt|Opts], Alias, Stack, Trail, System, Detached, G0) :-
	'$thread_option'(Opt, Alias, Stack, Trail, System, Detached, G0),
	'$thread_options'(Opts, Alias, Stack, Trail, System, Detached, G0).

'$thread_option'(Option, _, _, _, _, _, G0) :- var(Option), !,
	'$do_error'(instantiation_error,G0).
'$thread_option'(stack(Stack), _, Stack, _, _, _, G0) :- !,
	( \+ integer(Stack) -> '$do_error'(type_error(integer,Stack),G0) ; true ).
'$thread_option'(trail(Trail), _, _, Trail, _, _, G0) :- !,
	( \+ integer(Trail) -> '$do_error'(type_error(integer,Trail),G0) ; true ).
'$thread_option'(system(System), _, _, _, System, _, G0) :- !,
	( \+ integer(System) -> '$do_error'(type_error(integer,System),G0) ; true ).
'$thread_option'(alias(Alias), Alias, _, _, _, _, G0) :- !,
	( \+ atom(Alias) -> '$do_error'(type_error(atom,Alias),G0) ; true ).
'$thread_option'(detached(Detached), _, _, _, _, Detached, G0) :- !,
	( Detached \== true, Detached \== false  -> '$do_error'(domain_error(thread_option,Detached+[true,false]),G0) ; true ).
'$thread_option'(Option, _, _, _, _, _, G0) :-
	'$do_error'(domain_error(thread_option,Option),G0).

'$record_thread_info'(_, Alias, _, _, Goal) :-
	recorded('$thread_alias', [_|Alias], _), !,
	'$do_error'(permission_error(create,thread,alias(Alias)), Goal).
'$record_thread_info'(Id, Alias, Sizes, Detached, _) :-
	recorda('$thread_alias', [Id|Alias], _),
	'$record_thread_info'(Id, Sizes, Detached).

'$record_thread_info'(Id, Sizes, Detached) :-
	recorda('$thread_sizes', [Id|Sizes], _),
	recorda('$thread_detached', [Id|Detached], _).

% vsc: ?????
thread_defaults(Defaults) :-
	nonvar(Defaults), !,
	'$do_error'(type_error(variable, Defaults), thread_defaults(Defaults)).
thread_defaults([stack(Stack), trail(Trail), system(System), detached(Detached)]) :-
	recorded('$thread_defaults',[Stack, Trail, System, Detached], _).

thread_default(Default) :-
	var(Default), !,
	recorded('$thread_defaults', Defaults, _),
	'$thread_default'(Default, Defaults).
thread_default(stack(Stack)) :- !,
	recorded('$thread_defaults',[Stack, _, _, _], _).
thread_default(trail(Trail)) :- !,
	recorded('$thread_defaults',[_, Trail, _, _], _).
thread_default(system(System)) :- !,
	recorded('$thread_defaults',[_, _, System, _], _).
thread_default(detached(Detached)) :- !,
	recorded('$thread_defaults',[_, _, _, Detached], _).
thread_default(Default) :-
	'$do_error'(type_error(thread_option,Default),thread_default(Default)).

'$thread_default'(stack(Stack), [Stack, _, _, _]).
'$thread_default'(trail(Trail), [_, Trail, _, _]).
'$thread_default'(stack(System), [_, _, System, _]).
'$thread_default'(detached(Detached), [_, _, _, Detached]).

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
	recorded('$thread_defaults', [_, Trail, System, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(trail(Trail), G) :-
	\+ integer(Trail), !,
	'$do_error'(type_error(integer, Trail), G).
'$thread_set_default'(trail(Trail), G) :-
	Trail < 0, !,
	'$do_error'(domain_error(not_less_than_zero, Trail), G).
'$thread_set_default'(trail(Trail), _) :- !,
	recorded('$thread_defaults', [Stack, _, System, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(system(System), G) :-
	\+ integer(System), !,
	'$do_error'(type_error(integer, System), G).
'$thread_set_default'(system(System), G0) :-
	System < 0, !,
	'$do_error'(domain_error(not_less_than_zero, System), G0).
'$thread_set_default'(system(System), _) :- !,
	recorded('$thread_defaults', [Stack, Trail, _, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(detached(Detached), G) :-
	Detached \== true, Detached \== false, !,
	'$do_error'(type_error(boolean, Detached), G).
'$thread_set_default'(detached(Detached), _) :- !,
	recorded('$thread_defaults', [Stack, Trail, System, _], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

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
	'$check_thread_or_alias'(Id, thread_cancel(Id)),
	'$thread_id_alias'(Id0, Id),
	'$unlock_all_thread_mutexes'(Id0),
	'$erase_thread_info'(Id0),
	'$thread_destroy'(Id0).	

thread_detach(Id) :-
	'$check_thread_or_alias'(Id, thread_detach(Id)),
	'$thread_id_alias'(Id0, Id),
	'$detach_thread'(Id0).

thread_exit(Term) :-
	var(Term), !,
	'$do_error'(instantiation_error, thread_exit(Term)).
thread_exit(Term) :-
	'$thread_self'(Id0),
	'$run_at_thread_exit'(Id0),
	recorda('$thread_exit_status', [Id0|exited(Term)], _),
	'$thread_exit'.

'$run_at_thread_exit'(Id0) :-
	findall(Hook, (recorded('$thread_exit_hook',[Id0|Hook],R), erase(R)), Hooks),
	'$run_thread_hooks'(Hooks),
	message_queue_destroy(Id0).

'$run_thread_hooks'([]).
'$run_thread_hooks'([Hook|Hooks]) :-
	'$thread_top_goal'(Hook),
	'$run_thread_hooks'(Hooks).

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
	recorded('$with_mutex_mutex',WMId,_),
	'$lock_mutex'(WMId),
	(	recorded('$mutex_alias',[Id|M],_) ->
		true
	;	'$new_mutex'(Id),
		recorda('$mutex_alias',[Id|M],_)
	),
	'$unlock_mutex'(WMId),
	'$lock_mutex'(Id),
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
'$mutex_property'(Id, locked(Thread, Count)) :-
	'$mutex_info'(Id, Count, HoldingThread),
	Count > 0,
	'$thread_id_alias'(HoldingThread, Alias),
	once((Thread = Alias; Thread = HoldingThread)).

message_queue_create(Cond) :-
	var(Cond), !,
	mutex_create(Mutex),
	'$cond_create'(Cond),
	'$mq_iname'(Cond, CName),
	recorda('$queue',q(Cond,Mutex,Cond,CName), _).
message_queue_create(Name) :-
	atom(Name),
	recorded('$thread_alias',[_,Name],_), !,
	'$do_error'(permission_error(create,message_queue,Name),message_queue_create(Name)).
message_queue_create(Name) :-
	atom(Name), !,
	'$create_mq'(Name).
message_queue_create(Name) :-
	'$do_error'(type_error(atom,Name),message_queue_create(Name)).

'$create_mq'(Name) :-
	mutex_create(Mutex),
	'$cond_create'(Cond),
	'$mq_iname'(Name, CName),
	recorda('$queue',q(Name,Mutex,Cond, CName),_).

'$mq_iname'(I,X) :-
	integer(I), !,
	number_codes(I,Codes),
	atom_codes(X, [0'$,0'M,0'Q,0'_|Codes]).
'$mq_iname'(A,X) :-
	atom_concat('$MQ_NAME_KEY_',A,X).

	
message_queue_destroy(Name) :-
	var(Name), !,
	'$do_error'(instantiation_error,message_queue_destroy(Name)).
message_queue_destroy(Queue) :-
	recorded('$queue',q(Queue,Mutex,Cond,CName),R), !,
	erase(R),
	'$cond_destroy'(Cond),
	mutex_destroy(Mutex),
	'$clean_mqueue'(CName).
message_queue_destroy(Queue) :-
	atom(Queue), !,
	'$do_error'(existence_error(message_queue,Queue),message_queue_destroy(Queue)).
message_queue_destroy(Name) :-
	'$do_error'(type_error(atom,Name),message_queue_destroy(Name)).

'$clean_mqueue'(Queue) :-
	recorded(Queue,_,R),
	erase(R),
	fail.
'$clean_mqueue'(_).

thread_send_message(Term) :-
	'$thread_self'(Id),
	thread_send_message(Id, Term).

thread_send_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_send_message(Queue,Term)).
thread_send_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_), !,
	thread_send_message(Id, Term).
thread_send_message(Queue, Term) :-
	recorded('$queue',q(Queue,Mutex,Cond,Key),_), !,
	mutex_lock(Mutex),
	recordz(Key,Term,_),
	'$cond_broadcast'(Cond),
	mutex_unlock(Mutex).
thread_send_message(Queue, Term) :-
	'$do_error'(existence_error(message_queue,Queue),thread_send_message(Queue,Term)).

thread_get_message(Term) :-
	'$thread_self'(Id),
	thread_get_message(Id, Term).

thread_get_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_get_message(Queue,Term)).
thread_get_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_), !,
	thread_get_message(Id, Term).
thread_get_message(Queue, Term) :-
	recorded('$queue',q(Queue,Mutex,Cond,Key),_), !,
	mutex_lock(Mutex),
	'$thread_get_message_loop'(Key, Term, Mutex, Cond).
thread_get_message(Queue, Term) :-
	'$do_error'(existence_error(message_queue,Queue),thread_get_message(Queue,Term)).


'$thread_get_message_loop'(Key, Term, Mutex, _) :-
	recorded(Key,Term,R), !,
	erase(R),
	mutex_unlock(Mutex).
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
	recorded('$queue',q(Queue,Mutex,_,Key),_), !,
	mutex_lock(Mutex),
	'$thread_peek_message2'(Key, Term, Mutex).
thread_peek_message(Queue, Term) :-
	'$do_error'(existence_error(message_queue,Queue),thread_peek_message(Queue,Term)).


'$thread_peek_message2'(Key, Term, Mutex) :-
	recorded(Key,Term,_), !,
	mutex_unlock(Mutex).
'$thread_peek_message2'(_, _, Mutex) :-
	mutex_unlock(Mutex),
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
	;	'$current_thread'(Id)
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
'$thread_property'(Id, stack(Stack)) :-
	recorded('$thread_sizes', [Id, Stack, _, _], _).
'$thread_property'(Id, trail(Trail)) :-
	recorded('$thread_sizes', [Id, _, Trail, _], _).
'$thread_property'(Id, system(System)) :-
	recorded('$thread_sizes', [Id, _, _, System], _).


'$current_thread'(Id) :-
	'$current_thread'(0, Id).

'$current_thread'(Id, Id) :-
	'$valid_thread'(Id).
'$current_thread'(Id, NextId) :-
	'$valid_thread'(Id),
	Id2 is Id + 1,
	'$current_thread'(Id2, NextId).


threads :-
	write('------------------------------------------------------------------------'), nl,
	format("~t~a~48+~n", 'Thread  Detached  Status'),
	write('------------------------------------------------------------------------'), nl,
	thread_property(Thread, detached(Detached)),
	thread_property(Thread, status(Status)),
	'$thread_id_alias'(Thread, Alias),
	format("~t~q~30+~33|~w~42|~q~n", [Alias, Detached, Status]),
	fail.
threads :-
	write('------------------------------------------------------------------------'), nl.


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
'$check_mutex_property'(locked(_, _), _) :- !.
'$check_mutex_property'(Term, Goal) :-
	'$do_error'(domain_error(mutex_property, Term), Goal).
