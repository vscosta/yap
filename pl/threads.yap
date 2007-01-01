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
	thread_at_exit(:),
	thread_signal(+,:),
	with_mutex(+,:).

:- initialization('$init_thread0').

'$init_thread0' :-
	no_threads, !.
'$init_thread0' :-
	'$create_mq'(0),
	'$add_thread_alias'(main, 0, '$init_thread0'),
	recorda('$thread_defaults', [0, 0, 0, false], _).

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

thread_create(Goal, Id) :-
	G0 = thread_create(Goal, Id),
	'$check_callable'(Goal, thread_create(Goal, Id)),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'([], [], Stack, Trail, System, Detached, G0),
	'$thread_new_tid'(Id),
	'$clean_db_on_id'(Id),
	'$create_mq'(Id),	
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id).

thread_create(Goal, Id, Options) :-
	G0 = thread_create(Goal, Id, Options),
	'$check_callable'(Goal,G0),
	( nonvar(Id) -> '$do_error'(type_error(variable,Id),G0) ; true ),
	'$thread_options'(Options, Alias, Stack, Trail, System, Detached, G0),
	'$thread_new_tid'(Id),
	'$clean_db_on_id'(Id),
	'$add_thread_alias'(Alias, Id, G0),
	'$create_mq'(Id),	
	'$create_thread'(Goal, Stack, Trail, System, Detached, Id).

'$clean_db_on_id'(Id) :-
	recorded('$thread_exit_status', [Id|_], R),
	erase(R),
	fail.
'$clean_db_on_id'(Id) :-
	recorded('$thread_alias',[Id|_],R),
	erase(R),
	fail.
'$clean_db_on_id'(Id) :-
	recorded('$thread_exit_hook',[Id|_],R),
	erase(R),
	fail.
'$clean_db_on_id'(_).


'$thread_options'(V, _, _, _, _, _, G) :- var(V), !,
	'$do_error'(instantiation_error,G).
'$thread_options'([], [], Stack, Trail, System, Detached, _) :-
	recorded('$thread_defaults', [DefaultStack, DefaultTrail, DefaultSystem, DefaultDetached], _),
	( var(Stack) -> Stack = DefaultStack; true ),
	( var(Trail) -> Trail = DefaultTrail; true ),
	( var(System) -> System = DefaultSystem; true ),
	( var(Detached) -> Detached = DefaultDetached; true ).
'$thread_options'([Opt|Opts], Alias, Stack, Trail, System, Detached, G0) :-
	'$thread_option'(Opt, Alias, Stack, Trail, System, Detached, G0),
	'$thread_options'(Opts, Alias, Stack, Trail, System, Detached, G0).

'$thread_option'(Option, Alias, _, _, _, _, G0) :- var(Option), !,
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

'$add_thread_alias'(Alias, Id, G) :-
	recorded('$thread_alias',[_|Alias],_), !,
	'$do_error'(permission_error(create,thread,alias(Alias)),G).
'$add_thread_alias'(Alias, Id, _) :-
	recorda('$thread_alias',[Id|Alias],_).

thread_defaults(Defaults) :- nonvar(Defaults), !,
	'$do_error'(type_error(variable,Id),thread_defaults(Defaults)).
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
'$thread_set_default'(stack(Stack), G) :- !,
	recorded('$thread_defaults', [_, Trail, System, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(trail(Trail), G) :-
	\+ integer(Trail), !,
	'$do_error'(type_error(integer, Trail), G).
'$thread_set_default'(trail(Trail), G) :-
	Trail < 0, !,
	'$do_error'(domain_error(not_less_than_zero, Trail), G).
'$thread_set_default'(trail(Trail), G) :- !,
	recorded('$thread_defaults', [Stack, _, System, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(system(System), G) :-
	\+ integer(System), !,
	'$do_error'(type_error(integer, System), G).
'$thread_set_default'(system(System), G0) :-
	System < 0, !,
	'$do_error'(domain_error(not_less_than_zero, System), G0).
'$thread_set_default'(system(System), G) :- !,
	recorded('$thread_defaults', [Stack, Trail, _, Detached], Ref),
	erase(Ref),
	recorda('$thread_defaults', [Stack, Trail, System, Detached], _).

'$thread_set_default'(detached(Detached), G) :-
	Detached \== true, Detached \== false, !,
	'$do_error'(type_error(boolean, Detached), G).
'$thread_set_default'(detached(Detached), G) :- !,
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
	'$check_thread_alias'(Id0,Id).

'$check_thread_alias'(Id0,Id) :-
	recorded('$thread_alias',[Id0|Id],_), !.
'$check_thread_alias'(Id,Id).

/* Exit status may be either true, false, exception(Term), or exited(Term) */
thread_join(Id, Status) :-
	var(Id), !,
	'$do_error'(instantiation_error, thread_join(Id, Status)).
thread_join(Id, Status) :-
	\+ integer(Id), \+ atom(Id), !,
	'$do_error'(domain_error(thread_or_alias, Id), thread_join(Id, Status)).
thread_join(Id, Status) :-
	integer(Id),
	\+ '$valid_thread'(Id),
	'$do_error'(existence_error(thread, Id), thread_join(Id, Status)).
thread_join(Id, Status) :-
	atom(Id),
	\+ recorded('$thread_alias',[_|Id],_),
	'$do_error'(existence_error(thread, Id), thread_join(Id, Status)).
thread_join(Id, Status) :-
	nonvar(Status), !,
	'$do_error'(type_error(variable,Status),thread_join(Id, Status)).
thread_join(Id, Status) :-
	'$check_thread_alias'(Id0,Id),
	'$thread_join'(Id0),
	'$erase_thread_aliases'(Id0),
	recorded('$thread_exit_status',[Id0|Status],R),
	erase(R),
	'$thread_destroy'(Id0).

'$erase_thread_aliases'(Id0) :-
	recorded('$thread_alias',[Id0|_],R),
	erase(R),
	fail.
'$erase_thread_aliases'(_).

thread_detach(Id) :-
	var(Id), !,
	'$do_error'(instantiation_error,thread_detach(Id)).
thread_detach(Id) :-
	\+ atom(Id),
	\+ integer(Id),
	'$do_error'(type_error(thread_or_alias, Id),thread_detach(Id)).
thread_detach(Id) :-
	atom(Id),
	recorded('$thread_alias',[Id0|Id],_),
	'$valid_thread'(Id0), !,
	'$detach_thread'(Id0).
thread_detach(Id) :-
	integer(Id),
	'$valid_thread'(Id), !,
	'$detach_thread'(Id0).
thread_detach(Id) :-
	'$do_error'(existence_error(thread, Id),thread_detach(Id)).

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

current_thread(Tid, Status) :-
	var(Tid), !,
	'$cur_threads'(0, Tid, Status).
current_thread(Tid, Status) :-
	( atom(Tid) ; integer(Tid) ), !,
	'$check_thread_alias'(Id0,Tid),
	'$valid_thread'(Id0),
	'$thr_status'(Id0, Status).
current_thread(Tid, Status) :-
	'$do_error'(type_error(integer,Tid),current_thread(Tid, Status)).

'$cur_threads'(Tid, TidName, Status) :-
	'$valid_thread'(Tid),
	'$thr_status'(Tid, Status),
	'$tid_to_alias'(Tid,TidName).
'$cur_threads'(Tid, TidF, Status) :-
	'$valid_thread'(Tid),
	Tid1 is Tid+1,
	'$cur_threads'(Tid1, TidF, Status).
	
'$thr_status'(Tid, Status) :-
	recorded('$thread_exit_status', [Tid|Status], _), !.
'$thr_status'(_, running).

'$tid_to_alias'(Tid,TidName) :-
	recorded('$thread_alias', [Tid|TidName], _), !.
'$tid_to_alias'(Tid,Tid).


mutex_create(V) :-
	var(V), !,
	'$new_mutex'(V),
	recorda('$mutex',[V|V],_).
mutex_create(A) :-
	atom(A),
	recorded('$mutex',[A|_],_), !,
	'$do_error'(permission_error(create,mutex,A),mutex_create(A)).
mutex_create(A) :-
	atom(A), !,
	'$new_mutex'(Id),
	recorda('$mutex',[A|Id],_).
mutex_create(V) :-
	'$do_error'(type_error(atom,V),mutex_create(V)).
	
mutex_destroy(V) :-
	var(V), !,
	'$do_error'(instantiation_error,mutex_destroy(V)).
mutex_destroy(A) :-
	recorded('$mutex',[A|Id],R), !,
	'$destroy_mutex'(Id),
	erase(R).
mutex_destroy(A) :-
	atom(A), !,
	'$do_error'(existence_error(mutex,A),mutex_destroy(A)).
mutex_destroy(V) :-
	'$do_error'(type_error(atom,V),mutex_destroy(V)).
	
mutex_lock(V) :-
	var(V), !,
	'$do_error'(instantiation_error,mutex_lock(V)).
mutex_lock(A) :-
	recorded('$mutex',[A|Id],_), !,
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
	recorded('$mutex',[A|Id],_), !,
	'$trylock_mutex'(Id).
mutex_trylock(A) :-
	atom(A), !,
	mutex_create(A),
	mutex_trylock(A).
mutex_trylock(V) :-
	'$do_error'(type_error(atom,V),mutex_trylock(V)).
	
mutex_unlock(V) :-
	var(V), !,
	'$do_error'(instantiation_error,mutex_unlock(V)).
mutex_unlock(A) :-
	recorded('$mutex',[A|Id],_), !,
	( '$unlock_mutex'(Id) ->
	    true
	;
	    '$do_error'(permission_error(unlock,mutex,A),mutex_unlock(A))
	).
mutex_unlock(A) :-
	atom(A), !,
	'$do_error'(existence_error(mutex,A),mutex_unlock(A)).
mutex_unlock(V) :-
	'$do_error'(type_error(atom,V),mutex_unlock(V)).

mutex_unlock_all :-
	'$thread_self'(T),
	recorded('$mutex',[_|Id],_),
	'$mutex_info'(Id, NRefs, T),
	NRefs > 0,
	'$mutex_unlock_all'(Id),
	fail.
mutex_unlock_all.

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
	\+ callable(G),
	'$do_error'(type_error(callable,G),with_mutex(M, G)).
with_mutex(M, G) :-
	atom(M), !,
	(	recorded('$mutex',[M|Id],_) ->
		true
	;	'$new_mutex'(Id),
		recorda('$mutex',[M|Id],_)
	),
	'$lock_mutex'(Id),
	call_cleanup(once(G), '$unlock_mutex'(Id)).
with_mutex(M, G) :-
	'$do_error'(type_error(atom,M),with_mutex(M, G)).

current_mutex(M, T, NRefs) :-
	recorded('$mutex',[M|Id],_),
	'$mutex_info'(Id, NRefs, T).

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


thread_signal(Thread, Goal) :-
	var(Thread), !,
	'$do_error'(instantiation_error,thread_signal(Thread, Goal)).
thread_signal(Thread, Goal) :-
	'$check_callable'(Goal,thread_signal(Thread,Goal)).
thread_signal(Thread, Goal) :-
	recorded('$thread_alias',[Id|Thread],_), !,
	'$thread_signal'(Id, Goal).
thread_signal(Thread, Goal) :-
	integer(Thread), !,
	'$thread_signal'(Thread, Goal).
thread_signal(Thread, Goal) :-
	'$do_error'(type_error(integer,Thread),thread_signal(Thread, Goal)).

'$thread_signal'(Thread, Goal) :-
	( recorded('$thread_signal',[Thread|_],R), erase(R), fail ; true ),
	recorda('$thread_signal',[Thread|Goal],_),
	'$signal_thread'(Thread).

'$thread_gfetch'(G) :-
	'$thread_self'(Id),
	recorded('$thread_signal',[Id|G],R),
	erase(R).


threads :-
	write('--------------------------------------------------------------'), nl,
	format("~t~a~38+~n", 'Thread  Status'),
	write('--------------------------------------------------------------'), nl,
	current_thread(Thread, Status),
	format("~t~q~30+  ~q~n", [Thread, Status]), 
	fail.
threads :-
	write('--------------------------------------------------------------'), nl.
