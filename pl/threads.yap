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

/**
  @defgroup Threads Threads
  @ingroup extensions
  @{

  YAP implements a SWI-Prolog compatible multithreading
library. Like in SWI-Prolog, Prolog threads have their own stacks and
only share the Prolog <em>heap</em>: predicates, records, flags and other
global non-backtrackable data.  The package is based on the POSIX thread
standard (Butenhof:1997:PPT) used on most popular systems except
for MS-Windows.

*/

:- system_module( '$_threads', [current_mutex/3,
        current_thread/2,
        message_queue_create/1,
        message_queue_create/2,
        message_queue_destroy/1,
        message_queue_property/2,
        mutex_create/1,
        mutex_create/2,
        mutex_destroy/1,
        mutex_lock/1,
        mutex_property/2,
        mutex_trylock/1,
        mutex_unlock/1,
        mutex_unlock_all/0,
        thread_at_exit/1,
        thread_cancel/1,
        thread_create/1,
        thread_create/2,
        thread_create/3,
        thread_default/1,
        thread_defaults/1,
        thread_detach/1,
        thread_exit/1,
        thread_get_message/1,
        thread_get_message/2,
        thread_join/2,
        (thread_local)/1,
        thread_peek_message/1,
        thread_peek_message/2,
        thread_property/1,
        thread_property/2,
        thread_self/1,
        thread_send_message/1,
        thread_send_message/2,
        thread_set_default/1,
        thread_set_defaults/1,
        thread_signal/2,
        thread_sleep/1,
        threads/0,
        (volatile)/1,
        with_mutex/2], ['$reinit_thread0'/0,
        '$thread_gfetch'/1,
        '$thread_local'/2]).

:- use_system_module( '$_boot', [
        '$run_at_thread_start'/0,
        '$system_catch'/4]).

:- use_system_module( '$_errors', ['$do_error'/2]).


:- meta_predicate
	thread_initialization(0),
	thread_at_exit(0),
	thread_create(0, -, :),
	thread_create(0, -),
	thread_create(0),
	thread_signal(+, 0),
	with_mutex(+, 0),
	thread_signal(+,0),
	volatile(:).

volatile(P) :- var(P),
	throw(error(instantiation_error,volatile(P))).
volatile(M:P) :-
	'$do_volatile'(P,M).
volatile((G1,G2)) :-
	'$current_module'(M),
	'$do_volatile'(G1,M),
	'$do_volatile'(G2,M).
volatile(P) :-
	'$current_module'(M),
	'$do_volatile'(P,M).

'$do_volatile'(P,M) :- dynamic(M:P).

/** @defgroup Creating_and_Destroying_Prolog_Threads Creating and Destroying Prolog Threads
@ingroup Threads

@{
 */


:- initialization('$init_thread0').

'$init_thread0' :-
	recorda('$thread_alias', [0|main], _),
	fail.
'$init_thread0' :-
	'$no_threads', !.
'$init_thread0' :-
	recorda('$thread_defaults', [0, 0, 0, false, true], _).

'$reinit_thread0' :-
	'$no_threads', !.
'$reinit_thread0'.


'$top_thread_goal'(G, Detached) :-
	'$thread_self'(Id),
	(Detached == true -> '$detach_thread'(Id) ; true),
	'$current_module'(Module),
	'$run_at_thread_start',
	% always finish with a throw to make sure we clean stacks.
	'$system_catch'((G -> throw('$thread_finished'(true)) ; throw('$thread_finished'(false))),Module,Exception,'$close_thread'(Exception,Detached)),
	% force backtracking and handling exceptions
	fail.

'$close_thread'(Status, _Detached) :-
	'$thread_zombie_self'(Id0), !,
	'$record_thread_status'(Id0,Status),
	'$run_at_thread_exit'(Id0),
	'$erase_thread_info'(Id0).

% OK, we want to ensure atomicity here in case we get an exception while we
% are closing down the thread.
'$record_thread_status'(Id0,Stat) :- !,
	'$mk_tstatus_key'(Id0, Key),
	 (recorded(Key, _, R), erase(R), fail
	 ;
	  Stat = '$thread_finished'(Status) ->
	  recorda(Key, Status, _)
	 ;
	  recorda(Key, exception(Stat), _)
	 ).

/** @pred thread_create(: _Goal_)


Create a new Prolog detached thread using default options. See thread_create/3.

*/
thread_create(Goal) :-
	G0 = thread_create(Goal),
	must_be_callable(Goal),
	'$thread_options'([detached(true)], [], Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
%	'$erase_thread_info'(Id), % this should not be here
	(
	'$create_thread'(Goal, Stack, Trail, System, Detached, AtExit, Id)
	->
	 true
	;
	'$mk_tstatus_key'(Id, Key),
	 recorda(Key, exception(resource_error(memory)),_)
	).

/** @pred thread_create(: _Goal_, - _Id_)


Create a new Prolog thread using default options. See thread_create/3.


*/
thread_create(Goal, Id) :-
	G0 = thread_create(Goal, Id),
	must_be_callable(Goal),
	( nonvar(Id) -> '$do_error'(uninstantiation_error(Id),G0) ; true ),
	'$thread_options'([], [], Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
%	'$erase_thread_info'(Id), % this should not be here
	(
	 '$create_thread'(Goal, Stack, Trail, System, Detached, AtExit, Id)
	->
	 true
	;
	'$mk_tstatus_key'(Id, Key),
	 recorda(Key, exception(resource_error(memory)),_)
	).


/**
@pred thread_create(: _Goal_, - _Id_, + _Options_)

Create a new Prolog thread (and underlying C-thread) and start it
by executing  _Goal_.  If the thread is created successfully, the
thread-identifier of the created thread is unified to  _Id_.
 _Options_ is a list of options.  Currently defined options are:

+ stack
Set the limit in K-Bytes to which the Prolog stacks of
this thread may grow.  If omitted, the limit of the calling thread is
used.  See also the  commandline `-S` option.

+ trail
Set the limit in K-Bytes to which the trail stack of this thread may
grow.  If omitted, the limit of the calling thread is used. See also the
commandline option `-T`.

+ alias
Associate an alias-name with the thread.  This named may be used to
refer to the thread and remains valid until the thread is joined
(see thread_join/2).

+ at_exit
Define an exit hook for the thread.  This hook is called when the thread
terminates, no matter its exit status.

+ detached
If `false` (default), the thread can be waited for using
thread_join/2. thread_join/2 must be called on this thread
to reclaim the all resources associated to the thread. If `true`,
the system will reclaim all associated resources automatically after the
thread finishes. Please note that thread identifiers are freed for reuse
after a detached thread finishes or a normal thread has been joined.
See also thread_join/2 and thread_detach/1.


The  _Goal_ argument is <em>copied</em> to the new Prolog engine.
This implies further instantiation of this term in either thread does
not have consequences for the other thread: Prolog threads do not share
data from their stacks.
*/
thread_create(Goal, Id, Options) :-
	G0 = thread_create(Goal, Id, Options),
	must_be_callable(Goal),
	( nonvar(Id) -> '$do_error'(uninstantiation_error(Id),G0) ; true ),
	'$thread_options'(Options, Alias, Stack, Trail, System, Detached, AtExit, G0),
	'$thread_new_tid'(Id),
%	'$erase_thread_info'(Id), % this should not be here
	'$record_alias_info'(Id, Alias),
	(
	 '$create_thread'(Goal, Stack, Trail, System, Detached, AtExit, Id)
	->
	 true
	;
	'$mk_tstatus_key'(Id, Key),
	 recorda(Key, exception(resource_error(memory)),_)
	).

'$erase_thread_info'(Id) :-
	recorded('$thread_alias',[Id|_],R),
	erase(R),
	fail.
'$erase_thread_info'(Id) :-
	recorded('$thread_exit_hook', [Id|_], R),
	erase(R),
	fail.
'$erase_thread_info'(_).


'$thread_options'(Opts, Alias, Stack, Trail, System, Detached, AtExit, G) :-
	strip_module(Opts, Mod, LOpts),
	(
	 var(Opts)
	->
	 '$do_error'(instantiation_error,G)
	;
	 var(Mod)
	->
	 '$do_error'(instantiation_error,G)
	;
	 \+ atom(Mod)
	->
	 '$do_error'(uninstantiation_error(Mod),G)
	;
	 var(LOpts)
	->
	 '$do_error'(instantiation_error,G)
	;
	 '$thread_options'(LOpts, Alias, Stack, Trail, System, Detached, AtExit, Mod, G)
	).

'$thread_options'([], _, Stack, Trail, System, Detached, AtExit, _M, _) :-
	recorded('$thread_defaults', [DefaultStack, DefaultTrail, DefaultSystem, DefaultDetached, DefaultAtExit], _),
	( var(Stack) -> Stack = DefaultStack; true ),
	( var(Trail) -> Trail = DefaultTrail; true ),
	( var(System) -> System = DefaultSystem; true ),
	( var(Detached) -> Detached = DefaultDetached; true ),
	( var(AtExit) -> AtExit = DefaultAtExit; true ).
'$thread_options'([Opt|Opts], Alias, Stack, Trail, System, Detached, AtExit, M, G0) :-
	'$thread_option'(Opt, Alias, Stack, Trail, System, Detached, AtExit, M, G0),
	'$thread_options'(Opts, Alias, Stack, Trail, System, Detached, AtExit, M, G0).

'$thread_option'(Option, _, _, _, _, _, _, _, G0) :- var(Option), !,
	'$do_error'(instantiation_error,G0).
'$thread_option'(alias(Alias), Alias, _, _, _, _, _, _, G0) :- !,
	( \+ atom(Alias) -> '$do_error'(type_error(atom,Alias),G0) ; true ).
'$thread_option'(stack(Stack), _, Stack, _, _, _, _, _, G0) :- !,
	( \+ integer(Stack) -> '$do_error'(type_error(integer,Stack),G0) ; true ).
'$thread_option'(trail(Trail), _, _, Trail, _, _, _, _, G0) :- !,
	( \+ integer(Trail) -> '$do_error'(type_error(integer,Trail),G0) ; true ).
'$thread_option'(system(System), _, _, _, System, _, _, _, G0) :- !,
	( \+ integer(System) -> '$do_error'(type_error(integer,System),G0) ; true ).
'$thread_option'(detached(Detached), _, _, _, _, Detached, _, _, G0) :- !,
	( Detached \== true, Detached \== false -> '$do_error'(domain_error(thread_option,Detached+[true,false]),G0) ; true ).
'$thread_option'(at_exit(AtExit), _, _, _, _, _, AtExit, _M, G0) :- !,
	( \+ callable(AtExit) -> '$do_error'(type_error(callable,AtExit),G0) ; true ).
% succeed silently, like SWI.
'$thread_option'(_Option, _, _, _, _, _, _, _, _G0).
%	'$do_error'(domain_error(thread_option,Option),G0).

'$record_alias_info'(_, Alias) :-
	var(Alias), !.
'$record_alias_info'(_, Alias) :-
	recorded('$thread_alias', [_|Alias], _), !,
	'$do_error'(permission_error(create,thread,alias(Alias)), create_thread).
'$record_alias_info'(Id, Alias) :-
	recorda('$thread_alias', [Id|Alias], _).

% vsc: ?????
thread_defaults(Defaults) :-
	nonvar(Defaults), !,
	'$do_error'(uninstantiation_error(Defaults), thread_defaults(Defaults)).
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
'$thread_default'(system(System), [_, _, System, _, _]).
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

/** @pred thread_self(- _Id_)


Get the Prolog thread identifier of the running thread.  If the thread
has an alias, the alias-name is returned.


*/
thread_self(Id) :-
	nonvar(Id), \+ integer(Id), \+ atom(Id), !,
	'$do_error'(domain_error(thread_or_alias, Id), thread_self(Id)).
thread_self(Id) :-
	'$thread_self'(Id0),
	'$thread_id_alias'(Id0, Id).

/* Exit status may be either true, false, exception(Term), or exited(Term) */
/** @pred thread_join(+ _Id_, - _Status_)


Wait for the termination of thread with given  _Id_.  Then unify the
result-status of the thread with  _Status_.  After this call,
 _Id_ becomes invalid and all resources associated with the thread
are reclaimed.  Note that threads with the attribute `detached`
`true` cannot be joined.  See also current_thread/2.

A thread that has been completed without thread_join/2 being
called on it is partly reclaimed: the Prolog stacks are released and the
C-thread is destroyed. A small data-structure representing the
exit-status of the thread is retained until thread_join/2 is called on
the thread.  Defined values for  _Status_ are:

+ true
The goal has been proven successfully.

+ false
The goal has failed.

+ exception( _Term_)
The thread is terminated on an
exception.  See print_message/2 to turn system exceptions into
readable messages.

+ exited( _Term_)
The thread is terminated on thread_exit/1 using the argument  _Term_.


+ thread_detach(+ _Id_)


Switch thread into detached-state (see `detached` option at
thread_create/3 at runtime.   _Id_ is the identifier of the thread
placed in detached state.

One of the possible applications is to simplify debugging. Threads that
are created as `detached` leave no traces if they crash. For
not-detached threads the status can be inspected using
current_thread/2.  Threads nobody is waiting for may be created
normally and detach themselves just before completion.  This way they
leave no traces on normal completion and their reason for failure can be
inspected.


*/
thread_join(Id, Status) :-
	nonvar(Status), !,
	'$do_error'(uninstantiation_error(Status),thread_join(Id, Status)).
thread_join(Id, Status) :-
	'$check_thread_or_alias'(Id, thread_join(Id, Status)),
	'$thread_id_alias'(Id0, Id),
	'$thread_join'(Id0),
	'$mk_tstatus_key'(Id0, Key),
	recorded(Key, Status, R),
	erase(R),
	'$thread_destroy'(Id0).

thread_cancel(Id) :-
	(Id == main; Id == 0), !,
	'$do_error'(permission_error(cancel, thread, main), thread_cancel(Id)).
thread_cancel(Id) :-
	thread_signal(Id, throw(error(thread_cancel(Id),thread_cancel(Id)))).

thread_detach(Id) :-
	'$check_thread_or_alias'(Id, thread_detach(Id)),
	'$thread_id_alias'(Id0, Id),
	'$detach_thread'(Id0),
	'$mk_tstatus_key'(Id0, Key),
	(	recorded(Key, _, _) ->
		'$erase_thread_info'(Id0),
		'$thread_destroy'(Id0)
	;
		'$thread_unlock'(Id0)
	).

/** @pred thread_exit(+ _Term_)


Terminates the thread immediately, leaving `exited( _Term_)` as
result-state for thread_join/2.  If the thread has the attribute
`detached` `true` it terminates, but its exit status cannot be
retrieved using thread_join/2 making the value of  _Term_
irrelevant.  The Prolog stacks and C-thread are reclaimed.


*/
thread_exit(Term) :-
	var(Term), !,
	'$do_error'(instantiation_error, thread_exit(Term)).
thread_exit(Term) :-
	throw('$thread_finished'(exited(Term))).

'$run_at_thread_exit'(_Id0) :-
	'$thread_run_at_exit'(G, M),
	catch(once(M:G), _, fail),
	fail.
'$run_at_thread_exit'(Id0) :-
	recorded('$thread_exit_hook',[Id0|Hook],R), erase(R),
	catch(once(Hook),_,fail),
	fail.
'$run_at_thread_exit'(_).

/** @pred thread_at_exit(: _Term_)


Run  _Goal_ just before releasing the thread resources. This is to
be compared to `at_halt/1`, but only for the current
thread. These hooks are ran regardless of why the execution of the
thread has been completed. As these hooks are run, the return-code is
already available through thread_property/2 using the result of
thread_self/1 as thread-identifier. If you want to guarantee the
execution of an exit hook no matter how the thread terminates (the thread
can be aborted before reaching the thread_at_exit/1 call), consider
using instead the `at_exit/1` option of thread_create/3.


*/
thread_at_exit(Goal) :-
	must_be_callable(Goal),
	'$thread_self'(Id0),
	recordz('$thread_exit_hook',[Id0|Goal],_).

/**
@}
*/

/**
@defgroup Monitoring_Threads Monitoring Threads
@ingroup Threads


Normal multi-threaded applications should not need these the predicates
from this section because almost any usage of these predicates is
unsafe. For example checking the existence of a thread before signalling
it is of no use as it may vanish between the two calls. Catching
exceptions using catch/3 is the only safe way to deal with
thread-existence errors.

These predicates are provided for diagnosis and monitoring tasks.
@{
*/

/** @pred current_thread(+ _Id_, - _Status_)


Enumerates identifiers and status of all currently known threads.
Calling current_thread/2 does not influence any thread.  See also
thread_join/2.  For threads that have an alias-name, this name is
returned in  _Id_ instead of the numerical thread identifier.
 _Status_ is one of:

+ running
The thread is running.  This is the initial status of a thread.  Please
note that threads waiting for something are considered running too.

+ false
The  _Goal_ of the thread has been completed and failed.

+ true
The  _Goal_ of the thread has been completed and succeeded.

+ exited( _Term_)
The  _Goal_ of the thread has been terminated using thread_exit/1
with  _Term_ as argument.  If the underlying native thread has
exited (using pthread_exit())  _Term_ is unbound.

+ exception( _Term_)
The  _Goal_ of the thread has been terminated due to an uncaught
exception (see throw/1 and catch/3).



*/
current_thread(Id, Status) :-
	catch(thread_property(Id, status(Status)),
	  error(existence_error(_,_),_), fail).


'$thread_id_alias'(Id, Alias) :-
        recorded('$thread_alias', [Id|Alias], _), !.
'$thread_id_alias'(Id, Id).



thread_property(Prop) :-
	'$check_thread_property'(Prop, thread_property(Prop)),
	'$thread_self'(Id),
	'$thread_property'(Prop, Id).

/** @pred thread_property(? _Id_, ? _Property_)


Enumerates the properties of the specified thread.
Calling thread_property/2 does not influence any thread.  See also
thread_join/2.  For threads that have an alias-name, this name can
be used in  _Id_ instead of the numerical thread identifier.
 _Property_ is one of:

+ status( _Status_)
The thread status of a thread (see below).

+ alias( _Alias_)
The thread alias, if it exists.

+ at_exit( _AtExit_)
The thread exit hook, if defined (not available if the thread is already terminated).

+ detached( _Boolean_)
The detached state of the thread.

+ stack( _Size_)
The thread stack data-area size.

+ trail( _Size_)
The thread trail data-area size.

+ system( _Size_)
The thread system data-area size.



*/
thread_property(Id, Prop) :-
	(	nonvar(Id) ->
		'$check_thread_or_alias'(Id, thread_property(Id, Prop))
	;	'$enumerate_threads'(Id)
	),
	'$check_thread_property'(Prop, thread_property(Id, Prop)),
	'$thread_id_alias'(Id0, Id),
	'$thread_property'(Prop, Id0).

'$enumerate_threads'(Id) :-
	'$max_threads'(Max),
	Max1 is Max-1,
	between(0,Max1,Id),
	'$thread_stacks'(Id, _, _, _).

'$thread_property'(alias(Alias), Id) :-
	recorded('$thread_alias', [Id|Alias], _).
'$thread_property'(status(Status), Id) :-
	'$mk_tstatus_key'(Id, Key),
	(	recorded(Key, Exit, _) ->
		Status = Exit
	;	Status = running
	).
'$thread_property'(detached(Detached), Id) :-
	( '$thread_detached'(Id,Detached) -> true ; Detached = false ).
'$thread_property'(at_exit(M:G), _Id) :-
	'$thread_run_at_exit'(G,M).
'$thread_property'(stack(Stack), Id) :-
	'$thread_stacks'(Id, Stack, _, _).
'$thread_property'(trail(Trail), Id) :-
	'$thread_stacks'(Id, _, Trail, _).
'$thread_property'(system(System), Id) :-
	'$thread_stacks'(Id, _, _, System).

threads :-
	format(user_error,'------------------------------------------------------------------------~n',[]),
	format(user_error, '~t~a~48+~n', 'Thread  Detached  Status'),
	format(user_error,'------------------------------------------------------------------------~n',[]),
	thread_property(Id, detached(Detached)),
	thread_property(Id, status(Status)),
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

'$mk_tstatus_key'(Id0, Key) :-
	atomic_concat('$thread_exit_status__',Id0,Key).

/** @pred thread_statistics(+ _Id_, + _Key_, - _Value_)


Obtains statistical information on thread  _Id_ as `statistics/2`
does in single-threaded applications.  This call returns all keys
of `statistics/2`, although only information statistics about the
stacks and CPU time yield different values for each thread.

+ mutex_statistics


Print usage statistics on internal mutexes and mutexes associated
with dynamic predicates.  For each mutex two numbers are printed:
the number of times the mutex was acquired and the number of
collisions: the number times the calling thread has to
wait for the mutex.  The collision-count is not available on
Windows as this would break portability to Windows-95/98/ME or
significantly harm performance.  Generally collision count is
close to zero on single-CPU hardware.

+ threads


Prints a table of current threads and their status.



 */
thread_statistics(Id, Key, Val) :-
    format("not implemented yet: ~w, ~w, ~w~n",[Id, Key, Val]).

%% @}


/** @defgroup Signalling_Threads Signalling Threads
@ingroup Threadas


These predicates provide a mechanism to make another thread execute some
goal as an <em>interrupt</em>.  Signalling threads is safe as these
interrupts are only checked at safe points in the virtual machine.
Nevertheless, signalling in multi-threaded environments should be
handled with care as the receiving thread may hold a <em>mutex</em>
(see with_mutex/2).  Signalling probably only makes sense to start
debugging threads and to cancel no-longer-needed threads with throw/1,
where the receiving thread should be designed carefully do handle
exceptions at any point.

*/

/** @defgroup Thread_Synchronisation Thread Synchronisation
@ingroup Threads
@{

All
 internal Prolog operations are thread-safe. This implies two Prolog
threads can operate on the same dynamic predicate without corrupting the
consistency of the predicate. This section deals with user-level
<em>mutexes</em> (called <em>monitors</em> in ADA or
<em>critical-sections</em> by Microsoft).  A mutex is a
<em>MUT</em>ual <em>EX</em>clusive device, which implies at most one thread
can <em>hold</em> a mutex.

Mutexes are used to realise related updates to the Prolog database.
With `related', we refer to the situation where a `transaction' implies
two or more changes to the Prolog database.  For example, we have a
predicate `address/2`, representing the address of a person and we want
to change the address by retracting the old and asserting the new
address.  Between these two operations the database is invalid: this
person has either no address or two addresses, depending on the
assert/retract order.

Here is how to realise a correct update:

~~~~~
:- initialization
    mutex_create(addressbook).

change_address(Id, Address) :-
    mutex_lock(addressbook),
    retractall(address(Id, _)),
    asserta(address(Id, Address)),
    mutex_unlock(addressbook).
~~~~~
*/

/** @pred mutex_create(? _MutexId_)


  Create a mutex.  if  _MutexId_ is an atom, a <em>named</em> mutex is
  created.  If it is a variable, an anonymous mutex reference is returned.
  There is no limit to the number of mutexes that can be created.


*/
mutex_create(Id, Options) :-
	nonvar(Id), !,
	'$do_error'(uninstantiation_error(Id), mutex_create(Id, Options)).
mutex_create(Id, Options) :-
	Goal = mutex_create(Id, Options),
	'$mutex_options'(Options, Alias, Goal),
	(	atom(Alias) ->
		mutex_create(Alias)
	;	mutex_create(Id)
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

/** @pred mutex_unlock_all


Unlock all mutexes held by the current thread.  This call is especially
useful to handle thread-termination using abort/0 or exceptions.  See
also thread_signal/2.


*/
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

/** @pred current_mutex(? _MutexId_, ? _ThreadId_, ? _Count_)


Enumerates all existing mutexes.  If the mutex is held by some thread,
 _ThreadId_ is unified with the identifier of the holding thread and
 _Count_ with the recursive count of the mutex. Otherwise,
 _ThreadId_ is `[]` and  _Count_ is 0.



 */
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

%% @}

/** @defgroup Thread_Communication Thread communication
@ingroup Threads


Prolog threads can exchange data using dynamic predicates, database
records, and other globally shared data. These provide no suitable means
to wait for data or a condition as they can only be checked in an
expensive polling loop. <em>Message queues</em> provide a means for
threads to wait for data or conditions without using the CPU.

Each thread has a message-queue attached to it that is identified
by the thread. Additional queues are created using
`message_queue_create/2`.
@{
*/

message_queue_create(Id, Options) :-
	nonvar(Id), !,
	'$do_error'(uninstantiation_error(Id), message_queue_create(Id, Options)).
message_queue_create(Id, Options) :-
	var(Options), !,
	'$do_error'(instantiation_error, message_queue_create(Id, Options)).
message_queue_create(Id, []) :- !,
	'$message_queue_create'(Id).
message_queue_create(Id, [alias(Alias)]) :-
	var(Alias), !,
	'$do_error'(instantiation_error, message_queue_create(Id, [alias(Alias)])).
message_queue_create(Id, [alias(Alias)]) :-
	\+ atom(Alias), !,
	'$do_error'(type_error(atom,Alias), message_queue_create(Id, [alias(Alias)])).
message_queue_create(Id, [alias(Alias)]) :- var(Id), !,
	(	recorded('$thread_alias', [_|Alias], _) ->
		'$do_error'(permission_error(create,queue,alias(Alias)),message_queue_create(Alias, [alias(Alias)]))
	;	'$message_queue_create'(Id),
		recordz('$thread_alias', [Id|Alias], _)
	).
message_queue_create(Alias, [alias(Alias)]) :- !,
	(	recorded('$thread_alias', [_|Alias], _) ->
		'$do_error'(permission_error(create,queue,alias(Alias)),message_queue_create(Alias, [alias(Alias)]))
	;	'$message_queue_create'(Alias)
	).
message_queue_create(Id, [Option| _]) :-
	'$do_error'(domain_error(queue_option, Option), message_queue_create(Id, [Option| _])).
message_queue_create(Id, Options) :-
	'$do_error'(type_error(list, Options), message_queue_create(Id, Options)).

/** @pred message_queue_create(? _Queue_)


If  _Queue_ is an atom, create a named queue.  To avoid ambiguity
on `thread_send_message/2`, the name of a queue may not be in use
as a thread-name.  If  _Queue_ is unbound an anonymous queue is
created and  _Queue_ is unified to its identifier.


*/
message_queue_create(Id) :-
	(	var(Id) ->		% ISO DTR
		'$message_queue_create'(Id)
	;	atom(Id) ->		% old behavior
		'$message_queue_create'(Id)
	;	'$do_error'(uninstantiation_error(Id), message_queue_create(Id))
	).

/** @pred message_queue_destroy(+ _Queue_)


Destroy a message queue created with message_queue_create/1.  It is
<em>not</em> allows to destroy the queue of a thread.  Neither is it
allowed to destroy a queue other threads are waiting for or, for
anonymous message queues, may try to wait for later.


*/
message_queue_destroy(Name) :-
	var(Name), !,
	'$do_error'(instantiation_error,message_queue_destroy(Name)).
message_queue_destroy(Alias) :-
    recorded('$thread_alias', [Id|Alias], Ref),
    atom(Id), !,
    '$message_queue_destroy'(Id),
    erase(Ref).
message_queue_destroy(Name) :-
    atom(Name),
    '$message_queue_destroy'(Name),
    recorded('$thread_alias', [Name|_Alias], Ref),
    erase(Ref),
    fail.
message_queue_destroy(_).

/* @pred message_queue_property(+ _Queue_)


Report on the alias and number of messages stored in a queue created
with message_queue_create/1.

+ `alias(Alias)` report the alias for stream _S_. It can also be used
to enumerate all message queues that have aliases, including anonymous
queues.

+ `size(Size)` unifies _Size_ with the number of messages in the queue.
*/

message_queue_property( Id, alias(Alias) ) :-
    recorded('$thread_alias',[Id|Alias],_).
message_queue_property( Alias, size(Size) ) :-
    ground(Alias),
    recorded('$thread_alias',[Id|Alias],_),
    '$message_queue_size'(Id, Size).
message_queue_property( Id, size(Size) ) :-
    '$message_queue_size'(Id, Size).



/** @pred thread_send_message(+ _Term_)

Places  _Term_ in the message-queue of the thread running the goal.
Any term can be placed in a message queue, but note that the term is
copied to the receiving thread and variable-bindings are thus lost.
This call returns immediately.
*/
thread_send_message(Term) :-
	'$thread_self'(Id),
	thread_send_message(Id, Term).

/** @pred thread_send_message(+ _QueueOrThreadId_, + _Term_)

Place  _Term_ in the given queue or default queue of the indicated
thread (which can even be the message queue of itself (see
thread_self/1). Any term can be placed in a message queue, but note that
the term is copied to the receiving thread and variable-bindings are
thus lost. This call returns immediately.

If more than one thread is waiting for messages on the given queue and
at least one of these is waiting with a partially instantiated
 _Term_, the waiting threads are <em>all</em> sent a wakeup signal,
starting a rush for the available messages in the queue.  This behaviour
can seriously harm performance with many threads waiting on the same
queue as all-but-the-winner perform a useless scan of the queue. If
there is only one waiting thread or all waiting threads wait with an
unbound variable an arbitrary thread is restarted to scan the queue.

*/
thread_send_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_send_message(Queue,Term)).
thread_send_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_R), !,
	'$message_queue_send'(Id, Term).
thread_send_message(Queue, Term) :-
	'$message_queue_send'(Queue, Term).

/** @pred thread_get_message(? _Term_)


Examines the thread message-queue and if necessary blocks execution
until a term that unifies to  _Term_ arrives in the queue.  After
a term from the queue has been unified unified to  _Term_, the
term is deleted from the queue and this predicate returns.

Please note that not-unifying messages remain in the queue.  After
the following has been executed, thread 1 has the term `gnu`
in its queue and continues execution using  _A_ is `gnat`.

~~~~~
   <thread 1>
   thread_get_message(a(A)),

   <thread 2>
   thread_send_message(b(gnu)),
   thread_send_message(a(gnat)),
~~~~~

See also thread_peek_message/1.


*/
thread_get_message(Term) :-
	'$thread_self'(Id),
	thread_get_message(Id, Term).

/** @pred thread_get_message(+ _Queue_, ? _Term_)

As thread_get_message/1, operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.


*/
thread_get_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_get_message(Queue,Term)).
thread_get_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_R), !,
	'$message_queue_receive'(Id, Term).
thread_get_message(Queue, Term) :-
	'$message_queue_receive'(Queue, Term).


/** @pred thread_peek_message(? _Term_)


Examines the thread message-queue and compares the queued terms
with  _Term_ until one unifies or the end of the queue has been
reached.  In the first case the call succeeds (possibly instantiating
 _Term_.  If no term from the queue unifies this call fails.


*/
thread_peek_message(Term) :-
	'$thread_self'(Id),
	thread_peek_message(Id, Term).

/** @pred thread_peek_message(+ _Queue_, ? _Term_)

As thread_peek_message/1, operating on a given queue. It is allowed to
peek into another thread's message queue, an operation that can be used
to check whether a thread has swallowed a message sent to it.



Explicit message queues are designed with the <em>worker-pool</em> model
in mind, where multiple threads wait on a single queue and pick up the
first goal to execute.  Below is a simple implementation where the
workers execute arbitrary Prolog goals.  Note that this example provides
no means to tell when all work is done. This must be realised using
additional synchronisation.

~~~~~
%    create_workers(+Id, +N)
%
%    Create a pool with given Id and number of workers.

create_workers(Id, N) :-
    message_queue_create(Id),
    forall(between(1, N, _),
           thread_create(do_work(Id), _, [])).

do_work(Id) :-
    repeat,
      thread_get_message(Id, Goal),
      (   catch(Goal, E, print_message(error, E))
      ->  true
      ;   print_message(error, goal_failed(Goal, worker(Id)))
      ),
    fail.

%    work(+Id, +Goal)
%
%    Post work to be done by the pool

work(Id, Goal) :-
    thread_send_message(Id, Goal).
~~~~~


 */
thread_peek_message(Queue, Term) :- var(Queue), !,
	'$do_error'(instantiation_error,thread_peek_message(Queue,Term)).
thread_peek_message(Queue, Term) :-
	recorded('$thread_alias',[Id|Queue],_R), !,
	'$message_queue_peek'(Id, Term).
tthread_peek_message(Queue, Term) :-
	'$message_queue_peek'(Queue, Term).

%% @}

/** @defgroup Signalling_Threads Signalling Threads
@ingroup Threadas


These predicates provide a mechanism to make another thread execute some
goal as an <em>interrupt</em>.  Signalling threads is safe as these
interrupts are only checked at safe points in the virtual machine.
Nevertheless, signalling in multi-threaded environments should be
handled with care as the receiving thread may hold a <em>mutex</em>
(see with_mutex/2).  Signalling probably only makes sense to start
debugging threads and to cancel no-longer-needed threads with throw/1,
where the receiving thread should be designed carefully do handle
exceptions at any point.

@{
*/

/** @pred thread_sleep(+ _Time_)

th
Make current thread sleep for  _Time_ seconds.  _Time_ may be an
integer or a floating point number. When time is zero or a negative value
the call succeeds and returns immediately. This call should not be used if
alarms are also being used.



 */
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
	must_be_callable(Goal),
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

%% @}

/** @defgroup Threads_and_Dynamic_Predicates Threads and Dynamic Predicates
@ingroup Threads

@{

Besides queues threads can share and exchange data using dynamic
predicates. The multi-threaded version knows about two types of
dynamic predicates. By default, a predicate declared <em>dynamic</em>
(see dynamic/1) is shared by all threads. Each thread may
assert, retract and run the dynamic predicate. Synchronisation inside
Prolog guarantees the consistency of the predicate. Updates are
<em>logical</em>: visible clauses are not affected by assert/retract
after a query started on the predicate. In many cases primitive from
thread synchronisation should be used to ensure application invariants on
the predicate are maintained.

Besides shared predicates, dynamic predicates can be declared with the
thread_local/1 directive. Such predicates share their
attributes, but the clause-list is different in each thread.

*/

/** @pred thread_local( _+Functor/Arity_)


related to the dynamic/1 directive.  It tells the system that the
predicate may be modified using assert/1, retract/1,
etc, during execution of the program.  Unlike normal shared dynamic
data however each thread has its own clause-list for the predicate.
As a thread starts, this clause list is empty.  If there are still
clauses as the thread terminates these are automatically reclaimed by
the system.  The `thread_local` property implies
the property `dynamic`.

Thread-local dynamic predicates are intended for maintaining
thread-specific state or intermediate results of a computation.

It is not recommended to put clauses for a thread-local predicate into
a file as in the example below as the clause is only visible from the
thread that loaded the source-file.  All other threads start with an
empty clause-list.

~~~~~
:- thread_local
    foo/1.

foo(gnat).
~~~~~




 */
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
	(Mod \= idb -> '$predicate_flags'(T,Mod,F,F) ; true),
	( '$install_thread_local'(T,Mod) -> true ;
	   F /\ 0x08002000 =\= 0 -> '$do_error'(permission_error(modify,dynamic_procedure,A/N),thread_local(Mod:A/N)) ;
	   '$do_error'(permission_error(modify,static_procedure,A/N),thread_local(Mod:A/N))
	).
'$thread_local2'(X,Mod) :-
	'$do_error'(type_error(callable,X),thread_local(Mod:X)).



%% @}


/**
@}
*/
