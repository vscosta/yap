
:- module(meld_interpreter,
	[
	 live/0,
	 delete/1,
	 push/1,
	 first/2,
	 min/3,
	 max/3,
	 sum/3,
	 minval/3,
	 maxval/3
	]).


:- use_module(meldp,
	[
	 run/1
	]).

:- use_module(meld).


:- use_module(library(nb),
	[
	 nb_queue/1,
	 nb_queue_enqueue/2,
	 nb_queue_dequeue/2,
	 nb_queue_replace/3
	]).


:- initialization
	init_meld_queue.

:- dynamic speculative_delete/3.

live :-
	repeat,
	( pop(Goal) ->
%	  format('<- ~w~n',[Goal]),
	  run(Goal),
	  fail
	;
	  !,
	  done
	).

done :-
	speculative_delete(_, _, _), !,
	push_residuals,
	live.
done :-
	current_predicate(meld_program:P),
	P \= run/1,
	P \= trace/2,
%	P \= neighbor/2,
%	P \= root/1,
	listing(meld_program:P),
	fail.
done.


delete(Fact) :-
	once(retract(meld_program:Fact)),
	nb_getval(meld_queue, Queue),
	(
%	 nb:nb_queue_show(Queue, L ), writeln(show:Fact:L),
	 nb_queue_replace(Queue, Fact, [] ),
%	 format('R ~w~n', [Fact]),
	 deleted(Fact)
	->
	 true
	;
	 nb_queue_enqueue(Queue, deleted(Fact))
	).

pop(Goal) :-
	nb_getval(meld_queue, Queue),
	nb_queue_dequeue(Queue, Goal).

push(Goal) :-
	clause(meld_program:Goal,_,Ref),
	!,
	increase_reference_count(Ref),
	fail.
push(Goal) :-
%	format('-> ~w~n',[Goal]),
	nb_getval(meld_queue, Queue), !,
	assert(meld_program:Goal),
	nb_queue_enqueue(Queue, Goal),
%	 nb:nb_queue_show(Queue, L ), writeln(show:Goal:L),
	fail.


% create a queue
init_meld_queue :-
	nb_queue(Queue),
	nb_setval(meld_queue, Queue).

first(Skel,Goal) :-
	meld_program:Skel, !,
	cache(Goal).
first(_,Goal) :-
	cache(Goal),
	push(Goal).

max(Skel,Arg,Goal) :-
	meld_program:Skel,
	arg(Arg, Skel, A0),
	arg(Arg, Goal, AN),
	AN =< A0, !,
	delete(Skel),
	cache(Goal).
max(Skel,_,Goal) :-
	clean(Skel),
	cache(Goal),
	push(Goal).

min(Skel,Arg,Goal) :-
	meld_program:Skel,
	arg(Arg, Skel, A0),
	arg(Arg, Goal, AN),
	AN >= A0, !,
	delete(Skel),
	cache(Goal).
min(Skel,_,Goal) :-
	clean(Skel),
	cache(Goal),
	push(Goal).

sum(Skel,Arg,Goal) :-
	copy_term(Skel, NGoal),
	meld_program:Skel, !,
	arg(Arg, Skel, A0),
	arg(Arg, Goal, A),
	AN is A0+A,
	AN \= A0,
	delete(Skel),
	arg(Arg, NGoal, AN),
%	format('S ~w~n',[NGoal-Skel]),
	push(NGoal).
sum(_Skel,_Arg,Goal) :-
%	format('S ~w~n',[Goal]),
	push(Goal).


clean(Skel) :-
%	format('D~w~n',[Skel]),
	retractall(meld_program:Skel).

cache(Goal) :-
	clause(meld_cache:Goal,_,Ref),
	!,
	increase_reference_count(Ref).
cache(Goal) :-
	assert(meld_cache:Goal).


deleted(Goal) :-
	clause(meld_program:Goal,_,Ref),
	decrease_reference_count(Ref),
	!,
	force_delete(Goal, Ref),
	complete_delete(Goal).
deleted(Goal) :-
	retract(speculative_delete(Goal, Ref, Count)), !,
	NCount is Count-1,
	(
	    NCount > 0 
	->
	   assert(speculative_delete(Goal, Ref, Count))
       ;
	   true
       ).
deleted(Goal) :-
%	format('-~w~n',[Goal]),
	complete_delete(Goal).

complete_delete(Goal) :-
	nb_getval(meld_queue, Queue), !,
	retract(meld_program:Goal),
	nb_queue_enqueue(Queue, deleted(Goal)).

force_delete(Goal, Ref) :-
	current_reference_count(Ref, Count),
	assert(speculative_delete(Goal, Ref, Count)).

push_residuals :-
	retract(speculative_delete(Goal, _, _)),
	push(Goal),
	fail.
push_residuals.


%
% first, cleanup cache
%
delete_from_first(_,Goal) :-
	clause(meld_program:Goal,_,Ref), !,
	(
	    decrease_reference_count(Ref)
	->
	    true
	;
	    force_delete(Goal, Ref)
	),
	erase(Ref),
	retract(meld_cache:Goal),
	retract(meld_program:Goal),
	push(deleted(Goal)),
	once(meld_cache:VGoal),
	push(VGoal).
delete_from_first(Goal) :-
	retract(speculative_delete(Goal, Ref, Count)), !,
	NCount is Count-1,
	(
	    NCount > 0 
	->
	   assert(speculative_delete(Goal, Ref, Count))
       ;
	   true
       ).
delete_from_first(Goal) :-
	retract(meld_cache:Goal),
	push(deleted(Goal)).


delete_from_max(VGoal,Arg,Goal) :-
	clause(meld_program:Goal,_,Ref), !,
	(
	 decrease_reference_count(Ref)
	->
	 true
        ;
	    force_delete(Goal, Ref)
	),
	erase(Ref),
	retract(meld_cache:Goal),
	push(deleted(Goal)),
	new_max(VGoal, Arg).
delete_from_max(Goal) :-
	retract(speculative_delete(Goal, Ref, Count)), !,
	NCount is Count-1,
	(
	    NCount > 0 
	->
	   assert(speculative_delete(Goal, Ref, Count))
       ;
	   true
       ).
delete_from_max(Goal) :-
	retract(meld_cache:Goal),
	push(deleted(Goal)).

delete_from_sum(Skel,Arg,Goal) :-
	copy_term(Skel, NGoal),
	once(meld_program:Skel),
	arg(Arg, Skel, A0),
	arg(Arg, Goal, A),
	AN is A0-A,
	AN \= A0,
	delete(Skel),
	arg(Arg, NGoal, AN),
	push(NGoal).

new_max(VGoal,Arg) :-
	arg(Arg, VGoal, A),
	maxval(A, meld_cache:VGoal, VGoal),
	push(VGoal).

delete_from_min(VGoal,Arg,Goal) :-
	clause(meld_program:Goal,_,Ref), !,
	(
	 decrease_reference_count(Ref)
	->
	 true
        ;
	 force_delete(Goal, Ref)
	),
	erase(Ref),
	retract(meld_cache:Goal),
	push(deleted(Goal)),
	new_min(VGoal, Arg).
delete_from_min(Goal) :- !,
	retract(speculative_delete(Goal, Ref, Count)),
	NCount is Count-1,
	(
	    NCount > 0 
	->
	   assert(speculative_delete(Goal, Ref, Count))
       ;
	   true
       ).
delete_from_min(Goal) :-
	retract(meld_cache:Goal),
	push(deleted(Goal)).

new_min(VGoal,Arg) :-
	arg(Arg, VGoal, A),
	minval(A, meld_cache:VGoal, VGoal),
	push(VGoal).

:- meta_predicate minval(+,:,-), maxval(+,:,-).

maxval(V,G,GMax) :-
	Memory = f(-inf,[]),
	(
	 call(G),
	 arg(1, Memory, V0),
	 V > V0,
	 nb_setarg(1, Memory, V),
	 nb_setarg(2, Memory, V.GMax),
	 fail
	 ;
	 arg(2, Memory, V.GMax)
	).

minval(V,G,GMin) :-
	Memory = f(+inf,[]),
	(
	 call(G),
	 arg(1, Memory, V0),
	 V < V0,
	 nb_setarg(1, Memory, V),
	 nb_setarg(2, Memory, V.GMin),
	 fail
	 ;
	 arg(2, Memory, V.GMin)
	).
