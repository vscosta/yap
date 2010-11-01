
:- module(meld_interpreter,
	[
	 live/0,
	 delete/1,
	 push/1,
	 first/2,
	 min/3,
	 max/3
	]).


:- use_module(meldp,
	[
	 run/1
	]).


:- use_module(library(nb),
	[
	 nb_queue/1,
	 nb_queue_enqueue/2,
	 nb_queue_dequeue/2
	]).


:- initialization
	init_meld_queue.

live :-
	repeat,
	( pop(Goal) ->
	  format('-~w~n',[Goal]),
	  run(Goal),
	  fail
	;
	  !,
	  done
	).

done :-
	current_predicate(meld_program:P),
	P \= run/1,
%	P \= neighbor/2,
%	P \= root/1,
	listing(meld_program:P),
	fail.
done.


delete(Fact) :-
	nb_getval(meld_queue, Queue),
	retract(meld_program:Fact),
nb_queue_enqueue(Queue, deleted(Fact)),
	live.

pop(Goal) :-
	nb_getval(meld_queue, Queue),
	nb_queue_dequeue(Queue, Goal).

push(Goal) :-
	clause(meld_program:Goal,_,Ref),
	!,
	writeln(Goal+ref),
	increase_reference_count(Ref),
	fail.
push(Goal) :-
%	format('+~w~n',[Goal]),
	writeln(Goal+0),
	nb_getval(meld_queue, Queue), !,
	assert(meld_program:Goal),
	nb_queue_enqueue(Queue, Goal).


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
	cache(Goal).
min(Skel,_,Goal) :-
	clean(Skel),
	cache(Goal),
	push(Goal).

clean(Skel) :-
%	format('D~w~n',[Skel]),
	retractall(meld_program:Skel).

cache(Goal) :-
	writeln(cache(Goal)),fail.
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
	fail.
deleted(Goal) :-
%	format('-~w~n',[Goal]),
	nb_getval(meld_queue, Queue), !,
	retract(meld_program:Goal),
	nb_queue_enqueue(Queue, deleted(Goal)).



%
% first, cleanup cache
%
delete_from_first(_,Goal) :-
	clause(meld_cache:Goal,_,Ref),
	(
	 decrease_reference_count(Ref)
	->
	 fail
	;
	 erase(Ref),
	 fail
	).
delete_from_first(_,Goal) :-
	clause(meld_program:Goal,_,Ref),
	decrease_reference_count(Ref),
	!,
	fail.
delete_from_first(VGoal,Goal) :-
	retract(meld_program:Goal),
	push(deleted(Goal)),
	once(meld_cache:VGoal),
	push(VGoal).


delete_from_max(VGoal,Arg,Goal) :-
	clause(meld_cache:Goal,_,Ref),
	(
	 decrease_reference_count(Ref)
	->
	 clause(meld_program:Goal,_,CRef),
	 decrease_reference_count(CRef),
	 fail
	;
	 erase(Ref),
	 new_max(VGoal, Arg, Goal)
	).

new_max(VGoal,Arg,Goal) :-
%	format('-~w~n',[Goal]),
	retract(meld_program:Goal),
	push(deleted(Goal)),
	writeln(delete_from_max(VGoal,Arg,Goal)),
	maxval(Arg, meld_cache:VGoal, VGoal),
	push(VGoal).

delete_from_min(VGoal,Arg,Goal) :-
	clause(meld_cache:Goal,_,Ref),
	(
	 decrease_reference_count(Ref)
	->
	 clause(meld_program:Goal,_,CRef),
	 decrease_reference_count(CRef),
	 fail
	;
	 erase(Ref),
	 new_min(VGoal, Arg, Goal)
	).

new_min(VGoal,Arg,Goal) :-
%	format('-~w~n',[Goal]),
	retract(meld_program:Goal),
	push(deleted(Goal)),
	writeln(delete_from_min(VGoal,Arg,Goal)),
	minval(Arg, meld_cache:VGoal, VGoal),
	push(VGoal).

minval(_,_,_) :-
	nb_setval(min, +inf),
	nb_setval(min_arg, '$none'),
	fail.
minval(V,G,GMax) :-
	call(G),
	nb_getval(min, V0),
	V < V0,
	nb_setval(min, V),
	nb_setval(min_arg, V.GMax),
	fail.
minval(V,_,GMax) :-
	nb_getval(min_arg, V.GMax).
	

maxval(_,_,_) :-
	nb_setval(max, -inf),
	nb_setval(max_arg, '$none'),
	fail.
maxval(V,G,GMax) :-
	call(G),
	nb_getval(max, V0),
	V > V0,
	nb_setval(max, V),
	nb_setval(max_arg, V.GMax),
	fail.
maxval(V,_,GMax) :-
	nb_getval(max_arg, V.GMax).

