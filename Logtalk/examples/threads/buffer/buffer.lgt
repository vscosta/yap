
:- object(buffer(_MaxCapacity)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/6/20,
		comment is 'Producer-consumer problem with a bounded buffer.']).

	:- threaded.

	:- public(put/1).
	:- dynamic(put/1).
	:- mode(put(?integer), one).
	:- info(put/1, [
		comment is 'Put an item in the buffer.']).

	:- public(get/1).
	:- dynamic(get/1).
	:- mode(get(?integer), one).
	:- info(get/1, [
		comment is 'Get an item from the buffer.']).

	:- private(item_/1).
	:- dynamic(item_/1).

	:- private(size_/1).
	:- dynamic(size_/1).

	size_(0).

	put(Item) :-
		parameter(1, MaxCapacity),
		size_(N),
		(	N =:= MaxCapacity ->		% if the maximum buffer capacity have been
			threaded_wait(vacancy),		% reached, wait until an item is consumed
			put(Item)					% be sure to consume all "vacancy" notifications before proceeding
		;	put_item(Item),
			(	N =:= 0 ->
				threaded_notify(not_empty)
			;	true
			)
		).

	get(Item) :-
		parameter(1, MaxCapacity),
		size_(N),
		(	N =:= 0 ->					% if the buffer is empty, wait
			threaded_wait(not_empty),	% until an item is produced
			get(Item)					% be sure to consume all "not_empty" notifications before proceeding
		;	get_item(Item),
			(	N =:= MaxCapacity ->
				threaded_notify(vacancy)
			;	true
			)
		).

	:- synchronized([put_item/1, get_item/1]).

	put_item(Item) :-
		assertz(item_(Item)),
		retract(size_(N)),
		N2 is N + 1,
		assertz(size_(N2)),
		sender(Sender),
		writeq(Sender), write(' stored item '), write(Item), nl.

	get_item(Item) :-
		retract(item_(Item)),
		retract(size_(N)),
		N2 is N - 1,
		assertz(size_(N2)),
		sender(Sender),
		writeq(Sender), write(' consumed item '), write(Item), nl.

:- end_object.


:- object(producer(_MaxTime)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		parameter(1, MaxTime),
		random::random(1, MaxTime, Random),	% simulate a variable amount of 
		thread_sleep(Random),				% time to produce a new item
		buffer(7)::put(M),
		M2 is M + 1,
		run(M2, N).

:- end_object.


:- object(consumer(_MaxTime)).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		parameter(1, MaxTime),
		random::random(1, MaxTime, Random),	% simulate a variable amount of 
		thread_sleep(Random),				% time to produce a new item
		buffer(7)::get(_Item),
		M2 is M + 1,
		run(M2, N).

:- end_object.
