
:- object(buffer).

	:- threaded.

	:- public([put/1, get/1]).

	:- private(item/1).
	:- dynamic(item/1).

	put(N) :-
		(	N > 0										% wait until the previous item is consumed
		->	NP is N - 1, threaded_wait(consumed(NP))	% (except for the first item!) 
		;	true
		),
		assertz(item(N)),
		sender(Sender),
		writeq(Sender), write(' wrote item '), write(N), nl,
		threaded_notify(produced(N)).					% notify consumer that a new item is available

	get(N) :-
		threaded_wait(produced(N)),						% wait until an item is available
		retract(item(N)),
		sender(Sender),
		writeq(Sender), write(' read item '), write(N), nl,
		threaded_notify(consumed(N)).					% notify producer that the item was consumed

:- end_object.


:- object(producer).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		random::random(1, 5, Random),	% simulate a variable time to 
		thread_sleep(Random),			% produce a new item
		buffer::put(M),
		M2 is M + 1,
		run(M2, N).

:- end_object.


:- object(consumer).

	:- public(run/1).

	run(N) :-
		run(0, N).

	run(N, N) :- !.
	run(M, N) :-
		M < N,
		random::random(1, 5, Random),	% simulate a variable time
		thread_sleep(Random),			% to consume an item
		buffer::get(M),
		M2 is M + 1,
		run(M2, N).

:- end_object.
