
:- object(generator).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/03/22,
		comment is 'Simple object defining a predicate for generating lists of random values.']).

	:- public(list/2).

	list(0, []).
	list(N, [R| Rs]) :-
		N > 0,
		N2 is N - 1,
		random::random(R),
		list(N2, Rs).

:- end_object.
