
:- object(quick(_Order)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		parnames is ['Order'],
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- public(sort/2).
	:- mode(sort(+list, -list), one).


	sort([], []).
	
	sort([X| L], S):-
		split(L, X, L1, L2),
		sort(L1, S1),
		sort(L2, S2),
		app(S1, [X| S2], S).


	split([], _, [], []).

	split([D| L], X, [D| L1], L2):-
		parameter(1, Order),
		Order::less(D, X),
		!,
		split(L, X, L1, L2).

	split([D| L], X, L1, [D| L2]):-
		split(L, X, L1, L2).


	app([], L, L).

	app([H| T], L, [H| T2]) :-
		app(T, L, T2).


:- end_object.
