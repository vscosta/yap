
:- object(circular,
	extends(salesman)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	route([Town| Towns], Route) :-
		route(Towns, Town~Town, Route).


	route([], Route, Route).

	route([Town| Towns], Route, Route2) :-
		best_place(Route, Town, Best),
		split(Best, Route, Town, Split),
		route(Towns, Split, Route2).


	best_place(Route, Town, Best) :-
		best_place(Route, Town, 10000000, 0, 0, Best).

	best_place(R~T1~T2, Town, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, Town, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(R~T1, Town, XT, I2, I2, Best).

	best_place(R~T1~T2, Town, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(R~T1, Town, XD, XI, I2, Best).

	best_place(T1~T2, Town, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, Town, T2, XT),
		XT < XD,
		Best is I + 1.

	best_place(_~_, _, _, XI, _, XI).


	split(0, Route, Town, Route~Town).

	split(IX, Route~Town1, Town, Split~Town1) :-
		IX2 is IX - 1,
		split(IX2, Route, Town, Split).

	split(1, Route, Town, Town~Route).


	extra(T1, T, T2, E) :-
		T1::crow_flies(T, E1),
		T::crow_flies(T2, E2),
		T1::crow_flies(T2, E3),
		E is E1 + E2 - E3.


:- end_object.
