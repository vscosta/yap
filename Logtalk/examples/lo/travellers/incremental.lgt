
:- object(incremental,
	extends(salesman)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- op(400, yfx, ~).


	route([Town| Towns], Route) :-
  		route(Towns, Town, Route).


	route([], Route, Route).

	route([Town| Towns], Route, Route2) :-
		best_place(Route, Town, Best),
		split(Best, Route, Town, NewR),
		route(Towns, NewR, Route2).


	best_place(Route~Town1, Town, Best) :-  % try the back first ...
		atom(Town1),
		Town::crow_flies(Town1, Distance),
		best_place(Route~Town1, Town, Distance, 0, 0, Best).

	best_place(Town, _, 0) :-
		atom(Town).


	best_place(R~T1~T2, T, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, T, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(R~T1, T, XT, I2, I2, Best).

	best_place(R~T1~T2, T, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(R~T1, T, XD, XI, I2, Best).

	best_place(T1~T2, T, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, T, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(T1, T, XT, I2, I2, Best).

	best_place(T1~T2, T, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(T1, T, XD, XI, I2, Best).

	best_place(T1, T, XD, _, I, Best) :-
		atom(T1),
		T1::crow_flies(T, Distance),
		Distance < XD,
		Best is I + 1.

	best_place(_, _, _, XI, _, XI).


	split(0, Route, Town, Route~Town).

	split(IX, Route~Town1, Town, S~Town1) :-
		IX2 is IX -1,
		split(IX2, Route, Town, S).

	split(1, Route, Town, Town~Route).


	extra(T1, T, T2, XT) :-
		T1::crow_flies(T, Distance1),
		T::crow_flies(T2, Distance2),
		T1::crow_flies(T2, Distance3),
		XT is Distance1 + Distance2 - Distance3.


:- end_object.
