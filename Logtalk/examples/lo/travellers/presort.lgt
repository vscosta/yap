
:- object(presort,
	extends(incremental)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adopted from the Francis G. McCabe L&O documentation.']).


	:- uses(quick(_)).


	route(Towns, Route) :-
		arrange(Towns, Towns2),
		^^route(Towns2, Route).


	arrange(Towns, Sorted) :-
		centre(Towns, X, Y),
    	quick(geographic(X, Y))::sort(Towns, Sorted).


	centre(Towns, X, Y) :-
		average(Towns, 0, 0, U, V, 0, L),
		X is U/L,
		Y is V/L.


	average([], U, V, U, V, L, L).

	average([Town| Towns], UX, VX, U, V, I, L):-
		Town::at(UT, VT),
		UX2 is UX+UT,
		VX2 is VX+VT,
		I2 is I + 1,
		average(Towns, UX2, VX2, U, V, I2, L).


:- end_object.
