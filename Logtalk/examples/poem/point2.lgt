
/* Points are going to be the fundamental quantity.                     */
/* These will be defined in Cartesian co-ordinates.                     */

:- object(point(_X, _Y)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric points.',
		parnames is ['X', 'Y'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).


	:- public(identical/1).
	:- mode(identical(+nonvar), one).

	:- public(distance/2).
	:- mode(distance(+nonvar, -number), one).


	identical(point(X1, Y1)) :-
		/* succeeds if the argument and owner points are the same.  */
		parameter(1, X),
		parameter(2, Y),
		X1 = X,
		Y1 = Y.


	distance(point(X1, Y1), Distance) :-
		/* finds the distance between argument and owner points.    */
		parameter(1, X),
		parameter(2, Y),
		Distance is sqrt((X1-X)*(X1-X)+(Y1-Y)*(Y1-Y)).


:- end_object.
