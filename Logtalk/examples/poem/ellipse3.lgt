
/* Ellipses are defined by centre and semi-axes                         */

:- object(ellipse(_Center, _A, _B)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric ellipses.',
		parnames is ['Center', 'Rx', 'Ry'],
		source is 'Example adopted from the POEM system by Ben Staveley-Taylor.']).


	:- public(area/1).
	:- mode(area(-number), one).


	area(Area) :-
		pi(Pi),
		parameter(2, A),
		parameter(3, B),
		Area is Pi*A*B.


	pi(3.14196).


:- end_object.
