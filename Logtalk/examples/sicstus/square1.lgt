
:- object(square(Side),
	extends(rectangle(Side, Side))).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric squares.',
		parnames is ['Side']]).


	:- public(side/1).

	:- mode(side(?number), zero_or_one).

	:- info(side/1, [
		comment is 'Square side.',
		argnames is ['Side']]).


	side(Side) :-
		parameter(1, Side).


	width(Width) :-
		parameter(1, Width).


	height(Height) :-
		parameter(1, Height).


:- end_object.
