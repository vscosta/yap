
:- object(math_constants).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Implements predicates for retriving common mathematical constants.']).


	:- public(pi/1).
	:- mode(pi(-float), one).

	:- public(e/1).
	:- mode(e(-float), one).


	pi(Pi) :-
		Pi is 4.0*atan(1.0).

	e(E) :-
		E is exp(1.0).


:- end_object.
