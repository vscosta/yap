
:- object(double).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/2/16,
		comment is 'Contains a simple table of facts for testing operator handling code.']).

	:- public(double/2).

	:- op(500, xfx, double).


	1 double 2.
	2 double 4.
	3 double 6.


:- end_object.
