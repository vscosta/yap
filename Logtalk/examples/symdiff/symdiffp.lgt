
:- protocol(symdiffp).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		comment is 'Symbolic differentiation and simplification protocol.',
		source is 'Example based on the Clocksin and Mellish Prolog book.']).


	:- public(diff/1).

	:- mode(diff(-expression), one).

	:- info(diff/1, [
		comment is 'Returns the symbolic differentiation of self.',
		argnames is ['Expression']]).


	:- public(simplify/1).

	:- mode(simplify(-expression), one).

	:- info(simplify/1, [
		comment is 'Returns the symbolic simplification of self.',
		argnames is ['Expression']]).


:- end_protocol.
