
:- protocol(protop).


	:- info([
		version is 1.0,
		date is 2000/7/24,
		authors is 'Paulo Moura',
		comment is 'Default protocol for all prototypes.']).


	:- public(clone/1).

	:- mode(clone(?object), zero_or_one).

	:- info(clone/1, [
		comment is 'Clones a prototype.',
		argnames is ['Clone']]).


	:- public(print/0).

	:- mode(print, one).

	:- info(print/0, [
		comment is 'Pretty prints an object description.']).


:- end_protocol.
