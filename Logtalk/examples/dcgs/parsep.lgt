
:- protocol(parsep).


	:- info([
		version is 1.0,
		date is 2003/3/17,
		author is 'Paulo Moura',
		comment is 'Parse protocol for using DCG rules.']).


	:- public(parse/2).
	:- mode(parse(?list, ?nonvar), zero_or_more).

	:- info(parse/2, [
		comment is 'Parses a list using the defined set of DCG rules.',
		argnames is ['List', 'Result']]).


:- end_protocol.