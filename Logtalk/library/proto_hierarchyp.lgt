
:- protocol(proto_hierarchyp,
	extends(hierarchyp)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Prototype hierarchy protocol.']).


	:- public(parent/1).

	:- mode(parent(?object), zero_or_more).

	:- info(parent/1, [
		comment is 'Returns, by backtracking, all object parents.',
		argnames is ['Parent']]).


	:- public(parents/1).

	:- mode(parents(-list), one).

	:- info(parents/1, [
		comment is 'List of all object parents.',
		argnames is ['Parents']]).


:- end_protocol.
