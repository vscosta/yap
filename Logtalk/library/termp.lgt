
:- protocol(termp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/4/3,
		comment is 'Prolog term utility predicates protocol.']).

	:- public(depth/2).
	:- mode(depth(@term, ?integer), zero_or_one).
	:- info(depth/2, [
		comment is 'True if the depth of Term is Depth. The depth of atomic terms is zero; the depth of a compound term is one plus the maximium depth of its sub-terms.',
		argnames is ['Term', 'Depth']]).

	:- public(ground/1).
	:- mode(ground(@term), zero_or_one).
	:- info(ground/1, [
		comment is 'True if the argument is ground.',
		argnames is ['Term']]).

	:- public(new/1).
	:- mode(new(-nonvar), zero_or_one).
	:- info(new/1, [
		comment is 'Creates a new term instance (if meaningful).',
		argnames is ['Term']]).

	:- public(occurs/2).
	:- mode(occurs(@var, @term), zero_or_one).
	:- info(occurs/2, [
		comment is 'True if the variable occurs in the term.',
		argnames is ['Variable', 'Term']]).

	:- public(subsumes/2).
	:- mode(subsumes(@term, @term), zero_or_one).
	:- info(subsumes/2, [
		comment is 'The first term subsumes the second term.',
		argnames is ['General', 'Specific']]).

	:- public(subterm/2).
	:- mode(subterm(?term, +term), zero_or_more).
	:- info(subterm/2, [
		comment is 'The first term is a subterm of the second term.',
		argnames is ['Subterm', 'Term']]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is valid.',
		argnames is ['Term']]).

	:- public(variant/2).
	:- mode(variant(@term, @term), zero_or_one).
	:- info(variant/2, [
		comment is 'Each term is a variant of the other (i.e. they are structurally equivalent).',
		argnames is ['Term1', 'Term2']]).

	:- public(vars/2).
	:- mode(vars(@term, -list), one).
	:- info(vars/2, [
		comment is 'Returns a list of all term variables.',
		argnames is ['Term', 'List']]).

:- end_protocol.
