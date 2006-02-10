
:- protocol(term_expansionp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/2/5,
		comment is 'Term expansion protocol.']).

	:- public(term_expansion/2).
	:- mode(term_expansion(?term, ?term), zero_or_more).
	:- info(term_expansion/2,
		[comment is 'Expands a term into a new term.',
		 argnames is ['Term', 'Expansion']]).

:- end_protocol.
