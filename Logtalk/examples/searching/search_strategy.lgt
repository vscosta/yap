
:- object(search_strategy,
	instantiates(abstract_class),
	specializes(object)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'State space search strategies.']).


	:- public(solve/3).

	:- mode(solve(+object, +nonvar, -list), zero_or_more).

	:- info(solve/3,
		[comment is 'State space search solution.',
		 argnames is ['Space', 'State', 'Path']]).


:- end_object.
