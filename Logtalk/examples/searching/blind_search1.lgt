
:- object(blind_search(_),
	instantiates(class),
	specializes(search_strategy)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Blind search state space strategies.',
		parnames is ['Bound']]).



	:- public(bound/1).

	:- mode(bound(?integer), zero_or_one).

	:- info(bound/1,
		[comment is 'Search depth bound.',
		 argnames is ['Bound']]).


	:- protected(search/4).

	:- mode(search(+object, +nonvar, +integer, -list), zero_or_more).

	:- info(search/4,
		[comment is 'State space search solution.',
		 argnames is ['Space', 'State', 'Bound', 'Path']]).


	bound(Bound) :-
		parameter(1, Bound).


	solve(Space, State, Path) :-
		::bound(Bound),
		::search(Space, State, Bound, Path).


:- end_object.
