
:- object(heuristic_search(_),
	instantiates(class),
	specializes(search_strategy)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Heuristic state space search strategies.',
		parnames is ['Threshold']]).


	:- public(threshold/1).

	:- mode(threshold(?number), one).

	:- info(threshold/1,
		[comment is 'Search cost threshold.',
		 argnames is ['Threshold']]).


	:- public(solve/4).

	:- mode(solve(+object, +nonvar, -list, -number), zero_or_more).

	:- info(solve/4,
		[comment is 'State space search solution.',
		 argnames is ['Space', 'State', 'Path', 'Cost']]).


	:- protected(search/5).

	:- mode(search(+object, +nonvar, +number, -list, -number), zero_or_more).

	:- info(search/5,
		[comment is 'State space search solution.',
		 argnames is ['Space', 'State', 'Threshold', 'Path', 'Cost']]).


	solve(Space, State, Path) :-
		::solve(Space, State, Path, _).


	solve(Space, State, Path, Cost) :-
		::threshold(Threshold),
		::search(Space, State, Threshold, Path, Cost).


	threshold(Threshold) :-
		parameter(1, Threshold).


:- end_object.
