
:- object(heuristic_state_space,
	instantiates(class),
	specializes(state_space)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Heuristic state space.']).


	:- public(next_state/3).

	:- mode(next_state(+nonvar, -nonvar, -number), zero_or_more).

	:- info(next_state/2,
		[comment is 'Generates a state sucessor.',
		 argnames is ['State', 'Next', 'Cost']]).


	:- public(heuristic/2).

	:- mode(heuristic(+nonvar, -number), one).

	:- info(heuristic/2,
		[comment is 'Estimates state distance to a goal state.',
		 argnames is ['State', 'Estimate']]).


	next_state(Prev, Next) :-
		::next_state(Prev, Next, _).


:- end_object.
