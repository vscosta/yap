
:- object(state_space,
	instantiates(class),
	specializes(object)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'State space description predicates.']).


	:- public(initial_state/1).

	:- mode(initial_state(?nonvar), one_or_more).

	:- info(initial_state/1,
		[comment is 'Initial state.',
		 argnames is ['State']]).


	:- public(initial_state/2).

	:- mode(initial_state(?atom, ?nonvar), zero_or_more).

	:- info(initial_state/2,
		[comment is 'Named initial state.',
		 argnames is ['Name', 'State']]).


	:- public(next_state/2).

	:- mode(next_state(+nonvar, -nonvar), zero_or_more).

	:- info(next_state/2,
		[comment is 'Generates a state sucessor.',
		 argnames is ['State', 'Next']]).


	:- public(goal_state/1).

	:- mode(goal_state(?nonvar), one_or_more).

	:- info(goal_state/1,
		[comment is 'Goal state.',
		 argnames is ['State']]).


	:- public(goal_state/2).

	:- mode(goal_state(?atom, ?nonvar), zero_or_more).

	:- info(goal_state/2,
		[comment is 'Named goal state.',
		 argnames is ['Name', 'State']]).


	:- public(print_state/1).

	:- mode(print_state(+nonvar), one).

	:- info(print_state/1,
		[comment is 'Pretty print state.',
		 argnames is ['State']]).


	:- public(print_path/1).

	:- mode(print_path(+list), one).

	:- info(print_path/1,
		[comment is 'Pretty print a path (list of states).',
		 argnames is ['Path']]).


	initial_state(State) :-
		::initial_state(_, State).


	goal_state(State) :-
		::goal_state(_, State).


	print_state(State) :-
		writeq(State), nl.


	print_path([]).

	print_path([State| States]) :-
		::print_state(State),
		print_path(States).


:- end_object.
