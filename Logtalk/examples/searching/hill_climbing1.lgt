
:- object(hill_climbing(Threshold),
	instantiates(heuristic_search(Threshold))).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2004/8/15,
		comment is 'Hill climbing heuristic state space search strategy.',
		parnames is ['Threshold']]).


	:- uses(list,
		[member/2, reverse/2, sort/2]).

	:- private(hill/7).


	search(Space, State, Threshold, Solution, Cost) :-
		hill(Space, State, Threshold, [], Path, 0, Cost),
		reverse(Path, Solution).


	hill(Space, State, _, Path, [State| Path], Cost, Cost) :-
		Space::goal_state(State).

	hill(Space, State, Threshold, Path, Solution, SoFar, Total) :-
		findall(
			(Estimate, Cost, Next),
			(Space::next_state(State, Next, Cost),
             \+ member(Next, [State| Path]),
             Space::heuristic(Next, Guess),
             Estimate is Guess + Cost),
			States),
		sort(States, SortedStates),
		member((_, Cost2, Next2), SortedStates),
		SoFar2 is SoFar + Cost2,
		SoFar2 =< Threshold,
		hill(Space, Next2, Threshold, [State| Path], Solution, SoFar2, Total).


:- end_object.
