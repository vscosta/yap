
:- object(breadth_first(Bound),
	instantiates(blind_search(Bound))).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Breadth first state space search strategy.',
		parnames is ['Bound']]).


	:- uses(list).


	search(Space, State, Bound, Solution) :-
		breadt(Space, l(State), Bound, Path),
		list::reverse(Path, Solution).


	breadt(Space, Tree, Bound, Solution) :-
		expand([], Tree, Tree2, Solved, Solution, Space, Bound),
		(Solved ->
			true
			;
			breadt(Space, Tree2, Bound, Solution)).


	expand(Path, l(State), _, true, [State| Path], Space, _) :-
		Space::goal_state(State).

	expand(Path, l(State), t(State, Subs), fail, _, Space, Bound) :-
		Bound > 0,
		bagof(l(Next),
			(Space::next_state(State, Next),
			\+ list::member(Next, [State| Path])),
			Subs).

	expand(Path, t(State,Subs), t(State, Subs2), Solved, Solution, Space, Bound) :-
		expandall([State| Path], Subs, [], Subs2, Solved, Solution, Space, Bound).


	expandall(_, [], [Tree| Trees], [Tree| Trees], fail, _, _, _).

	expandall(Path, [Tree| Trees], Trees2, Subs2, Solved, Solution, Space, Bound) :-
		Bound > 0,
		Bound2 is Bound -1,
		expand(Path, Tree, Tree2, Solved2, Solution, Space, Bound2),
		(Solved2 ->
			Solved = true
			;
			expandall(Path, Trees, [Tree2| Trees2], Subs2, Solved, Solution, Space, Bound))
		;
		expandall(Path, Trees, Trees2, Subs2, Solved, Solution, Space, Bound).


:- end_object.
