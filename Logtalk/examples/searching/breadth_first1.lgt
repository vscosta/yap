
:- object(breadth_first(Bound),
	instantiates(blind_search(Bound))).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/10/22,
		comment is 'Breadth first state space search strategy.',
		source is 'Example adopted from the book "Prolog Programming for Artificial Intelligence" by Ivan Bratko.',
		parnames is ['Bound']]).


	:- uses(list, [member/2, reverse/2]).


	search(Space, State, Bound, Solution) :-
		breadt(Space, l(State), Bound, Path),
		reverse(Path, Solution).


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
			\+ member(Next, [State| Path])),
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
