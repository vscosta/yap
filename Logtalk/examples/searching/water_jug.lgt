
:- object(water_jug,
	instantiates(state_space)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Water jug state space search problem.']).


	initial_state(start, (0, 0)).


	goal_state(end1, (2, 0)).

	goal_state(end2, (0, 2)).


	next_state((X, Y), (4, Y)) :-
		X < 4.

	next_state((X, Y),(X, 3)) :-
		Y < 3.

	next_state((X, Y), (4, Z)) :-
		Y > 0, X < 4,
		Aux is X + Y, Aux >= 4,
		Z is Y - (4 - X).

	next_state((X, Y), (Z, 3)) :-
		X > 0, Y < 3,
		Aux is X + Y, Aux >= 3,
		Z is X - (3 - Y).

	next_state((X, Y),(Z, 0)) :-
		Y > 0,
		Aux is X + Y, Aux =< 4,
		Z is Y + X.

	next_state((X, Y),(0, Z)) :-
		X > 0,
		Aux is X + Y, Aux =< 3,
		Z is Y + X.

	next_state((X, Y), (0, Y)) :-
		X > 0.

	next_state((X, Y), (X, 0)) :-
		Y > 0.


	print_state((X, Y)) :-
		write('4-gallon jug: '), write(X), nl,
		write('3-gallon jug: '), write(Y), nl, nl.


:- end_object.
