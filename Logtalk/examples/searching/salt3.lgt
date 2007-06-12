/*
	Salt state-space search problem

	2003 Portuguese National Logical Programming Contest problem
	http://paginas.fe.up.pt/~eol/LP/0304/documents/Exercicios_CNPL.PDF

Introduction:
	Mr Silva sells salt. He has to measure the quantity requested by his 
	customers by using two measures and an accumulator.  Neither has any 
	measuring markers. Those measures can easily be broken and he has to 
	replace them each time it happens.  More, a substitution can be made 
	by a measure with a different capacity than the one being replaced.

Objective:
	To produce a program, given the capacity of two measures and the 
	intended quantity, which helps Mr. Silva knowing if it is possible 
	to obtain the amount requested by his customer, and if so, measuring 
	the intended quantity in the least amount of steps.

Remarks:
	This problem is similar to the Water Jug's' problem. It is more general, 
	seeing that the Water Jug's problem uses static values for the jugs 
	capacities and the final goal.
*/


:- object(salt(_Acumulator, _Measure1, _Measure2),
	instantiates(state_space)).


	:- info([
		version is 1.0,
		author is 'Paula Marisa Sampaio',
		date is 2005/06/08,
		comment is 'Salt state-space search problem.']).


	% each state is represented by a compound term with four arguments: (Acumulator, Measure1, Measure2, Step)

	initial_state(initial, (0, 0, 0, all_empty)).


	% the intended salt quantity must end up on the acumulator

	goal_state(acumulator, (Acumulator, _, _, _)) :-
		parameter(1, Acumulator).


	% state transitions:


	% emptying a measure into the accumulator

	next_state((Acc, X, Y, _), (NewAcc, 0, Y, transfer(m1, acc))) :-
		X > 0,
		NewAcc is Acc + X.

	next_state((Acc, X, Y, _), (NewAcc, X, 0, transfer(m2, acc))) :-
		Y > 0,
		NewAcc is Acc + Y.
		

	% filling up of one of the measures

	next_state((Acc, X, Y, Step), (Acc, MaxX, Y, fill(m1))) :-
		parameter(2, MaxX),
		X < MaxX,
		Step \= empty(m1).

	next_state((Acc, X, Y, Step), (Acc, X, MaxY, fill(m2))) :-
		parameter(3, MaxY),
		Y < MaxY,
		Step \= empty(m2).


	% either pouring of a measure into the other till it is filled up
	% or all content of a measure into the other one

	next_state((Acc, X, Y, _), (Acc, W, Z, transfer(m2, m1))) :-
		parameter(2, MaxX),
		Y > 0,
		X < MaxX,
		(X + Y >= MaxX ->
			W = MaxX,
			Z is Y - (MaxX - X)
			;
			W is X + Y,
			Z = 0
		 ).

	next_state((Acc, X, Y, _), (Acc, W, Z, transfer(m1, m2))) :-
		parameter(3, MaxY),
		X > 0,
		Y < MaxY,
		(X + Y >= MaxY ->
			W is X - (MaxY - Y),
			Z = MaxY
			;
			W = 0,
			Z is X + Y
		 ).


	% throwing out the contents of a measure; does not afect the accumulator
 
	next_state((Acc, X, Y, Step), (Acc, 0, Y, empty(m1))) :-
		X > 0,
		Step \= fill(m1).
 
	next_state((Acc, X, Y, Step), (Acc, X, 0, empty(m2))) :-
		Y > 0,
		Step \= fill(m2).


	print_state((Acc, X, Y, Step)) :-
		write('('), write((Acc, X, Y)), write(')	'), write(Step), nl.


:- end_object.
