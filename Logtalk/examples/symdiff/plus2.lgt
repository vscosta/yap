
:- object(_ + _,
	implements(symdiffp)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression1', 'Expression2'],
		comment is 'Symbolic differentiation and simplification of +/2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.']).


	diff(Diff) :-
		this(X + Y),
		once(diff(X, Y, Diff)).


	diff(I, J, 0) :-
		integer(I),
		integer(J).

	diff(X, J, DX) :-
		integer(J),
		X::diff(DX).

	diff(I, Y, DY) :-
		integer(I),
		Y::diff(DY).

	diff(X, Y, DX + DY) :-
		X::diff(DX),
		Y::diff(DY).


	simplify(S) :-
		this(X + Y),
		once(simplify(X, Y, S)).


	simplify(I, J, S) :-
		integer(I),
		integer(J),
		S is I + J.

	simplify(X, 0, S) :-
		X::simplify(S).

	simplify(0, Y, S) :-
		Y::simplify(S).

	simplify(X, J, S + J) :-
		integer(J),
		X::simplify(S).

	simplify(I, Y, I + S) :-
		integer(I),
		Y::simplify(S).

	simplify(X, Y, S) :-
		X::simplify(SX),
		Y::simplify(SY),
		(X + Y \= SX + SY ->
			(SX + SY)::simplify(S)
			;
			S = SX + SY).


:- end_object.
