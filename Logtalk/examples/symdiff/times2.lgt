
:- object(_ * _,
	implements(symdiffp)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression1', 'Expression2'],
		comment is 'Symbolic differentiation and simplification of */2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.']).


	diff(Diff) :-
		this(X * Y),
		once(diff(X, Y, Diff)).


	diff(I, J, 0) :-
		integer(I),
		integer(J).

	diff(0, _, 0).

	diff(_, 0, 0).

	diff(X, J, J * DX) :-
		integer(J),
		X::diff(DX).

	diff(I, Y, I * DY) :-
		integer(I),
		Y::diff(DY).

	diff(X, Y, X * DY + DX * Y) :-
		X::diff(DX),
		Y::diff(DY).


	simplify(S) :-
		this(X * Y),
		once(simplify(X, Y, S)).


	simplify(I, J, S) :-
		integer(I),
		integer(J),
		S is I * J.

	simplify(0, _, 0).

	simplify(_, 0, 0).

	simplify(1, Y, SY) :-
		Y::simplify(SY).

	simplify(X, 1, SX) :-
		X::simplify(SX).

	simplify(I, Y, I * SY) :-
		integer(I),
		Y::simplify(SY).

	simplify(X, J, J * SX) :-
		integer(J),
		X::simplify(SX).

	simplify(X, Y, SX * SY) :-
		X::simplify(SX),
		Y::simplify(SY).


:- end_object.
