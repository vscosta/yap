
:- object(_ ** _,
	implements(symdiffp)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression', 'Power'],
		comment is 'Symbolic differentiation and simplification of **/2 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.']).


	diff(Diff) :-
		this(X ** Y),
		once(diff(X, Y, Diff)).


	diff(X, Y, Y * X ** Y2 * DX) :-
		integer(Y),
		Y2 is Y - 1,
		X::diff(DX).

	diff(X, Y, Y * X ** Y2 * DX) :-
		Y2 = Y - 1,
		X::diff(DX).


	simplify(S) :-
		this(X ** Y),
		once(simplify(X, Y, S)).


	simplify(_, 0, 1).

	simplify(X, 1, X).

	simplify(X, Y, S ** Y) :-
		integer(Y),
		X::simplify(S).

	simplify(X, Y, SX ** SY) :-
		X::simplify(SX),
		Y::simplify(SY).


:- end_object.
