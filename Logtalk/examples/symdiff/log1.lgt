
:- object(log(_),
	implements(symdiffp)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 1999/12/29,
		parnames is ['Expression'],
		comment is 'Symbolic differentiation and simplification of log/1 expressions.',
		source is 'Example based on the Clocksin and Mellish Prolog book.']).


	diff(Diff) :-
		this(log(X)),
		once(diff(X, Diff)).


	diff(I, 0) :-
		integer(I).

	diff(X, DX * X ** -1) :-
		X::diff(DX).


	simplify(S) :-
		this(log(X)),
		once(simplify(X, S)).


	simplify(1, 0).

	simplify(I, Log) :-
		integer(I),
		Log is log(I).

	simplify(X, X).


:- end_object.
