
:- object(tak).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/07/15,
		comment is 'Takeuchi function (recursive arithmetic).']).

	:- threaded.

	:- public(tak_st/4).
	:- mode(tak_st(+integer, +integer, +integer, -integer), one).
	:- info(tak_st/4, [
		comment is 'Single-threaded version of Takeuchi function.',
		argnames is ['X', 'Y', 'Z', 'R']]).

	:- public(tak_mt/4).
	:- mode(tak_mt(+integer, +integer, +integer, -integer), one).
	:- info(tak_mt/4, [
		comment is 'Multi-threaded version of Takeuchi function.',
		argnames is ['X', 'Y', 'Z', 'R']]).

	tak_st(X, Y, Z, A):-
		X =< Y,
		Z = A.
	tak_st(X, Y, Z, A):-
		X > Y,
		X1 is X - 1,
		tak_st(X1, Y, Z, A1),
		Y1 is Y - 1,
		tak_st(Y1, Z, X, A2),
		Z1 is Z - 1,
		tak_st(Z1, X, Y, A3),
		tak_st(A1, A2, A3, A).

	tak_mt(X, Y, Z, A):-
		X =< Y,
		Z = A.
	tak_mt(X, Y, Z, A):-
		X > Y,
		X1 is X - 1,
		Y1 is Y - 1,
		Z1 is Z - 1,
		threaded((
			tak_st(X1, Y, Z, A1),
			tak_st(Y1, Z, X, A2),
			tak_st(Z1, X, Y, A3)
		)),
		tak_mt(A1, A2, A3, A).

:- end_object.
