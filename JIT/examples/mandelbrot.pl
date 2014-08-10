0% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/

% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(Height,H), Width = Height,

  format('P4~N~d ~d~N',[Height, Width]),
  pointsY(Height, Width, 0, 0, 0, 0, 0),

  statistics,
  statistics_jit.

% ------------------------------- %

pointsY(Height, Width, Y, X,
	OUTFLAG0,
	BYTEOUT0,
	BITN0) :-
	Y1 is Y + 1, Height >= Y1, !,
	pointsX(Height, Width, Y, 0,
		OUTFLAG0, OUTFLAG,
		BYTEOUT0, BYTEOUT,
		BITN0, BITN),
	pointsY(Height, Width, Y1, X,
		OUTFLAG,
		BYTEOUT,
		BITN).

pointsY(_, _, _, _, _, _, _) :- !.

% ------------- %

pointsX(Height, Width, Y, X,
	OUTFLAG0, OUTFLAG,
	BYTEOUT0, BYTEOUT,
	BITN0, BITN) :-

	X1 is X + 1, Width >= X1, !,

	(mandel(Height, Width, Y, X, 50) -> LimitAdj = 0 ; LimitAdj = 1),

	BITN1 is BITN0 + 1,
	(BITN1 == 8 -> OUTFLAG1 = 1 ; OUTFLAG1 = OUTFLAG0),

	BYTEOUT1 is BYTEOUT0 * 2 + LimitAdj,
	(
		(Width == X1, BITN1 \== 8) ->
		(BYTEOUT2 is BYTEOUT1 * integer(2 ** (8 - Width mod 8)), OUTFLAG2 = 1)
	;
		(BYTEOUT2 = BYTEOUT1, OUTFLAG2 = OUTFLAG1)
	),

	output(OUTFLAG2, OUTFLAG3, BYTEOUT2, BYTEOUT3, BITN1, BITN2),

	pointsX(Height, Width, Y, X1,
		OUTFLAG3, OUTFLAG,
		BYTEOUT3, BYTEOUT,
		BITN2, BITN).

pointsX(_, _, _, _, OUTFLAG, OUTFLAG, BYTEOUT, BYTEOUT, BITN, BITN) :- !.

% ------------- %

mandel(Height, Width, Y, X, Repetitions) :-
	Cr is (2.0 * X / Width - 1.5), Ci is (2.0 * Y / Height - 1.0),
	mandel_(Cr, Ci, 0.0, 0.0, Repetitions, 0).

mandel_(_, _, Zr, Zi, Repetitions, Repetitions) :- !,
	Limit is Zr * Zr + Zi * Zi, Limit > 4.0.

mandel_(Cr, Ci, Zr, Zi, Repetitions, N) :-
	Zr1 is Zr * Zr - Zi * Zi + Cr,
	Zi1 is 2.0 * Zr * Zi + Ci,
	Limit is Zr1 * Zr1 + Zi1 * Zi1,
	Limit =< 4.0, N1 is N + 1, !,
	mandel_(Cr, Ci, Zr1, Zi1, Repetitions, N1).

mandel_(_, _, _, _, _, _) :- !.

% ------------- %

output(OUTFLAG0, OUTFLAG, BYTEOUT0, BYTEOUT, BITN0, BITN) :-
(
	OUTFLAG0 =:= 1 ->
	(
		put_byte(BYTEOUT0),
		BITN = 0,
		BYTEOUT = 0,
		OUTFLAG = 0
	)
;
	(
		BYTEOUT = BYTEOUT0,
		BITN = BITN0,
		OUTFLAG = OUTFLAG0
	)
).

% ------------------------------- %
