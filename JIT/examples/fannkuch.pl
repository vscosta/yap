% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/

% Contributed by Anthony Borla
% Modified by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- use_module(library(lists)).

:- initialization(main).

main :-
  unix( argv([H|_]) ), number_atom(N,H),

  f_permutations(N, MaxFlips),
  format('Pfannkuchen(~d) = ~d~n', [N, MaxFlips]),

  statistics,
  statistics_jit.

% ------------------------------- %

f_permutations(N, MaxFlips) :-
  numlist(1, N, L),
  f_permutations_(L, N, 0, 0, MaxFlips, 0, _).

% ------------- %

f_permutations_(L, N, I, MaxFlips0, MaxFlips, PermN0, PermN) :-
(I < N ->
	(
		N =:= 1 ->
		!, processPerm(L, MaxFlips0, MaxFlips, PermN0, PermN)
	;
		N1 is N - 1,
		f_permutations_(L, N1, 0, MaxFlips0, MaxFlips1, PermN0, PermN1),
		split_list(L, N, Lt, Ld),
		rotateLeft(Lt, LtRL), append(LtRL, Ld, La), Ii is I + 1,
		!, f_permutations_(La, N, Ii, MaxFlips1, MaxFlips, PermN1, PermN)
	)
;
	!, MaxFlips = MaxFlips0, PermN = PermN0
).

% ------------------------------- %

flips(L, Flips) :- flips_(L, 0, Flips).

flips_([1|_], Fla, Fla) :- !.

flips_([N|T], Fla, Flips) :-
	take_drop([N|T], N, Lt, Ld), append(Lt, Ld, La),
	Fla1 is Fla + 1, !, flips_(La, Fla1, Flips).

% ------------------------------- %

rotateLeft([H|T], RL) :- append(T, [H], RL).
rotateLeft([], []).

% ------------------------------- %

numlist(N, M, [N|Ls]) :- N < M, !, N1 is N + 1, numlist(N1, M, Ls).
numlist(M, M, [M]).

% ------------------------------- %

printPerm([L|Ls]) :- write(L), printPerm(Ls).
printPerm([]) :- nl.

% ------------------------------- %

processPerm(L, MaxFlips0, MaxFlips, PermN0, PermN) :-
	flips(L, Flips),
	(
		Flips > MaxFlips0 ->
		MaxFlips = Flips
	;
		MaxFlips = MaxFlips0
	),
	(
		PermN0 < 30 ->
		printPerm(L),
		PermN is PermN0 + 1
	;
		PermN = PermN0
	).

% ------------------------------- %

split_list([L|Ls], N, [L|Hs], Ts) :-
	N > 0, !, N1 is N - 1,
	split_list(Ls, N1, Hs, Ts).

split_list(Ls, 0, [], Ls) :- !.

% ------------------------------- %

take_drop(L, N, Taken, Rest) :- take_drop_(L, N, 0, [], Taken, Rest).

take_drop_(L, N, N, Ta, Ta, L) :- !.

take_drop_([H|T], N, Nc, Ta, Taken, Rest) :-
	Nc1 is Nc + 1, !, take_drop_(T, N, Nc1, [H|Ta], Taken, Rest).

% ------------------------------- %
