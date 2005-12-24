:- module(list, [contained/2, notcontained/2]).

contained(H, [H| _]).
contained(H, [_| T]) :-
	contained(H, T).

notcontained(H, L) :-
	\+ contained(H, L).

