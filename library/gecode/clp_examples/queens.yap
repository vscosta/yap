
:- use_module(library(maplist)).
:- [ge_clpfd].

queens(N, Queens) :-
	length(Queens, N),
	Queens ins 1..9,
	all_distinct(Queens),
	lconstrain( Queens, 0),
	labeling([], Queens).

lconstrain([], _).
lconstrain( [Q|Queens], I0) :-
	I is I0+1,
	foldl(constrain(Q, I0), Queens, I, _),
	lconstrain( Queens, I).

constrain(Q, I, R, J, J1) :-
	J1 is J+1,
	Q + I #\= R + J,
	Q - I #\= R - J.
