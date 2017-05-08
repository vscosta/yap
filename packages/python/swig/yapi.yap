
%% @file yapi.yap
%% @brief support yap shell
%%
:- module(yapi, [bindvars/2]).

:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).

bindvars( [], [] ) :- !.
bindvars( L, NL ) :-
	rb_new(T),
	foldl2( bind, L, NL, T, _ , 0, _),
	term_variables(NL, Vs),
	foldl( bind_new, Vs, 0, _).


bind(X=Y, X=X, T0, T, N, N) :-
	var(Y),
	!,
	rb_update(T0, Y, X, T).
bind(X = G, X = G, T, T, N0, N0) :-
	ground(G),
	!.
bind(X = C, X = NC, T, NT, N0, NF) :-
 	C =.. [N|L],
	foldl2(newb, L, NL, T, NT, N0, NF),
	NC =.. [N|NL].

newb(Y, X, T, T, N, N) :-
	var(Y),
	rb_lookup(Y, X, T),
	!.
newb(Y, X, T, TN, N, NF) :-
	var(Y),
	!,
	rb_insert(Y, T, X, TN),
	NF is N+1,
	atomic_concat('_',N,X).
newb(Y, Y, T, T, N, N) :-
	ground(Y),
	!.
newb(Y, X, T, NT, N0, NF) :-
 	Y =.. [N|L],
	foldl2(newb, L, NL, T, NT, N0, NF),
	X =.. [N|NL].
