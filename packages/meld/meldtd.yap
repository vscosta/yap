
:- module(meld_topdown,
	[
	 meld_top_down_compile/2,
	 meld_top_down_aggregate/3
	]).

:- use_module(meldi,
	[
	 maxval/3,
	 minval/3
	]).

:- use_module(meld).

:- dynamic extensional/3, translate/2.

meld_top_down_aggregate(S0, horn, _) :-
	functor(S0,Na,Arg),
	table(Na/Arg).
meld_top_down_aggregate(S0, max, Arg) :-
	functor(S0, Na, Arg), 
	functor(S, Na, Arg), 
	table(Na/Arg),
	meld_compiler:freshen(S, Arg, VHead),
	VHead =.. [Na|Args],
	atom_concat([Na,'__max'], NewName),
	NVHead =.. [NewName|Args],
	arg(Arg, NVHead, A),
	arg(Arg, S, MAX),
	assert_static((S :- maxval(A, NVHead, MAX))),
	assert(translate(Na,NewName)).
meld_top_down_aggregate(S0, min, Arg) :-
	functor(S0, Na, Arg), 
	functor(S, Na, Arg), 
	table(Na/Arg),
	meld_compiler:freshen(S, Arg, VHead),
	VHead =.. [Na|Args],
	atom_concat([Na,'__max'], NewName),
	NVHead =.. [NewName|Args],
	arg(Arg, NVHead, A),
	arg(Arg, S, MIN),
	assert_static((S :- minval(A, NVHead, MIN))),
	assert(translate(Na,NewName)).	
meld_top_down_aggregate(S0, first, _) :-
	functor(S0, Na, Arg), 
	functor(S, Na, Arg), 
	table(Na/Arg),
	S =.. [Na|Args],
	atom_concat([Na,'__max'], NewName),
	NS =.. [NewName|Args],
	assert_static((S :- once(NS))),
	assert(translate(Na,NewName)).	

meld_top_down_compile(Head, Body) :-
	compile_body(Body, NBody),
	compile_aggregate(Head, NHead),
	assert_static((NHead :- NBody)).

compile_body((G1,G2), (NG1, NG2)) :- !,
	compile_body(G1, NG1),
	compile_body(G2, NG2).
compile_body((forall G then B), (forall NG then NB)) :- !,
	compile_body(G, NG),
	compile_body(B, NB).
compile_body(G, meld_program:G) :-
	extensional(G,_,_), !.
compile_body(G, G).


compile_aggregate(Head, NewHead) :-
	Head =.. [Na|Args],
	translate(Na, NewNa), !,
	NewHead =.. [NewNa|Args].
compile_aggregate(Head, Head).
	

