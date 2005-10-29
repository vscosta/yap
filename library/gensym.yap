:- module(gensym, [
	gensym/2,
	reset_gensym/1,
	reset_gensym/0
    ]).

:- dynamic gensym_key/2.

gensym(Atom, New) :-
	retract(gensym_key(Atom,Id)), !,
	atomic_concat(Atom,Id,New),
	NId is Id+1,
	assert(gensym_key(Atom,NId)).
gensym(Atom, New) :-
	atomic_concat(Atom,1,New),
	assert(gensym_key(Atom,2)).

reset_gensym(Atom) :-
	retract(gensym_key(Atom,_)).

reset_gensym :-
	retractall(gensym_key(_,_)).


