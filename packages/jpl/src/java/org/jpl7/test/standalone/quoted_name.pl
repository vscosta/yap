:- module(jpl).

quoted_name(Atom, Quoted) :-
    format(atom(Quoted), '~q', [Atom]).
