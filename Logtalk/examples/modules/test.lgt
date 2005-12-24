
:- module(test, [test/0, names/0]).

:- use_module(list, [contained/2]).
:- use_module(meta, [meta/1]).

names :-
	contained(P, [paulo, carlos, helena]),
	write(P), nl,
	fail.
names.

test :-
	meta(names).
