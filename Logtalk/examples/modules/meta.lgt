
:- module(meta, [meta/1]).

:- meta_predicate(meta(:)).

meta(Goal) :-
	call(Goal).
