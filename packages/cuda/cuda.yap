:- module(cuda, [cuda_extensional/2,
		 cuda_inline/2,
		 cuda_rule/2,
		 cuda_erase/1,
		 cuda_eval/2,
		 cuda_coverage/4,
		 cuda_count/2]).

tell_warning :-
	print_message(warning,functionality(cuda)).

:- dynamic inline/2.

:- catch(load_foreign_files([cuda], [], init_cuda),_,fail) -> true ; tell_warning.

:- meta_predicate cudda_extensional(:,-).

cuda_inline(P, Q) :-
	assert(inline(P,Q)).

cuda_extensional( Call, IdFacts) :-
	strip_module(Call, Mod, Name/Arity),
	functor(S, Name, Arity),
	findall( S, Mod:S, L),
	length( L, N ),
	load_facts( N, Arity, L, IdFacts ).

cuda_rule((Head :- Body) , IdRules) :-
	body_to_list( Body, L, [], 1, N),
	functor(Head, Na, Ar),
	load_rule( N, Ar, [Head|L], IdRules ).


body_to_list( (B1, B2), LF, L0, N0, NF) :- !,
	body_to_list( B1, LF, LI, N0, N1), 
	body_to_list( B2, LI, L0, N1, NF). 
body_to_list( true, L, L, N, N) :- !.
body_to_list( B, [NB|L], L, N0, N) :-
	inline( B, NB ), !,
	N is N0+1.
body_to_list( B, [B|L], L, N0, N) :-
	N is N0+1.

