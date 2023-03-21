:- module(cuda, [cuda_extensional/2,
		 cuda_inline/2,
		 cuda_rule/2,
		 cuda_erase/1,
		 cuda_eval/2,
		 cuda_eval/3,
		 cuda_coverage/4,
		 cuda_statistics/0,
		 cuda_count/2,
		 cuda_query/1]).

tell_warning :-
	print_message(warning,functionality(cuda)).

:- dynamic inline__/2.

:- catch(load_foreign_files([],['YAPcuda'], init_cuda),_,fail) -> true ; tell_warning.

:- meta_predicate cuda_extensional(0,-).

cuda_inline(P, Q) :-
	assert(inline__(P,Q)).

cuda_extensional( Call, IdFacts) :-
	strip_module(Call, Mod, Name/Arity),
	functor(S, Name, Arity),
	findall(S,Mod:S,Facts),
	length(Facts, N),
	load_facts(N, Arity, Facts, IdFacts).

count_answers(G, N) :-
        S = count(0),
        ( 
	  G,
            arg(1, S, I0),
            I is I0+1,
            nb_setarg(1, S, I),
            fail ;
            S = count(N) ).
o
cuda_rule((Head :- Body) , IdRules) :-
	functor(Head, _Na, Ar),
	body_to_list( Body, L, [], 1, N, Ar, NAr   ),
	load_rule( N, NAr, [Head|L], IdRules ).


body_to_list( (B1, B2), LF, L0, N0, NF,As0, Asf) :- !,
	body_to_list( B1, LF, LI, N0, N1, As0, As1), 
	body_to_list( B2, LI, L0, N1, NF, As1, Asf). 
body_to_list( true, L, L, N, N, As,  As) :- !.
body_to_list( B, NL, L, N0, N, As0, Asf ) :-
	inline__( B, NB ), !,
	body_to_list( NB, NL, L, N0, N, As0, Asf).
body_to_list( B, [B|L], L, N0, N,As0,Asf) :-
	N is N0+1,
	functor(B,_,DA),
	Asf is DA+As0.


cuda_query(Call) :-
	cuda_init_query(Call).

cuda_eval(Q,Answer) :-
	cuda_eval(Q, Answer,0).
