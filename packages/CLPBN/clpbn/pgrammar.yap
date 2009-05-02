:- source.

:- style_check(all).

:- module(clpbn_pgrammar,[grammar_prob/2,
			  grammar_mle/2]).

:- load_files([library(clpbn)],
	      [ if(not_loaded),
		silent(true)
	      ]).

:- use_module([library(lists)],
	      [ sum_list/2
	      ]).

:- op(600, xfy,'::').

:- dynamic id/4.

:- meta_predicate grammar_prob(:,-).

grammar_prob(M:S, P) :- !,
	grammar_prob(S, M, P).
grammar_prob(S, P) :-
	source_module(M),
	grammar_prob(S, M, P).

grammar_prob(S,M,P) :-
	nb_setval(grammar_fast,on),
	get_internal(S, InternalS, Proof),
	findall(P,path_prob(M:InternalS,Proof,P),Ps),
	nb_setval(grammar_fast,off),
	sum_list(Ps, P).

path_prob(InternalS,Proof,P) :-
	call(InternalS),
	extract_probability(Proof, P).

grammar_mle(M:S, P) :- !,
	grammar_mle(S, M, P).
grammar_mle(S, P) :-
	source_module(M),
	grammar_mle(S, M, P).

grammar_mle(S,M,P) :-
	nb_setval(grammar_fast,on),
	nb_setval(best,p(0.0,_)),
	get_internal(S, InternalS, Proof),
	call(M:InternalS),
	extract_probability(Proof, P),
	nb_getval(best,p(P0,_)),
	P > P0,
	nb_setval(best,p(P,S)),
	fail.
grammar_mle(S,_,P) :-
	nb_setval(grammar_fast,off),
	nb_getval(best,p(P,S)), P > 0.0.

user:term_expansion((P::H --> B), Goal) :-
        functor(H,A0,_),
        % a-->b to a(p(K,P,C,[Cs])) --> b(Cs)
        convert_to_internal(H, B, IH, IB, Id),
	expand_term((IH --> IB),(NH :- NB)),
	prolog_load_context(module, Mod),
	functor(NH,N,A),
	functor(EH,N,A),
	EH =.. [N|Args],
	build_rule_name(A0,NRuleName),
	EH1 =.. [NRuleName,Choice|Args],
	tail2(Args,LArgs),
	Key =.. [A0|LArgs],
	Args = [_|RArgs],
	H0 =.. [A0|RArgs],
	add_to_predicate(Mod:EH1,Mod:EH,Mod:H0,NH,NB,Key,Choice,P,Id,Goal).

add_to_predicate(M:EH1,M:EH,_,NH,NB,Key,Choice,P,Id,(EH1:-NB)) :-
	predicate_property(M:EH1,number_of_clauses(I)), !,
	Choice is I+1,
	assert_static(M:ptab(EH,Choice,P)),
	new_id(Key,P,Choice,Id),
	EH = NH.
add_to_predicate(M:EH1,M:EH,M:H0,NH,NB,Key,Choice,P,Id,(EH1:-NB)) :-
	% interface predicate
	assert_static(M:(H0 :- EH)),
	% now ensure_tabled works.
	ensure_tabled(M,H0,EH),
	assert_static(M:(EH :-
			clpbn_pgrammar:p_rule(M,EH,Key,Choice),
			 M:EH1)),
	Choice = 1,
	new_id(Key,P,Choice,Id),
	assert_static(M:ptab(EH,Choice,P)),
	EH=NH.

p_rule(_,_,_,_) :-
	nb_setval(grammar_fast,on), !.
p_rule(M,EH,Key,Choice) :-
	all_tabs(M,EH,Dom,Opt),
	{ Choice = Key with p(Dom,Opt) }.

ensure_tabled(M,H0,EH) :-
	predicate_property(M:H0, tabled), !,
	functor(EH,N,Ar),
	table(M:N/Ar).
ensure_tabled(_,_,_).

build_internal(N,NInternal) :-
	atom_concat(N,'__internal',NInternal).

build_rule_name(N,NRule) :-
	atom_concat(N,'__rule',NRule).

convert_to_internal(Head, Body, NH, NBody, Id) :-
	convert_body_to_internal(Body, NBody, LGoals, []),
	Head =.. [Na|Args],
	build_internal(Na,NaInternal),
	NH =.. [NaInternal,p(Id,LGoals)|Args].

convert_body_to_internal((B1,B2), (NB1,NB2)) -->
	!,
	convert_body_to_internal(B1,NB1),
	convert_body_to_internal(B2,NB2).
convert_body_to_internal([A], [A]) --> !.
convert_body_to_internal({A}, {A}) --> !.
convert_body_to_internal(B, IB) -->
	[V],
	{
	 B =.. [Na|Args],
	 build_internal(Na,NaInternal),
	 IB =.. [NaInternal,V|Args]
	}.

new_id(Key,P,Choice,Id) :-
	(
	 predicate_property(id(_,_,_,_),number_of_clauses(Id))
	->
	 true
	;
	 Id = 0
	),
	assert(id(Id,Key,P,Choice)).

all_tabs(M,EH,Dom,Ps) :-
	findall(P,M:ptab(EH,_,P),Ps),
	build_dom(Dom,1,Ps).

build_dom([],_,[]).
build_dom([I|Dom],I,[_|Ps]) :-
	I1 is I+1,
	build_dom(Dom,I1,Ps).

tail2([A,B],[A,B]) :- !.
tail2([_|Args],LArgs) :-
	tail2(Args,LArgs).


get_internal(S, InternalS, Arg) :-
	S =.. [N|Args],
	build_internal(N, NInternal),
	InternalS =.. [NInternal, Arg|Args].


extract_probability(p(Id,Goals), P) :-
	id(Id,_,P0,_),
	LogP0 is log(P0),	
	extract_logprobability(Goals, LogP0, LogP),
	P is exp(LogP).

extract_logprobability(p(Id, Goals), LogP) :-
	id(Id,_,P0,_),
	LogP0 is log(P0),
	extract_logprobability(Goals, LogP0, LogP).

extract_logprobability([], LogP, LogP).
extract_logprobability([P1|Ps], LogP0, LogP) :-
	extract_logprobability(P1, LogDP),
	LogPI is LogDP+LogP0,
	extract_logprobability(Ps, LogPI, LogP).

