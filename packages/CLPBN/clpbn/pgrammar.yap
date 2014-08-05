:- source.

:- style_check(all).

:- module(clpbn_pgrammar,
		[grammar_to_atts/1,
		 grammar_prob/2,
		 grammar_mle/2,
		 init_pcg_solver/4,
		 run_pcg_solver/3,
		 pcg_init_graph/0
		]).

:- load_files(library(clpbn),
		[if(not_loaded), silent(true)]).

:- use_module(library(lists),
		[sum_list/2]).

:- use_module(library(matrix),
		[matrix_new/3,
		 matrix_add/3,
		 matrix_get/3,
		 matrix_op/4,
		 matrix_op_to_all/4,
		 matrix_set_all/2
		]).

:- op(600, xfy,'::').

:- dynamic id/4, dist_id/2, new_proof/2.

:- meta_predicate grammar_prob(:,-), grammar_mle(:,-), grammar_to_atts(:).

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
	nb_getval(grammar_fast,on), !.
p_rule(M,EH,Key,Choice) :-
	all_tabs(M,EH,Dom,Opt),
	{ AttVar = Key with p(Dom,Opt) },
	Choice = AttVar.

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

grammar_to_atts(M:S) :- !,
	grammar_to_atts(S, M).
grammar_to_atts(S) :-
	source_module(M),
	grammar_to_atts(S, M).

grammar_to_atts(S, M) :-
	nb_setval(grammar_fast,on),
	get_internal(S, InternalS, Proof),
	path_choices(M:InternalS,Proof).

path_choices(InternalS, Proof) :-
	new_id(Id),
	call(InternalS),
	/* use Ids because we may have repeated examples */
	assert(new_proof(Id,Proof)).

new_id(Id) :-
	(nb_getval(grammar_id,Id) ->
	  I1 is Id+1,
	  nb_setval(grammar_id,I1)
	;
	  nb_setval(grammar_id,1),
	  Id = 0
	).

find_dom(K, Vs, Ps) :-
	findall(V,id(_,K,_,V),Vs),
	gen_ps(Vs, Ps).

gen_ps([], []).
gen_ps([_|Vs], [1.0|Ps]) :-
	gen_ps(Vs, Ps).

init_pcg_solver(_, _, _, _).

run_pcg_solver(LVs, LPs, _) :-
	init_prob_array(Array, ExArray),
	add_proofs_to_array(Array, ExArray),
	matrix:matrix_to_list(Array,L), writeln(L),
	out_to_vs(LVs, LPs, Array).

add_proofs_to_array(Array, ExArray) :-
	nb_getval(grammar_id,IdMax),
	from(0,IdMax,Id),
	matrix_set_all(ExArray,0.0),
	sum_proofs(Id, ExArray),
%	matrix:matrix_to_list(ExArray,L0), writeln(Id:L0),
	matrix_op(ExArray, Array, +, Array),
%	matrix:matrix_to_list(Array,L0),writeln(i:L0),
	fail.
add_proofs_to_array(_,_).

from(I,_,I).
from(I0,Id,I) :-
	I1 is I0+1,
	I1 < Id,
	from(I1,Id,I).

sum_proofs(Id, ExArray) :-
	findall(P, add_proof(Id, ExArray, P), Ps),
	sum_list(Ps,TotP),
	matrix_op_to_all(ExArray,/,TotP,ExArray).

add_proof(Id, ExArray, P) :-
	new_proof(Id,Proof),
	extract_probability(Proof, P),
	add_to_array(Proof, P, ExArray).

add_to_array(p(Id, Goals), P, Array) :-
	add(Id, P, Array),
	add_to_array_goals(Goals, P, Array).

add_to_array_goals([], _, _).
add_to_array_goals([G|Goals], P, Array) :-
	add_to_array(G, P, Array),
	add_to_array_goals(Goals, P, Array).

init_prob_array(Array, ExArray) :-
	predicate_property(id(_,_,_,_),number_of_clauses(Cls)),
	matrix_new(floats, [Cls], Array),
	matrix_new(floats, [Cls], ExArray).

add(Id, P, Array) :-
	matrix_add(Array, [Id], P).

out_to_vs([], [], _).
out_to_vs([[V]|LVs], [Ps|LPs], Array) :-
	clpbn:get_atts(V, [key(Key)]),
	findall(P,count_for_key(Key,P,Array),Ps),
	out_to_vs(LVs, LPs, Array).

count_for_key(Key,P,Array) :-
	id(Id,Key,_,_),
	matrix_get(Array, [Id], P).

% generate attributed variables to make EM happy.
% just as few as possible
%
pcg_init_graph :-
	setof(K,I^P^S^id(I,K,P,S),Ks),
	generate_atts(Ks).

generate_atts([]).
generate_atts([Key|KVs]) :-
	find_dom(Key, Dom, Ps),
	{ _ = Key with p(Dom,Ps) },
	generate_atts(KVs).

