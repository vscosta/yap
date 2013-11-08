%
%
%
%

:- module(clpbn_evidence,
		[store_evidence/1,
		 incorporate_evidence/2,
		 check_stored_evidence/2,
		 add_stored_evidence/2,
		 put_evidence/2
		]).

:- use_module(library(clpbn)).

:- use_module(library('clpbn/dists'),
		[get_dist/4]).

:- use_module(library(rbtrees),
		[rb_new/1,
		 rb_lookup/3,
		 rb_insert/4
		]).

:- meta_predicate store_evidence(:).

:- dynamic node/3, edge/2, evidence/2.

%
% new evidence storage algorithm. The idea is that instead of
% redoing all the evidence every time we query the network, we shall
% keep a precompiled version around.
%
% the format is as follows:
%  evidence_store:parent(Key,ParentList,[EvidenceChildren])
%
%
store_evidence(G) :-
	clpbn_flag(solver,PreviousSolver, graphs),
	compute_evidence(G, PreviousSolver).

compute_evidence(G, PreviousSolver) :-
	catch(get_clpbn_vars(G, Vars), Ball, evidence_error(Ball,PreviousSolver)), !,
	store_graph(Vars), !,
	set_clpbn_flag(solver, PreviousSolver).
compute_evidence(_,PreviousSolver) :-
	set_clpbn_flag(solver, PreviousSolver).

get_clpbn_vars(G, Vars) :-
%	attributes:all_attvars(Vars0),
	once(G),
	attributes:all_attvars(Vars).

evidence_error(Ball,PreviousSolver) :-
	set_clpbn_flag(solver,PreviousSolver),
	throw(Ball).

store_graph([]).
store_graph([V|Vars]) :-
	clpbn:get_atts(V,[key(K),dist(Id,Vs)]),
	\+ node(K, Id, _), !,
	translate_vars(Vs,TVs),
	assert(node(K,Id,TVs)),
	( clpbn:get_atts(V,[evidence(Ev)]) -> assert(evidence(K,Ev)) ; true),
	add_links(TVs,K),
	store_graph(Vars).
store_graph([_|Vars]) :-
	store_graph(Vars).

translate_vars([],[]).
translate_vars([V|Vs],[K|Ks]) :-
	clpbn:get_atts(V, [key(K)]),
	translate_vars(Vs,Ks).

add_links([],_).
add_links([K0|TVs],K) :-
	edge(K,K0), !,
	add_links(TVs,K).
add_links([K0|TVs],K) :-
	assert(edge(K,K0)),
	add_links(TVs,K).

incorporate_evidence(Vs,AllVs) :-
	rb_new(Cache0),
	create_open_list(Vs, OL, FL, Cache0, CacheI),
	do_variables(OL, FL, CacheI),
	extract_vars(OL, AllVs).

create_open_list([], L, L, C, C).
create_open_list([V|Vs], [K-V|OL], FL, C0, CF) :-
	clpbn:get_atts(V,[key(K)]),
	add_stored_evidence(K, V),
	rb_insert(C0, K, V, CI),
	create_open_list(Vs, OL, FL, CI, CF).

do_variables([], [], _) :- !.
do_variables([K-V|Vs], Vf, C0) :-
	check_for_evidence(K, V, Vf, Vff, C0, Ci),
	do_variables(Vs, Vff, Ci).

extract_vars([], []).
extract_vars([_-V|Cache], [V|AllVs]) :-
	extract_vars(Cache, AllVs).

%make sure that we are consistent
check_stored_evidence(K, Ev) :-
	evidence(K, Ev0), !,
	Ev0 = Ev.
check_stored_evidence(_, _).

add_stored_evidence(K, V) :-
	evidence(K, Ev), !,
	put_evidence(Ev, V).
add_stored_evidence(_, _).

check_for_evidence(_, V, Vf, Vf, C, C) :-
	clpbn:get_atts(V, [evidence(_)]), !.
check_for_evidence(K, _, Vf0, Vff, C0, Ci) :-
	findall(Rt,edge(Rt,K),Rts),
	add_variables(Rts, _, Vf0, Vff, C0, Ci).

add_variables([], [], Vf, Vf, C, C).
add_variables([K|TVs], [V|NTVs], Vf0, Vff, C0, Cf) :-
	rb_lookup(K, V, C0), !,
	add_variables(TVs, NTVs, Vf0, Vff, C0, Cf).
add_variables([K|TVs], [V|NTVs], [K-V|Vf0], Vff, C0, Cf) :-
	rb_insert(C0, K, V, C1),
	create_new_variable(K, V, Vf0, Vf1, C1, C2),
	add_variables(TVs, NTVs, Vf1, Vff, C2, Cf).

create_new_variable(K, V, Vf0, Vff, C0, Cf) :-
	node(K, Id, TVs),
writeln(add:K:Id),
	get_dist(Id,_,Dom,CPT), !,
	{ V = K with p(Dom, CPT, NTVs) },
	add_stored_evidence(K, V),
	add_variables(TVs, NTVs, Vf0, Vff, C0, Cf).
create_new_variable(K, V, Vf0, Vff, C0, Cf) :-
	node(K, Id, TVs),
	Id =.. [Na,Dom],
	Dist =.. [Na,Dom,NTVs],
	{ V = K with Dist },
	add_stored_evidence(K, V),
	add_variables(TVs, NTVs, Vf0, Vff, C0, Cf).

put_evidence(Ev, V) :-
	clpbn:put_atts(V, [evidence(Ev)]).

