

:- module(clpbn, [{}/1).

:- use_module(library(atts)).
:- use_module(library(lists)).
:- use_module(library(terms)).

:- op( 500, xfy, with).

%
% avoid the overhead of using goal_expansion/2.
%
:- multifile
	user:term_expansion/2.

:- dynamic
	user:term_expansion/2.

:- multifile
	user:term_expansion/2.

:- attribute key/1, dist/1, evidence/1, done/1, starter/0.


:- use_module('clpbn/bnt', [dump_as_bnt/2,
		    check_if_bnt_done/1
		    ]).

:- use_module('clpn/vel', [vel/3,
		    check_if_vel_done/1
		    ]).

:- use_module('clpbn/evidence', [add_to_evidence/1,
			 execute_pre_evidence/0
		    ]).

use(vel).

{Var = Key with Dist} :-
%	key_entry(Key,Indx),
%	array_element(clpbn,Indx,El),
%	attributes:put_att(El,3,indx(Indx)),
	put_atts(El,[key(Key),dist(E=>Domain)]),
	extract_dist(Dist, E, Domain),
	add_evidence(Var,El).

extract_dist(V, (Tab.Inps), Domain) :- var(V), !,
	V = p(Domain, Tab, Inps).
extract_dist(p(Domain, Tab, []), Tab, Domain) :- !.
extract_dist(p(Domain, Tab, Inps), (Tab.Inps), Domain).
extract_dist(p(Domain, Tab), Tab, Domain).
	
key_entry(Key, I) :-
	hash_table_size(Size),
	term_hash(Key, -1, Size, Hash),
	collision(Hash, Size, I),
	( array_element(keys, I, El) ->
	    update_array(keys, I, Key)
	;
	    El = Key),
 	!.

% go from beginning
collision(Size, Size, I) :- !,
	collision(0, Size, I).
collision(Hash, _, Hash).
collision(Hash, Size, I) :-
	Hash1 is Hash+1,
	collision(Hash1, Size, I).

%
% just fetch skolems so that we can process them carefully.
%
fetch_skolems(A, A) --> { var(A) }, !. %meta-calls
fetch_skolems((A,B), (NA,NB)) --> !,
	fetch_skolems(A, NA),
	fetch_skolems(B, NB).
% do not allow disjunctive clauses, at least for now.
fetch_skolems((A;B), (A;B)) --> !.
fetch_skolems((A|B), (A|B)) --> !.
fetch_skolems((A->B), (NA->NB)) --> !,
	fetch_skolems(A, NA),
	fetch_skolems(B, NB).
fetch_skolems(M:A, M:NA) --> !,
	fetch_skolems(A, NA).
fetch_skolems(X = { Constraints }, true)  --> !,
	[ [X|Constraints] ].
fetch_skolems(G, G) --> [].

%
% just fetch skolems so that we can process them carefully.
%
handle_body_goals((A,B), (NA,NB)) :- !,
	handle_body_goals(A, NA),
	handle_body_goals(B, NB).
% do not allow disjunctive clauses, at least for now.
handle_body_goals((A;B), (A;B)) :- !.
handle_body_goals((A|B), (A|B)) :- !.
handle_body_goals((A->B), (NA->NB)) :- !,
	handle_body_goals(A, NA),
	handle_body_goals(B, NB).
handle_body_goals(M:A, M:NA) :- !,
	handle_body_goals(A, NA).
handle_body_goals(findall(V,G,L), (findall(V,G,L), aggs:fix_vars(L))) :- !.
handle_body_goals(setof(V,G,L), (setof(V,G,L),aggs:fix_vars(L))) :- !.
handle_body_goals(bagof(V,G,L), (bagof(V,G,L),aggs:fix_vars(L))) :- !.
handle_body_goals(G, G).


compile_skolems([[X|Constraints]], Vars, NVars, A, Code) :- !,
	compile_skolem(X, Vars, NVars, A, Code, Constraints).
compile_skolems([[X|Constraints]|Cs], Vars, NVars, A, (Code, RCode)) :-
	compile_skolem(X, Vars, NVars, A, Code, Constraints),
	compile_skolems(Cs, Vars, NVars, A, RCode).

compile_skolem(EVar, Vars, NVars, Head, Code, Constraints) :-
	compile_constraints(Constraints, Vars, NVars, Head, Code, EVar).

compile_constraints((A : B), Vars, NVars, Head, (CA , CB), EVar) :- !,
	compile_first_constraint(A, Head, CA, EVar),
	compile_second_constraint(B, Vars, NVars, CB, EVar).

compile_first_constraint(SkKey, Head, (KeyGoal, /* cycle(Key,EVar), */ array_element(clpbn, Id, EVar), clpbn:put_atts(EVar,[key(KeyDesc),indx(Id)])), EVar) :-
	functor(SkKey, Name, _),!,
	SkKey =..  [_|Key],
	generate_key_goal(Head, Name, Key, KeyGoal, KeyDesc, Id).

compile_second_constraint(Constraint, Vars, NVars, clpbn:put_atts(EVar,[dist(NC)]), EVar) :-
	check_constraint(Constraint, Vars, NVars, NC).

check_constraint(Constraint, _, _, Constraint) :- var(Constraint), !.
check_constraint((A=>D), _, _, (A=>D)) :- var(A), !.
check_constraint((([A|B].L)=>D), Vars, NVars, (([A|B].NL)=>D)) :- !,
	check_cpt_input_vars(L, Vars, NVars, NL).
check_constraint(Dist, _, _, Dist).

check_cpt_input_vars([], _, _, []).
check_cpt_input_vars([V|L], Vars, NVars, [NV|NL]) :-
	replace_var(Vars, V, NVars, NV),
	check_cpt_input_vars(L, Vars, NVars, NL).

replace_var([], V, [], V).
replace_var([V|_], V0, [NV|_], NV) :- V == V0, !.
replace_var([_|Vars], V, [_|NVars], NV) :-
	replace_var(Vars, V, NVars, NV).

generate_key_goal(Head, Name, Key, clpbn_aux:KeyGoal, KeyDesc, Id) :-
	functor(Head, _, Ar),
	atom_chars(Name, NaS),
	number_codes(Ar, ArS),
	append("/", ArS, S1),
	append(NaS, S1, S2),
	append("key_", S2, SNNa),
	atom_chars(NNa, SNNa),
	append(Key,[Id],LArgs),
	KeyGoal =.. [NNa|LArgs],
	KeyDesc =.. [Name|Key],
	length(Key, N),
	NAr is N+1,
	abolish(clpbn_aux:(NNa/NAr)),
	reset_clpbn,
	dynamic_predicate(clpbn_aux:(NNa/NAr), logical),
	assert((clpbn_aux:KeyGoal :- new_key(Id), asserta(clpbn_aux:(KeyGoal :- !)))).

new_key(Key) :-
	get_value(clpbn_key, Key),
	X is Key+1,
	set_value(clpbn_key, X).

skolem_vars([], []).
skolem_vars([[V|_]|LV], [V|NLV]) :-
	skolem_vars(LV, NLV).


skolem_new_vars([], [], []).
skolem_new_vars([[_|B]|Sks], [NV|Vs], [[NV|B]|NSks]) :-
	skolem_new_vars(Sks, Vs, NSks).


fresh_vars([], [], B, B).
fresh_vars([V|LV], [NV|NLV], (clpbn:add_evidence(V,NV),B), B0) :-
	fresh_vars(LV, NLV, B, B0).


add_evidence(V,NV) :-
	nonvar(V), !,
	clpbn:put_atts(NV,evidence(V)).
add_evidence(V,V).

				%
% called by top-level
%
project_attributes(GVars, AVars) :-
	AVars = [_|_], !,
	execute_pre_evidence,
	sort_vars_by_key(AVars,SortedAVars,DiffVars),
	get_clpbn_vars(GVars,CLPBNGVars),
	write_out(CLPBNGVars, SortedAVars,DiffVars).
project_attributes(_, _).

get_clpbn_vars([],[]).
get_clpbn_vars([V|GVars],[V|CLPBNGVars]) :-
	get_atts(V, [key(_)]), !,
	get_clpbn_vars(GVars,CLPBNGVars).
get_clpbn_vars([_|GVars],CLPBNGVars) :-
	get_clpbn_vars(GVars,CLPBNGVars).

sort_vars_by_key(AVars,SortedAVars, UnifiableVars) :-
	get_keys(AVars, KeysVars),
	keysort(KeysVars, KVars),
	merge_same_key(KVars, SortedAVars, [], UnifiableVars).

get_keys([], []).
get_keys([V|AVars], [K-V|KeysVars]) :-
	get_atts(V, [key(K)]),
	get_keys(AVars, KeysVars).

merge_same_key([], [], _, []).
merge_same_key([K1-V1,K2-V2|Vs], SortedAVars, Ks, UnifiableVars) :-
	K1 == K2, !, V1 = V2,
	merge_same_key([K1-V1|Vs], SortedAVars, Ks, UnifiableVars).
merge_same_key([K1-V1,K2-V2|Vs], [V1|SortedAVars], Ks, [K1|UnifiableVars]) :-
	(in_keys(K1, Ks) ; \+ \+ K1 = K2), !, 
	add_to_keys(K1, Ks, NKs),
	merge_same_key([K2-V2|Vs], SortedAVars, NKs, UnifiableVars).
merge_same_key([K-V|Vs], [V|SortedAVars], Ks, UnifiableVars) :-
	add_to_keys(K, Ks, NKs),
	merge_same_key(Vs, SortedAVars, NKs, UnifiableVars).

in_keys(K1,[K|_]) :- \+ \+ K1 = K, !.
in_keys(K1,[_|Ks]) :- 
	in_keys(K1,Ks).
	
add_to_keys(K1, Ks, Ks) :- ground(K1), !.
add_to_keys(K1, Ks, [K1|Ks]).

write_out(GVars, AVars, DiffVars) :-
	use(vel),
	vel(GVars, AVars, DiffVars).
write_out(GVars, AVars, _) :-
	use(bnt),
	dump_as_bnt(GVars, AVars).
%	starter_vars(GVars).

starter_vars([]).
starter_vars([Var|Vs]) :-
	get_atts(Var, [key(_)]),
	put_atts(Var, [starter]),
	starter_vars(Vs).


get_bnode(Var, Goal) :-
	get_atts(Var, [key(Key),dist(X)]),
	dist_goal(X, Key, Goal0),
	include_evidence(Var, Goal0, Key, Goali),
	include_starter(Var, Goali, Key, Goal).

include_evidence(Var, Goal0, Key, ((Key<--Ev),Goal0)) :-
	get_atts(Var, [evidence(Ev)]), !.
include_evidence(_, Goal0, _, Goal0).

include_starter(Var, Goal0, Key, ((<--Key),Goal0)) :-
	get_atts(Var, [starter]), !.
include_starter(_, Goal0, _, Goal0).

dist_goal(norm(M,V), Key, (Key=norm(M,V))) :- !.
dist_goal(avg(L), Key, (Key=avg(L))) :- !.
dist_goal(Dist, Key, (Key=NDist)) :-
	term_variables(Dist, DVars),
	process_vars(DVars, DKeys),
	my_copy_term(Dist,DVars, NDist,DKeys).

my_copy_term(V, DVars, Key, DKeys) :-
	find_var(DVars, V, Key, DKeys).
my_copy_term(A, _, A, _) :- atomic(A), !.
my_copy_term(T, Vs, NT, Ks) :-
	T =.. [Na|As],
	my_copy_terms(As, Vs, NAs, Ks),
	NT =.. [Na|NAs].

my_copy_terms([], _, [], _).
my_copy_terms([A|As], Vs, [NA|NAs], Ks) :-
	my_copy_term(A, Vs, NA, Ks),
	my_copy_terms(As, Vs, NAs, Ks).

find_var([V1|_], V, Key, [Key|_]) :- V1 == V, !.
find_var([_|DVars], V, Key, [_|DKeys]) :-
	find_var(DVars, V, Key, DKeys).

process_vars([], []).
process_vars([V|Vs], [K|Ks]) :-
        process_var(V, K),
	process_vars(Vs, Ks).

process_var(V, K) :- get_atts(V, [key(K)]), !. 
% oops: this variable has no attributes.
process_var(V, _) :- throw(error(instantiation_error,clpbn(attribute_goal(V)))).

%
% unify a CLPBN variable with something. 
%
verify_attributes(Var, T, Goals) :-
	get_atts(Var, [key(Key),dist(Dist)]), !,
	/* oops, someone trying to bind a clpbn constrained variable */
	Goals = [],
	bind_clpbn(T, Var, Key, Dist).
verify_attributes(_, _, []).


bind_clpbn(T, _, Key, Dist) :- var(T),
	get_atts(T, [key(Key1),dist(Dist1)]), !,
	bind_clpbns(Key, Dist, Key1, Dist1).
bind_clpbn(_, Var, _, _) :-
	use(bnt),
	check_if_bnt_done(Var), !.
bind_clpbn(_, Var, _, _) :-
	use(vel),
	check_if_vel_done(Var), !.
bind_clpbn(T, Var, Key0, _) :-
	get_atts(Var, [key(Key0)]), !,
	(
	  Key = Key0 -> true
	;
	  format(user_error, "trying to force evidence ~w through unification with key ~w~n",[T, Key])
	).

fresh_attvar(Var, NVar) :-
	get_atts(Var, LAtts),
	put_atts(NVar, LAtts).

bind_clpbns(Key, Dist, Key, Dist1) :- !,
	( Dist = Dist1 -> true ; throw(error(domain_error(bayesian_domain),bind_clpbns))).
bind_clpbns(_, _, _, _, _) :-
	format(user_error, "unification of two bayesian vars not supported~n").

:- yap_flag(toplevel_hook,clpbn:init_clpbn).

hash_table_size(300000).

init_clpbn :-
	reset_clpbn,
	fail.
%init_clpbn :-
%	hash_table_size(HashTableSize),
%	array(clpbn,HashTableSize),
%	catch(static_array(keys,HashTableSize,term),_,true).
	

random_tmp_number(I) :-
	get_value(clpbn_random_tmp_number,I),
	I1 is I+1,
	set_value(clpbn_random_tmp_number,I1).

reset_clpbn :-
	current_predicate(_, clpbn_aux:P),
	retract(clpbn_aux:(P :- !)),
	fail.
reset_clpbn :-
	set_value(clpbn_key, 0), fail.
reset_clpbn :-
	set_value(clpbn_random_tmp_number, 0), fail.
reset_clpbn.


user:term_expansion((A :- {}), ( :- true )) :-	 !, % evidence
	prolog_load_context(module, M),
	add_to_evidence(M:A).
user:term_expansion((A :- B), (A :- (LCs,NB))) :-			% expands heads
	fetch_skolems(B, B0, Skolems, []),
	Skolems \= [],
	skolem_vars(Skolems, Vars),
	copy_term(Vars+A, NVars+NA),
	skolem_new_vars(Skolems, NVars, NSkolems),
	compile_skolems(NSkolems, Vars, NVars, NA, LCs),
	handle_body_goals(B0, B1),
	fresh_vars(Vars, NVars, NB, B1).

