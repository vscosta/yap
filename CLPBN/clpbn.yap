

:- module(clpbn, [{}/1,
		  clpbn_flag/2,
		  set_clpbn_flag/2,
		  clpbn_flag/3]).

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

:- attribute key/1, dist/3, evidence/1, starter/0.


:- use_module('clpbn/bnt', [dump_as_bnt/2,
		    check_if_bnt_done/1
		    ]).

:- use_module('clpbn/vel', [vel/3,
		    check_if_vel_done/1
		    ]).

:- use_module('clpbn/gibbs', [gibbs/3,
		    check_if_gibbs_done/1
		    ]).

:- use_module('clpbn/graphs', [
		    clpbn2graph/1
		    ]).

:- use_module('clpbn/evidence', [
	store_evidence/1,
	incorporate_evidence/2
	]).

:- dynamic solver/1,output/1,use/1.

solver(vel).

%output(xbif(user_error)).
%output(gviz(user_error)).
output(no).

clpbn_flag(Flag,Option) :-
	clpbn_flag(Flag, Option, Option).

set_clpbn_flag(Flag,Option) :-
	clpbn_flag(Flag, _, Option).

clpbn_flag(output,Before,After) :-
	retract(output(Before)),
	assert(output(After)).
clpbn_flag(solver,Before,After) :-
	retract(solver(Before)),
	assert(solver(After)).

{Var = Key with Dist} :-
	put_atts(El,[key(Key),dist(Domain,Table,Parents)]),
	extract_dist(Dist, Table, Parents, Domain),
	add_evidence(Var,El).

extract_dist(V, Tab.Inps, Domain) :- var(V), !,
	V = p(Domain, Tab, Inps).
extract_dist(p(Domain, Tab, Inps), Tab, Inps, Domain).
extract_dist(p(Domain, Tab), Tab, [], Domain).
	
check_constraint(Constraint, _, _, Constraint) :- var(Constraint), !.
check_constraint((A->D), _, _, (A->D)) :- var(A), !.
check_constraint((([A|B].L)->D), Vars, NVars, (([A|B].NL)->D)) :- !,
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

add_evidence(V,NV) :-
	nonvar(V), !,
	clpbn:put_atts(NV,evidence(V)).
add_evidence(V,V).

%
% called by top-level
% or by call_residue/2
%
project_attributes(GVars, AVars) :-
	AVars = [_|_], !,
	sort_vars_by_key(AVars,SortedAVars,DiffVars),
	get_clpbn_vars(GVars,CLPBNGVars),
	solver(Solver),
	incorporate_evidence(SortedAVars, AllVars),
	write_out(Solver,CLPBNGVars, AllVars, DiffVars).
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
	get_atts(V, [key(K)]), !,
	get_keys(AVars, KeysVars).
get_keys([_|AVars], KeysVars) :-  % may be non-CLPBN vars.
	get_keys(AVars, KeysVars).

merge_same_key([], [], _, []).
merge_same_key([K1-V1,K2-V2|Vs], SortedAVars, Ks, UnifiableVars) :-
	K1 == K2, !, V1 = V2,
	merge_same_key([K1-V1|Vs], SortedAVars, Ks, UnifiableVars).
merge_same_key([K1-V1,K2-V2|Vs], [V1|SortedAVars], Ks, [K1|UnifiableVars]) :-
	(in_keys(K1, Ks) ; \+ \+ K1 == K2), !, 
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

write_out(vel, GVars, AVars, DiffVars) :-
	vel(GVars, AVars, DiffVars).
write_out(gibbs, GVars, AVars, DiffVars) :-
	gibbs(GVars, AVars, DiffVars).
write_out(bnt, GVars, AVars, _) :-
	dump_as_bnt(GVars, AVars).
write_out(graphs, _, AVars, _) :-
	clpbn2graph(AVars).

get_bnode(Var, Goal) :-
	get_atts(Var, [key(Key),dist(A,B,C)]),
	(C = [] -> X = tab(A,B) ; X = tab(A,B,C)),
	dist_goal(X, Key, Goal0),
	include_evidence(Var, Goal0, Key, Goali),
	include_starter(Var, Goali, Key, Goal).

include_evidence(Var, Goal0, Key, ((Key:-Ev),Goal0)) :-
	get_atts(Var, [evidence(Ev)]), !.
include_evidence(_, Goal0, _, Goal0).

include_starter(Var, Goal0, Key, ((:-Key),Goal0)) :-
	get_atts(Var, [starter]), !.
include_starter(_, Goal0, _, Goal0).

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
	get_atts(Var, [key(Key),dist(Domain,Table,Parents)]), !,
	/* oops, someone trying to bind a clpbn constrained variable */
	Goals = [],
	bind_clpbn(T, Var, Key, Domain, Table, Parents).
verify_attributes(_, _, []).


bind_clpbn(T, Var, Key, Domain, Table, Parents) :- var(T),
	get_atts(T, [key(Key1),dist(Doman1,Table1,Parents1)]), !,
	bind_clpbns(Key, Domain, Table, Parents, Key1, Doman1, Table1, Parents1),
	(
	  get_atts(T, [evidence(Ev1)]) ->
	    bind_evidence_from_extra_var(Ev1,Var)
	;
	  get_atts(Var, [evidence(Ev)]) ->
	    bind_evidence_from_extra_var(Ev,T)
	;
	  true).
bind_clpbn(_, Var, _, _, _, _) :-
	use(bnt),
	check_if_bnt_done(Var), !.
bind_clpbn(_, Var, _, _, _, _) :-
	use(vel),
	check_if_vel_done(Var), !.
bind_clpbn(T, Var, Key0, _, _, _) :-
	get_atts(Var, [key(Key)]), !,
	(
	  Key = Key0 -> true
	;
	  add_evidence(Var,T)
	).

fresh_attvar(Var, NVar) :-
	get_atts(Var, LAtts),
	put_atts(NVar, LAtts).

% I will now allow two CLPBN variables to be bound together.
%bind_clpbns(Key, Domain, Table, Parents, Key, Domain, Table, Parents).
bind_clpbns(Key, Domain, Table, Parents, Key1, Domain1, Table1, Parents1) :- 
	Key == Key1, !,
	( Domain == Domain1, Table == Table1, Parents == Parents1 -> true ; throw(error(domain_error(bayesian_domain),bind_clpbns(var(Key, Domain, Table, Parents),var(Key1, Domain1, Table1, Parents1))))).
bind_clpbns(_, _, _, _, _, _, _, _) :-
	format(user_error, 'unification of two bayesian vars not supported~n', []).

bind_evidence_from_extra_var(Ev1,Var) :-
	get_atts(Var, [evidence(Ev0)]),!,Ev0 = Ev1.
bind_evidence_from_extra_var(Ev1,Var) :-
	put_atts(Var, [evidence(Ev1)]).

user:term_expansion((A :- {}), ( :- true )) :-	 !, % evidence
	prolog_load_context(module, M),
	store_evidence(M:A).

