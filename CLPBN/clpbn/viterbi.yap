
:- style_check(all).

:- module(viterbi, [viterbi/3]).

:- use_module(library(lists),
	      [nth/3]).

:- use_module(library('clpbn'), []).

:- use_module(library('clpbn/utils'), [
	sort_vars_by_key/3]).

:- use_module(library('ugraphs'), [
				   vertices_edges_to_ugraph/3,
				   add_edges/3,
				   top_sort/3]).

:- attribute prob/1, emission/1, backp/1.

viterbi(Start,End,Trace) :-
	attributes:all_attvars(Vars0),
	sort_vars_by_key(Vars0,Vars,_),
	add_emissions(Vars),
	topsort_vars(Vars,SortedVars),
	init_viterbi(Start),
	viterbi_alg(SortedVars),
	backtrace(Start,End,[],Trace).


add_emissions([]).
add_emissions([Var|Vars]) :-
	add_emission(Var),
	add_emissions(Vars).
	
add_emission(Var) :-
	clpbn:get_atts(Var,[evidence(Ev),dist(Vals,emission(CPT),Parents)]), !,
	nth(Nth, Vals, Ev),
	find_probs(CPT,Nth,Prob),
	adde2pars(Parents,Prob).
add_emission(_).

find_probs(log(Logs,_Norms),Nth,Log) :-
	arg(Nth,Logs,Log).
%	get_norm(Norms,Nth,Norm).

%get_norm(Norms,_,Norms) :- number(Norms), !.
%get_norm(Norms,Nth,Norm) :-
%	arg(Nth,Norms,Norm).

adde2pars([],_).
adde2pars([V|Vs],P) :-
	put_atts(V,[emission(P)]),
	adde2pars(Vs,P).

topsort_vars(Vars,SortedVars) :-
	vertices_edges_to_ugraph([],[],Graph0),
	sort_vars(Vars, Graph0, SortedVars).

%
% take advantage of the fact that variables can be split by timestamp.
%
sort_vars(Vars, Graph0, SortedVars) :-
	fetch_times(Vars,T0Vars),
	keysort(T0Vars, TVars),
	sort_times(TVars, Graph0, SortedVars).

fetch_times([], []).
fetch_times([V|Vs], [T-V|TVs]) :-
	clpbn:get_atts(V,[key(K)]),
	arg(1,K,T),
	fetch_times(Vs, TVs).

sort_times([], _, []).
sort_times([T-V|TVs], Graph0, SortedVars) :-
	fetch_same_time(TVs, T, Vars, NTVs),
	fetch_parents([V|Vars],Graph0,Graph),
	top_sort(Graph,SortedVars0,SortedVars),
	sort_times(NTVs, Graph0, SortedVars0).

fetch_same_time([T-V|TVs], T, [V|Vs], TVs0) :-
	fetch_same_time(TVs, T, Vs, TVs0).
fetch_same_time(TVs, _, [], TVs) :- !.


fetch_parents([],Graph,Graph).
fetch_parents([V|Vars],Graph0,GraphF) :-
	clpbn:get_atts(V,[dist(_,_,Parents)]),
	exp_edges(Parents,V,Graph0,GraphI),
	fetch_parents(Vars,GraphI,GraphF).

exp_edges([],_,Graph,Graph).
exp_edges([P|Parents],V,Graph0,GraphF) :-
	add_edges(Graph0,[V-P],GraphI),
	exp_edges(Parents,V,GraphI,GraphF).

extract_vars([],[]).
extract_vars([_-V|KVars],[V|Vars]) :-
	extract_vars(KVars,Vars).

init_viterbi(V) :-
	put_atts(V,[prob(0)]).

viterbi_alg([]).
viterbi_alg([V|Vs]) :-
	% get the current status
	get_atts(V,[prob(P0)]), !,
	clpbn:get_atts(V,[dist(_,trans(Probs),States)]),
	% adjust to consider emission probabilities
	adjust_for_emission(V, P0, Pf),
	propagate(Probs,States,Pf,V),
	viterbi_alg(Vs).
viterbi_alg([_|Vs]) :-
	viterbi_alg(Vs).

adjust_for_emission(V, P0, Pf) :-
	get_atts(V,[emission(P)]), !,
	mprob(P0,P,Pf),
	put_atts(V,[prob(Pf)]).
adjust_for_emission(_, P, P).

propagate([],[],_,_).
propagate([log(Prob,_)|Probs],[State|States],Pf,V) :-
	get_atts(State,[prob(P0)]), !,
	mprob(Pf,Prob,P),
	(P > P0 ->
	    put_atts(State,[prob(P),backp(V)])
	;
	    true
	),
	propagate(Probs,States,Pf,V).
propagate([log(Prob,_)|Probs],[State|States],Pf,V) :-
	mprob(Pf,Prob,P),
	put_atts(State,[prob(P),backp(V)]),
	propagate(Probs,States,Pf,V).

backtrace(Start,Var,Trace,Trace) :- Start == Var, !.
backtrace(Start,Var,Trace0,Trace) :-
	get_atts(Var,[backp(V)]),
	clpbn:get_atts(Var, [key(K)]),
	backtrace(Start,V,[K|Trace0],Trace).


	
mprob(*,P,P) :- !.
mprob(P,*,P) :- !.
mprob(P1,P2,P) :- P is P1+P2.


	
