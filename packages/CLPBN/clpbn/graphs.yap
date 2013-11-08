
%
% Just output a graph with all the variables.
%

:- module(clpbn2graph,
		[clpbn2graph/1]).

:- use_module(library('clpbn/utils'),
		[check_for_hidden_vars/3]).

:- use_module(library('clpbn/dists'),
		[get_dist/4]).

:- use_module(library(atts)).

:- attribute node/0.

clpbn2graph(Vs) :-
	check_for_hidden_vars(Vs, Vs, NVs),
	clpbn2graph2(NVs).

clpbn2graph2([]).
clpbn2graph2([V|Vs]) :-
	put_atts(V,[node]),
	clpbn2graph2(Vs).

%
% what is actually output
%
attribute_goal(V, node(K,Dom,CPT,TVs,Ev)) :-
	get_atts(V, [node]),
	clpbn:get_atts(V, [key(K),dist(Id,Vs)]),
	get_dist(Id,_,Dom,CPT),
	translate_vars(Vs,TVs),
	( clpbn:get_atts(V, [evidence(Ev)]) -> true ; true).

translate_vars([],[]).
translate_vars([V|Vs],[K|Ks]) :-
	clpbn:get_atts(V, [key(K)]),
	translate_vars(Vs,Ks).

