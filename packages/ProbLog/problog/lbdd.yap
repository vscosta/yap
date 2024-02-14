%========================================================================
%=
%=
%=
%========================================================================

/**
 * @file problog/lbdd.yap
 * support routines for BDD evaluation.
 *
*/
:- module(lbdd,[
	      set_tunable/3,
	prob2log/4,
	log2prob/4,
	bind_maplist/3,
	get_prob/2,
	store_gradient/3]
).

:- use_module('../problog').
:- use_module('flags').
:- use_module('logger').
:- use_module(library(matrix)).
:- use_module(library(lists)).
:- use_module(library(bdd)).

set_tunable(I,Slope,P) :-	
    X <== P[I],
    sigmoid(X,Slope,NewProbability),
    Prob_Secure is min(0.99,max(0.01,NewProbability)),
    problog:set_fact_probability(I,Prob_Secure).

%========================================================================
%= Updates all values of query_probability/2 and query_gradient/4
%= should be called always before these predicates are accessed

%= if the old values are still valid, nothing happens
%========================================================================

prob2log(_X,Slope,FactID,V) :-
    problog:get_fact_probability(FactID, V0),
    problog:inv_sigmoid(V0, Slope, V).

log2prob(X,Slope,FactID,V) :-
    V0 <== X[FactID],
    problog:sigmoid(V0, Slope, V).

bind_maplist([], _Slope, _X).
bind_maplist([Node-(Node-NPr)|MapList], Slope, X) :-
    SigPr <== X[Node],
    problog:sigmoid(SigPr, Slope, Pr),
    NPr is min(0.99,max(0.01,Pr)),
    bind_maplist(MapList, Slope, X).


%get_prob(Node, Prob) :-
%	query_probability(Node,Prob), !.
get_prob(Node, Prob) :-
	problog:get_fact_probability(Node,Prob).


bindp(I-Pr) :-
    problog:get_fact_probability(I,Pr).


bindg(I-(I-Pr)) :-
    problog:get_fact_probability(I,Pr).




store_gradient(QueryID,l,_Slope) :-
	recorded(QueryID,BDD,_),
	BDD = bdd(_,_,_,MapList),
	MapList = [_|_],
	maplist(bindp, MapList),
	tree_to_sp( BDD, MapList, Prob),
		assert(learning:query_probability_intern(QueryID,Prob)).
store_gradient(QueryID, g, _Slope) :-
	recorded(QueryID, BDD, _),
	BDD = bdd(_,_,_,MapList),
	maplist(bindg, MapList),
    member(I-_, MapList),
	tree_to_grad( BDD, I, Grad),
	assert(learning:query_gradient_intern(QueryID,I,p,Grad)),
	fail.
store_gradient(QueryID, g, Slope) :-
	store_gradient(QueryID, l, Slope).


