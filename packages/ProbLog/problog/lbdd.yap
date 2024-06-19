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

:- use_module(library(problog)).
:- use_module(library('problog/flags')).
:- use_module(library('problog/logger')).
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
store_gradient(QueryID, g, Slope) :-
    	user:example(QueryID,_,V0,_),
	problog:inv_sigmoid(V0, Slope, TrueProb),
	recorded(QueryID, BDD, _),
	BDD = bdd(_,_,_,MapList),
	maplist(bindg, MapList),
   	 member(I-_, MapList),
	tree_to_grad( BDD, I, Prob, Grad0),
	Error is Prob-TrueProb,
	Grad is (Grad0*Prob*(1.0-Prob)*2*Error),
	assert(learning:query_gradient_intern(QueryID,I,p,Grad)),
	fail.
store_gradient(QueryID, g, Slope) :-
	store_gradient(QueryID, l, Slope).


