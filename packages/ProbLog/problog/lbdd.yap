
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
	gradient/3,
	query_probabilities/2,
	evalp/2,
	query_gradients/4
	
]
).


:- use_module('../problog_lbfgs').
:- use_module('flags').
:- use_module('logger').
:- use_module(library(matrix)).
:- use_module(library(lists)).

set_tunable(I,Slope,P) :-
    X <== P[I],
    sigmoid(X,Slope,NewProbability),
    Prob_Secure is min(0.99,max(0.01,NewProbability)),
    set_fact_probability(I,Prob_Secure).

%========================================================================
%= Updates all values of query_probability/2 and query_gradient/4
%= should be called always before these predicates are accessed
%= if the old values are still valid, nothing happens
%========================================================================

prob2log(_X,Slope,FactID,V) :-
    get_fact_probability(FactID, V0),
    inv_sigmoid(V0, Slope, V).

log2prob(X,Slope,FactID,V) :-
    V0 <== X[FactID],
    sigmoid(V0, Slope, V).

bind_maplist([], _Slope, _X).
bind_maplist([Node-(Node-NPr)|MapList], Slope, X) :-
    SigPr <== X[Node],
    sigmoid(SigPr, Slope, Pr),
    NPr is min(0.99,max(0.01,Pr)),
    bind_maplist(MapList, Slope, X).


%get_prob(Node, Prob) :-
%	query_probability(Node,Prob), !.
get_prob(Node, Prob) :-
	get_fact_probability(Node,Prob).


bindp(I-(I-Pr)) :-
    get_fact_probability(I,Pr).

gradient(_QueryID, l, _).

gradient(QueryID,p,BDDProb) :-
	recorded(QueryID,BDD,_),
	BDD = bdd(_,_,MapList),
%		write(MapList:' '),
	MapList = [_|_],
	maplist(bindp, MapList),
	query_probabilities( BDD, BDDProb).

/*	query_probability(21,6.775948e-01). */
gradient(QueryID, g, Slope) :-
	recorded(QueryID, BDD, _),
	query_gradients(BDD,Slope,I,Grad),
	assert(query_gradient_intern(QueryID,I,p,Grad)),
	fail.
gradient(QueryID, g, Slope) :-
	gradient(QueryID, l, Slope).

query_probabilities( DBDD, Prob) :-
    DBDD = bdd(Dir, Tree, _MapList),
    findall(P, evalp(Tree,P), [Prob0]),
   % nonvar(Prob0),
    (Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0).

evalp( Tree, Prob0) :-
    foldl(evalp, Tree, _, Prob0).
    
query_gradients(bdd(Dir, Tree, MapList),I,IProb,Grad) :-
        member(I-(_-IProb), MapList),
	% run_grad(Tree, I, Slope, 0.0, Grad0),
	foldl( evalg(I), Tree, _, Grad0),
	( Dir == 1 -> Grad = Grad0 ; Grad is -Grad0).

evalp( pn(P, _-X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*(1.0-PR).
evalp( pp(P, _-X, PL, PR), _,P ):-  
    P is X*PL+ (1.0-X)*PR.

evalg( I, pp(P-G, J-X, L, R), _, G ):-
    ( number(L) -> PL=L, GL = 0.0 ; L = PL-GL ),
    ( number(R) -> PR=R, GR = 0.0 ; R = PR-GR ),
    P is X*PL+ (1.0-X)*PR,
    (
	I == J
    ->
    G is X*GL+ (1.0-X)*GR+PL-PR
    ;
    G is X*GL+ (1.0-X)*GR
    ).
evalg( I, pn(P-G, J-X, L, R), _,G ):-
    ( number(L) -> PL=L, GL = 0.0 ; L = PL-GL ),
    ( number(R) -> PR=R, GR = 0.0 ; R = PR-GR ),
    P is X*PL+ (1.0-X)*(1.0-PR),
    (
	I == J
    ->
    G is X*GL-(1.0-X)*GR+PL-(1-PR)
    ;
    G is X*GL- (1.0-X)*GR
    ).


