
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


%========================================================================
%= Updates all values of query_probability/2 and query_gradient/4
%= should be called always before these predicates are accessed
%= if the old values are still valid, nothing happens
%========================================================================

update_values :-
	values_correct,
	!.
update_values :-
	\+ values_correct,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% delete old values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	retractall(query_probability_intern(_,_)),
	retractall(query_gradient_intern(_,_,_,_)),	


	assertz(values_correct).

update_query_cleanup(QueryID) :-
	(
	 (query_is_similar(QueryID,_) ; query_is_similar(_,QueryID))
	->
	    % either this query is similar to another or vice versa,
	    % therefore we don't delete anything
	 true;
	 retractall(query_gradient_intern(QueryID,_,_,_))
	).


update_query(QueryID,Symbol,What_To_Update) :-
	(
	 query_is_similar(QueryID,_)
	->
				% we don't have to evaluate the BDD
	 format_learning(4,'#',[]);
	 (
	  problog_flag(sigmoid_slope,Slope),
	  ((What_To_Update=all;query_is_similar(_,QueryID)) -> Method='g' ; Method='l'),
	  gradient(QueryID, Method, Slope),
	  format_learning(4,'~w',[Symbol])
	 )
	).


prob2log(_X,Slope,FactID,V) :-
    get_fact_probability(FactID, V0),
    inv_sigmoid(V0, Slope, V).

log2prob(X,Slope,FactID,V) :-
    V0 <== X[FactID],
    sigmoid(V0, Slope, V).

bind_maplist([], _Slope, _X).
bind_maplist([Node-(Node-Pr)|MapList], Slope, X) :-
    SigPr <== X[Node],
    sigmoid(SigPr, Slope, Pr),
	bind_maplist(MapList, Slope, X).


%get_prob(Node, Prob) :-
%	query_probability(Node,Prob), !.
get_prob(Node, Prob) :-
	get_fact_probability(Node,Prob).

gradient(QueryID, l, Slope) :-
    probability( QueryID, Slope, Prob),
	assert(query_probability_intern(QueryID,Prob)),
	fail.
gradient(_QueryID, l, _).

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


