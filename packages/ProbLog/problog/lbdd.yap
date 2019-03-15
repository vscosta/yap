
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

maplist_to_hash([], H0, H0).
maplist_to_hash([I-V|MapList], H0, Hash) :-
	rb_insert(H0, V, I, H1),
	maplist_to_hash(MapList, H1, Hash).

bind_maplist([]).
bind_maplist([Node-Theta|MapList]) :-
	get_prob(Node, ProbFact),
	inv_sigmoid(ProbFact, Theta),
	bind_maplist(MapList).

tree_to_grad([], _, Grad, Grad).
tree_to_grad([Node|Tree], H, Grad0, Grad) :-
	node_to_gradient_node(Node, H, GNode),
	tree_to_grad(Tree, H, [GNode|Grad0], Grad).

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
%	writeln(grad(QueryID:I:Grad)),
	assert(query_gradient_intern(QueryID,I,p,Grad)),
	fail.
gradient(QueryID, g, Slope) :-
	gradient(QueryID, l, Slope).

query_probability( DBDD, Slope, Prob) :-
    DBDD = bdd(Dir, Tree, MapList),
    bind_maplist(MapList),
    run_sp(Tree, Slope, 1.0, Prob0),
    (Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0).

    
query_gradients(bdd(Dir, Tree, MapList),Slope,I,Grad) :-
	bind_maplist(MapList),
        member(I-_, MapList),
	run_grad(Tree, I, Slope, 0.0, Grad0),
	( Dir = 1 -> Grad = Grad0 ; Grad is -Grad0).


node_to_gradient_node(pp(P-G,X,L,R), H, gnodep(P,G,X,Id,PL,GL,PR,GR)) :-
	rb_lookup(X,Id,H),
	(L == 1 -> GL=0, PL=1 ; L == 0 -> GL = 0, PL=0 ; L = PL-GL),
	(R == 1 -> GR=0, PR=1 ; R == 0 -> GR = 0, PR=0 ; R = PR-GR).
node_to_gradient_node(pn(P-G,X,L,R), H, gnoden(P,G,X,Id,PL,GL,PR,GR)) :-
	rb_lookup(X,Id,H),
	(L == 1 -> GL=0, PL=1 ; L == 0 -> GL = 0, PL=0 ; L = PL-GL),
	(R == 1 -> GR=0, PR=1 ; R == 0 -> GR = 0, PR=0 ; R = PR-GR).
        
run_sp([], _, P0, P0).
run_sp(gnodep(P,_G, X, _Id, PL, _GL, PR, _GR).Tree, Slope, _, PF) :-
	EP = 1.0 / (1.0 + exp(-X * Slope) ),
	P is EP*PL+ (1.0-EP)*PR,
	run_sp(Tree, Slope, P, PF).
run_sp(gnoden(P,_G, X, _Id, PL, _GL, PR, _GR).Tree, Slope, _, PF) :-
	EP is 1.0 / (1.0 + exp(-X * Slope) ),
	P is EP*PL + (1.0-EP)*(1.0 - PR),
	run_sp(Tree, Slope, P, PF).

run_grad([], _I, _, G0, G0).
run_grad([gnodep(P,G, X, Id, PL, GL, PR, GR)|Tree], I, Slope, _, GF) :-
	EP is 1.0/(1.0 + exp(-X * Slope)),
	P is EP*PL+ (1.0-EP)*PR,
	G0 is EP*GL + (1.0-EP)*GR,
	% don' t forget the -X
	( I == Id -> G is G0+(PL-PR)* EP*(1-EP)*Slope ; G = G0 ),
	run_grad(Tree, I, Slope, G, GF).
run_grad([gnoden(P,G, X, Id, PL, GL, PR, GR)|Tree], I, Slope, _, GF) :-
	EP is 1.0 / (1.0 + exp(-X * Slope) ),
	P is EP*PL + (1.0-EP)*(1.0 - PR),
	G0 is EP*GL  - (1.0 - EP) * GR,
	( I == Id -> G is G0+(PL+PR-1)*EP*(1-EP)*Slope ; G = G0 ),
	run_grad(Tree, I, Slope, G, GF).


