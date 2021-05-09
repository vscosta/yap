%
% ProbLog extension to use an YAP BDD interface module, instead of simplecudd.
%

:- use_module(library(trie_sp)).
:- use_module(library(bdd)).
:- use_module(library(bhash)).
:- use_module(library(problog/lbdd)).

problog_lbdd_exact(Goal,BDD, Prob) :-
    BDD = bdd(_Dir, Tree, MapList),
    problog_lbdd_exact_tree(Goal, BDD ),
	bind_maplist(MapList, BoundVars),
	bdd_to_probability_sum_product(BDD, BoundVars, Prob).
%	evalp(Tree, Prob).

problog_lbdd_exact(Goal, Prob) :-
    BDD = bdd(_Dir, Tree, MapList),
    problog_lbdd_exact_tree(Goal, BDD ),
	bind_maplist(MapList, BoundVars),
	bdd_to_probability_sum_product(BDD, BoundVars, Prob).

problog_lbdd_exact_tree(Goal,Tree) :-
	problog_control(on, exact),
	problog_lbdd_low(Goal,0,Tree,ok),
	problog_control(off, exact).

problog_lbdd_low(Goal, Threshold, _, _) :-
	init_problog_low(Threshold),
	problog_control(off, up),
	timer_start(sld_time),
	problog_call(Goal),
	add_solution,
	fail.
problog_lbdd_low(_, _, Tree, ok) :-
    timer_stop(sld_time,SLD_Time),
	problog_var_set(sld_time, SLD_Time),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	trie_to_bdd_tree(Trie_Completed_Proofs, Tree),
	(problog_flag(verbose, true)->
	 problog_statistics
	;
	 true
	),
	delete_ptree(Trie_Completed_Proofs),
	(problog_flag(retain_tables, true) -> retain_tabling; true),
	clear_tabling.

                                                                           
problog_lbdd_kbest(Goal, K, Prob) :-
	problog_lbdd_kbest_tree(Goal, K, bdd(_Dir, Tree, MapList)),
	bind_maplist(MapList, _BoundVars),
	evalp(Tree, Prob).

problog_lbdd_kbest_tree(Goal, K, Tree) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	problog_control(off,up),
	problog_kbest_id(Goal, K),
	retract(current_kbest(_,ListFound,_NumFound)),
	build_prefixtree(ListFound),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	trie_to_bdd_tree(Trie_Completed_Proofs, Tree),
	delete_ptree(Trie_Completed_Proofs).


problog_lbdd_fl(Goal,  _) :-
	init_problog_low(0.0),
	problog_control(off, up),
	timer_start(sld_time),
	problog_call(Goal),
	add_solution,
	fail.
problog_lbdd_fl(_,Prob) :-
	timer_stop(sld_time,SLD_Time),
	problog_var_set(sld_time, SLD_Time),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	tabled_trie_to_bdd(Trie_Completed_Proofs, BDD, MapList),
	bind_maplist(MapList, BoundVars),
	bdd_to_probability_sum_product(BDD, BoundVars, Prob),
	(problog_flag(retain_tables, true) -> retain_tabling; true),
	clear_tabling.

bind_maplist([], []).
bind_maplist([Node-_|MapList], [ProbFact|BoundVars]) :-
	get_fact_probability(Node,ProbFact),
	bind_maplist(MapList, BoundVars).

