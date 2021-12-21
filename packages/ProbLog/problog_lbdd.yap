%
% ProbLog extension to use an YAP BDD interface module, instead of simplecudd.
%

:- use_module(library(trie_sp)).
:- use_module(library(bdd)).

:- use_module(library(tries)).
:- use_module(library(bhash)).
:- use_module(library(problog/lbdd)).

:- dynamic user :debug_problog/0.

problog_lbdd(Goal, Prob) :-
    problog_lbdd_low(Goal,_BDD, 0,Prob).

problog_lbdd(Goal,BDD, Prob) :-
    problog_lbdd_low(Goal,BDD, 0,Prob).

problog_lbdd_low(Goal, Threshold,_Prob) :-
    problog_lbdd_low(Goal,_BDD, Threshold,_Prob).


problog_lbdd_low(Goal,_BDD, Threshold,_Prob) :-
	init_problog_low(Threshold),
	problog_control(off, up),
	timer_start(sld_time),
	problog_call(Goal),
	add_solution,
	fail.
problog_lbdd_low(_, BDD, _, Prob) :-
    timer_stop(sld_time,SLD_Time),
    problog_var_set(sld_time, SLD_Time),
    nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
    (user:debug_problog ->
	 format("~nProofs for query ~w:~n", []),
	 trie_print(Trie_Completed_Proofs),
	 writeln('  **********')
	       ;
    true
    ),
    trie_to_bdd(Trie_Completed_Proofs, BDD),
    nb_setval(problog_completed_proofs, []),
    close_trie(Trie_Completed_Proofs),
    BDD = cudd(_Manager, _Tree, _, MapList),
    bind_maplist(MapList,Probs),
    bdd_to_probability_sum_product(BDD, Probs, Prob),
    bdd_close(BDD).
    %	evalp(Tree, Prob).

%:- spy problog_lbdd_exact.

problog_lbdd_exact(Goal, Prob) :-
    problog_lbdd_exact(Goal,_BDD, Prob).

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
    (user:debug_problog ->
	 format("~nProofs for query ~w:~n", []),
	 trie_print(Trie_Completed_Proofs),
	 writeln('  **********')
	       ;
    true
    ),
	trie_to_bdd_tree(Trie_Completed_Proofs, Tree),
    	bind_maplist(MapList, _BoundVars),
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
	trie_to_bdd(Trie_Completed_Proofs, Tree),
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
	tabled_trie_to(Trie_Completed_Proofs, BDD, MapList),
	bind_maplist(MapList, BoundVars),
	bdd_to_probability_sum_product(BDD, BoundVars, Prob),
	(problog_flag(retain_tables, true) -> retain_tabling; true),
	clear_tabling.

bind_maplist([], []).
bind_maplist([Node-_|MapList], [ProbFact|BoundVars]) :-
	get_fact_probability(Node,ProbFact),
	bind_maplist(MapList, BoundVars).

