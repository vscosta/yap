%
% ProbLog extension to use an YAP BDD interface module, instead of simplecudd.
%

:- dynamic user :debug_problog/0.

problog_lbdd(Goal, Prob) :-
    problog_lbdd(Goal, 0.0,Prob).

problog_lbdd(Goal, Threshold, _Prob) :-
    build_low_tree(Goal, Threshold),
    fail.
problog_lbdd( _, _, Prob) :-
    close_low_tree(CUDD, MapList),
    bind_maplist(MapList),
    bdd_to_probability_sum_product(CUDD, Prob),
    bdd_close(CUDD).

problog_lbdd(Goal, Threshold, _Tree, _Prob) :-
    build_low_tree(Goal, Threshold),
    fail.
problog_lbdd(_, _, BDD, Prob ) :-
    close_low_tree(CUDD, MapList),
    bdd_to_tree(CUDD,MapList,BDD),
    bdd_close(CUDD),
    bind_maplist(MapList),
    tree_to_sp(BDD,Prob).



problog_lbdd_tree(Goal, BDD) :-
    problog_lbdd_tree(Goal, 0.0, BDD).

problog_lbdd_tree(Goal, Threshold, _BDD) :-
    build_low_tree(Goal, Threshold),
    fail.
problog_lbdd_tree(_, _, BDD ) :-
    close_low_tree(CUDD, MapList),
    bdd_to_tree(CUDD,MapList,BDD),
    bdd_close(CUDD).

%:- spy problog_lbdd_exact.

build_low_tree(Goal, Threshold) :-
    init_problog_low(Threshold),
    problog_control(off, up),
    timer_start(sld_time),
    problog_call(Goal),
    add_solution.


close_low_tree(CUDD,MapList):-
    timer_stop(sld_time,SLD_Time),
     problog_var_set(sld_time, SLD_Time),
    nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
    (user:debug_problog ->
	 format("~nProofs for query ~w:~n", [Trie_Completed_Proofs]),
   trie_print(Trie_Completed_Proofs),
	 writeln('  **********')
    ;
    true
    ),
    trie_to_cudd(Trie_Completed_Proofs, MapList, CUDD),
    nb_setval(problog_completed_proofs, []),
    close_trie(Trie_Completed_Proofs).

problog_lbdd_exact(Goal, Prob) :-
    problog_lbdd(Goal,Prob).

problog_lbdd_exact_tree(Goal,Tree) :-
    problog_lbdd_tree(Goal,Tree).

problog_lbdd_kbest(Goal, K, Prob) :-
	problog_lbdd_kbest_tree(Goal, K, BDD),
	bind_maplist(MapList),
	tree_to_sp(BDD, MapList, Prob).

problog_lbdd_kbest_tree(Goal, K, Tree) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	problog_control(off,up),
	problog_kbest_id(Goal, K),
	retract(current_kbest(_,ListFound,_NumFound)),
	build_prefixtree(ListFound),
	fail
	;
	    nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	trie_to_bdd_tree(Trie_Completed_Proofs, Tree),
	delete_ptree(Trie_Completed_Proofs).


bind_maplist([]).
bind_maplist([Node-ProbFact|MapList]) :-
	get_fact_probability(Node,ProbFact),
	bind_maplist(MapList).
