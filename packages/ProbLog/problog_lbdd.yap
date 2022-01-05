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
    problog_lbdd(Goal, 0.0,Prob).

problog_lbdd(Goal, Threshold,_Prob) :-
    build_low_tree(Goal, Threshold),
    fail.
problog_lbdd(_, _, Prob) :-
    close_low_tree(CUDD),
    CUDD = cudd(_Manager, _Tree, _, MapList),
    bind_maplist(MapList,Probs),
    bdd_to_probability_sum_product(CUDD, Probs, Prob),
    bdd_close(CUDD).
%	evalp(Tree, Prob).

problog_lbdd_tree(Goal, BDD) :-
    problog_lbdd_tree(Goal, 0.0, BDD).

problog_lbdd_tree(Goal, Threshold, _BDD) :-
    build_low_tree(Goal, Threshold),
    fail.
problog_lbdd_tree(_, _, BDD ) :-
    close_low_tree(CUDD, MapList),
    bdd_tree(CUDD,MapList,BDD),
    bdd_close(CUDD).
%	evalp(Tree, Prob).

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
	 format("~nProofs for query ~w:~n", []),
	 trie_print(Trie_Completed_Proofs),
	 writeln('  **********')
    ;
    true
    ),
    trie_to_cudd(Trie_Completed_Proofs, MapList, CUDD),
    nb_setval(problog_completed_proofs, []),
    close_trie(Trie_Completed_Proofs).
    
problog_lbdd_exact(Goal, Prob) :-
    problog_lbdd(Goal,_BDD, Prob).

problog_lbdd_exact_tree(Goal,Tree) :-
    problog_lbdd(Goal,Tree,_).

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
	tabled_trie_to_bdd_tree(Trie_Completed_Proofs, BDD, MapList),
	bind_maplist(MapList, BoundVars),
	bdd_to_probability_sum_product(BDD, BoundVars, Prob),
	(problog_flag(retain_tables, true) -> retain_tabling; true),
	clear_tabling.

bind_maplist([], []).
bind_maplist([Node-_|MapList], [ProbFact|BoundVars]) :-
	get_fact_probability(Node,ProbFact),
	bind_maplist(MapList, BoundVars).


