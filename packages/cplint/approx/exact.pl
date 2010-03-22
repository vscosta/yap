/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File best.pl
 *	Goal oriented interpreter for LPADs based on SLDNF
 *	Copyright (c) 2009, Stefano Bragaglia
 *============================================================================*/

:- dynamic rule/4, def_rule/2.

/* EXTERNAL FILE
 * -------------
 * The following libraries are required by the program to work fine.
 */
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(ugraphs)).
:- use_module(params).
:- use_module(tptree_lpad).
:- use_module(utility).

% :- source.
% :- yap_flag(single_var_warnings, on).





/* SOLVING PREDICATES
 * ------------------
 * The predicates in this section solve any given problem with several class of 
 * algorithms.
 *
 * Note: the original predicates (no more need and eligible to be deleted) have 
 *       been moved to the end of the file.
 */
 
/* solve(GoalsList, Prob, ResTime, BddTime)
 * ----------------------------------------
 * This predicate computes the probability of a given list of goals using an 
 * exact algorithm. It also returns the number of handled BDDs (trivial but 
 * present for simmetry with other solving predicates), CPUTime spent in 
 * performing resolution and in handling the BDDs.
 * 
 * INPUT
 *  - GoalsList: given list of goal to work on.
 *
 * OUTPUT
 *  - Prob: the resulting exact probability for the given list of goals.
 *  - Count: number of BDDs handled by the algorithm (trivial, since it's always 1).
 *  - ResTime: cpu time spent on performing resolution.
 *  - BddTime: cpu time spent on handling BDDs.
 */ 
solve(GoalsList, Prob, ResTime, BddTime) :-
	% Resetting the clocks...
	statistics(walltime, [_, _]),

	% Performing resolution...
	findall(Deriv, exact(GoalsList, Deriv), List),

	% Taking elapsed times...
	statistics(walltime, [_, ElapResTime]),
	ResTime is ElapResTime/1000,

	% Building and solving equivalent bdds...
	init_ptree(1),
	fatto(List),
	insert_list_ptree(List, 1),
	bdd_ptree_map(1, 'bdd.txt', 'bdd.par', 'bdd.map'),
	delete_ptree(1),
	run_file('bdd.txt','bdd.par', Temp),
	(Temp == timeout ->
		Prob is -1.0;
		Prob is Temp),
	
	% Taking elapsed times
	statistics(walltime, [_, ElapBddTime]),
	BddTime is ElapBddTime/1000.



/* exact(GoalsList, CIn, COut) takes a list of goals and an input C set
and returns an output C set
The C set is a list of triple (N, R, S) where
- N is the index of the head atom used, starting from 0
- R is the index of the non ground rule used, starting from 1
- S is the substitution of rule R, in the form of a list whose elements
	are of the form 'VarName'=value
*/
exact(GoalsList, Deriv) :- 
	exact(GoalsList, [], Deriv).

exact([], C, C) :- !.

exact([bagof(V, EV^G, L)|T], CIn, COut) :- !, 
	list2and(GL, G), 
	bagof((V, C), EV^exact(GL, CIn, C), LD), 
	length(LD, N), 
	build_initial_graph(N, GrIn), 	
	build_graph(LD, 0, GrIn, Gr), 
	clique(Gr, Clique), 
	build_Cset(LD, Clique, L, [], C1), 
	remove_duplicates_eq(C1, C2), 
	exact(T, C2, COut).

exact([bagof(V, G, L)|T], CIn, COut) :- !, 
	list2and(GL, G), 
	bagof((V, C), exact(GL, CIn, C), LD), 
	length(LD, N), 
	build_initial_graph(N, GrIn), 	
	build_graph(LD, 0, GrIn, Gr), 
	clique(Gr, Clique), 
	build_Cset(LD, Clique, L, [], C1), 
	remove_duplicates_eq(C1, C2), 
	exact(T, C2, COut).

exact([setof(V, EV^G, L)|T], CIn, COut) :- !, 
	list2and(GL, G), 
	setof((V, C), EV^exact(GL, CIn, C), LD), 
	length(LD, N), 
	build_initial_graph(N, GrIn), 	
	build_graph(LD, 0, GrIn, Gr), 
	clique(Gr, Clique), 
	build_Cset(LD, Clique, L1, [], C1), 	
	remove_duplicates(L1, L), 	
	exact(T, C1, COut).

exact([setof(V, G, L)|T], CIn, COut) :- !, 
	list2and(GL, G), 
	setof((V, C), exact(GL, CIn, C), LD), 
	length(LD, N), 
	build_initial_graph(N, GrIn), 	
	build_graph(LD, 0, GrIn, Gr), 
	clique(Gr, Clique), 
	build_Cset(LD, Clique, L1, [], C1), 	
	remove_duplicates(L1, L), 	
	exact(T, C1, COut).

exact([\+ H|T], CIn, COut) :- 
	builtin(H), !, 
	call((\+ H)), 
	exact(T, CIn, COut).

exact([\+ H |T], CIn, COut) :- !, 
	list2and(HL, H), 
	findall(D, find_deriv(HL, D), L), 
	choose_clauses(CIn, L, C1), 	
	exact(T, C1, COut).
	
exact([H|T], CIn, COut) :- 
	builtin(H), !, 
	call(H), 
	exact(T, CIn, COut).

exact([H|T], CIn, COut) :- 
	def_rule(H, B), 
	append(B, T, NG), 
	exact(NG, CIn, COut).
	
exact([H|T], CIn, COut) :- 
	find_rule(H, (R, S, N), B, CIn), 
	solve_pres(R, S, N, B, T, CIn, COut).



solve_pres(R, S, N, B, T, CIn, COut) :- 
	member_eq((N, R, S), CIn), !, 
	append(B, T, NG), 
	exact(NG, CIn, COut).
	
solve_pres(R, S, N, B, T, CIn, COut) :- 
	append(CIn, [(N, R, S)], C1), 
	append(B, T, NG), 
	exact(NG, C1, COut).



/* find_rule(G, (R, S, N), Body, C) 
 * --------------------------------
 * This predicate takes a goal G and the current C set and returns the index R 
 * of a disjunctive rule resolving with G together with the index N of the 
 * resolving head, the substitution S and the Body of the rule.
 */
find_rule(H, (R, S, N), Body, C) :- 
	rule(H, _P, N, R, S, _NH, _Head, Body), 
	not_already_present_with_a_different_head(N, R, S, C).

/* run_file(BDDFile, BDDParFile, Prob)
 * -----------------------------------
 * This predicate calls for the resolution of a BDD via file.
 */
run_file(BDDFile, BDDParFile, Prob) :-
	ResultFile = 'result.txt',
 	library_directory(Dir),
	setting(timeout, BDDTime),
	(BDDTime = no ->
		atomic_concat([Dir, '/LPADBDD -l ', BDDFile, ' -i ', BDDParFile,' > ', ResultFile], Command);
		atomic_concat([Dir, '/LPADBDD -l ', BDDFile, ' -i ', BDDParFile,' -t ', BDDTime,' > ', ResultFile], Command)),

	%statistics(walltime,_),

	shell(Command, Return),
	(Return =\= 0 ->
		Status = timeout,
		Prob = Status;
				
		see(ResultFile),
		read(elapsed_construction(_TimeConstruction)),
		read(probability(Prob)),
		read(elapsed_traversing(_TimeTraversing)),
		seen,
	
		%write(probability(Prob)),nl,
		%read(_),
		%delete_file(ResultFile),
	
		Status = ok
		% format("Construction time ~f traversing time ~f~Number",[TimeConstruction, TimeTraversing])
	).

	%statistics(walltime,[_,E3]),
	%format(user,'~w ms BDD processing~Number',[E3]),
	% format("Status ~a~Number",[Status]).



/* insert_list_ptree([Head|Tail], Trie)
 * ------------------------------------
 * This predicate inserts the given list in a trie.
 */ 
insert_list_ptree([], _Trie).

insert_list_ptree([Head|Tail], Trie) :-
	reverse(Head, Head1),
	insert_ptree(Head1, Trie),
	insert_list_ptree(Tail, Trie).
