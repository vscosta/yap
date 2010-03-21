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

/* solve(Goals, Prob, ResTime, BddTime)
 * ------------------------------------
 * This predicate computes the probability of a given list of goals using an 
 * iterative deepening, probability bounded algorithm.
 * It also returns the number of handled BDDs and the CPUTime spent performing 
 * resolution and spent handling the BDDs.
 *
 * Note: when their derivation is cut, negative goals are added to the head of
 *	   the goals' list to be solved during the following iteration.
 * 
 * INPUT
 *  - GoalsList: given list of goal to work on. It can contains variables: the 
 *	  predicate returns in backtracking all the solutions and their equivalent
 *	  lower and upper bound probability.
 *
 * OUTPUT
 *  - Prob: resulting lower bound probability for the given list of goals.
 *  - ResTime: CPU time spent on performing resolution.
 *  - BddTime: CPU time spent on handling BDDs.
 */
solve(Goals, Prob, ResTime, BddTime) :-
	setting(k, K),
	setting(prob_step, ProbStep),
	ProbStepLog is log(ProbStep),
	% NB: log(1.0) == 0.0 !!!
	bestk([0.0-0.0-([], [], Goals)], K, ProbStepLog, Prob, ResTime, BddTime).



/* bestk(GoalsList, K, ProbStep, Prob, ResTime, BddTime)
 * -----------------------------------------------------
 * This recursive supporting predicate performs resolution for current iteration, 
 * sticks with the best complete solutions only and considers their equivalent 
 * BDD to compute their probability.
 *
 * INPUT
 *  - GoalsList: given list of goal to work on.
 *  - K: number of solution to consider.
 *  - ProbStep: value used to update the probability bound.
 *
 * OUTPUT
 *  - Prob: resulting probability (actaully a lower bound) for the given list of goals.
 *  - ResTime: cpu time spent on performing resolution.
 *  - BddTime: cpu time spent on handling BDDs.
 */
bestk(GoalsList, K, ProbStep, Prob, ResTime, BddTime) :-
	% Resetting the clocks...
	statistics(walltime, [_, _]),
	
	% Performing resolution...
	main(GoalsList, K, ProbStep, BestK),
	
	% Taking elapsed times...
	statistics(walltime, [_, ElapResTime]),
	ResTime is ElapResTime/1000,
	
	% Building and solving equivalent bdds...
	init_ptree(1),
	insert_full_ptree(BestK, 1),
	bdd_ptree_map(1, 'bdd.txt', 'bdd.par', 'bdd.map'),
	delete_ptree(1),
	run_file('bdd.txt','bdd.par', Temp),
	(Temp == timeout ->
		Prob is -1.0;
		Prob is Temp),	
	
	% Taking elapsed times...
	statistics(walltime, [_, ElapBddTime]),
	BddTime is (ElapBddTime / 1000).



/* main(Goals, K, ProbStep, Best)
 * ------------------------------
 * This tail recursive predicate returns the Best K complete solutions to the 
 * given Goals. The probability bound is dinamically computed at each iteration.
 *
 * INPUT
 *  - Goals: list of goals to achive.
 *  - K: desired number of solutions.
 *  - ProbStep: value used to update the probability bound.
 *
 * OUTPUT
 *  - Best: list of best solutions (at most k).
 */
main(Goals, K, ProbStep, Best) :-
	K > 0, 
	main(Goals, ProbStep, K, 0.0, [], Best).

main([], _ProbStep, _Left, _Worst, Best, Best).

main(Goals, ProbStep, Left0, Worst0, Best0, Best1) :-
	findall(Prob1-Bound-(Gnd1, Var1, Goals1),
			(member(Prob0-Bound0-(Gnd0, Var0, Goals0), Goals), Bound is Bound0+ ProbStep, explore(Bound, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1))), 
			Found),
	%% sepkeepbest(Found, Left0, Left2, Worst0, Worst2, Best0, Best2, [], Incomplete),
	separate_main(Found, [], Complete, [], _UpperList, [], Incomplete),	
	keepbest(Complete, Left0, Left2, Worst0, Worst2, Best0, Best2),	
	main(Incomplete, ProbStep, Left2, Worst2, Best2, Best1).



/* sepkeepbest(Found, Left0, Left1, Worst0, Worst1, List0, List1, Next0, Next1)
 * ----------------------------------------------------------------------------
 * This tail recursive predicate analyzes the given list of solutions found and
 * returns the list of (K at most) best complete solutions and the whole list of 
 * incomplete solutions for a full B&B behaviour.
 * The given worst value permits some optimization, such as immediate skipping 
 * of very bad solutions.
 *
 * INPUT
 *  - Found: list of solutions found.
 *  - Left0: actual amount of items still needed to have K solutions.
 *  - Worst0: value of the actual worst complete solution kept.
 *  - List0: actual list of best complete solutions.
 *  - Next0: actual list of incomplete solutions.
 *
 * OUTPUT
 *  - Left1: final amount of items still needed to have K solutions.
 *  - Worst1: value of the final worst complete solution kept.
 *  - List1: final list of best complete solutions.
 *  - Next1: final list of incomplete solutions.
 */
sepkeepbest([], Left, Left, Worst, Worst, List, List, Next, Next) :- !.
%% Closing condition: stop if no more results (current values are now final values).

sepkeepbest([Prob0-(_Gnd0, [], [])|Tail], 0, Left1, Worst0, Worst1, List0, List1, Next0, Next1) :-
	Prob0 =< Worst0, !,
	sepkeepbest(Tail, 0, Left1, Worst0, Worst1, List0, List1, Next0, Next1).
	
sepkeepbest([Prob0-(Gnd0, [], [])|Tail], 0, Left1, Worst0, Worst1, List0, List1, Next0, Next1) :-
	Prob0 > Worst0, !,
	discard(Prob0-(Gnd0, [], []), List0, List2, Worst2),
	sepkeepbest(Tail, 0, Left1, Worst2, Worst1, List2, List1, Next0, Next1).

sepkeepbest([Prob0-(Gnd0, [], [])|Tail], Left0, Left1, Worst0, Worst1, List0, List1, Next0, Next1) :- !,
	insert(Prob0-(Gnd0, [], []), List0, Worst0, List2, Worst2),
	Left2 is Left0 - 1,
	sepkeepbest(Tail, Left2, Left1, Worst2, Worst1, List2, List1, Next0, Next1).

sepkeepbest([Prob0-(Gnd0, Var0, Goals)|Tail], Left0, Left1, Worst0, Worst1, List0, List1, Next0, [Prob1-(Gnd1, Var1, Goals)|Next1]) :- 
	get_groundc(Var0, Gnd2, Var1, 1.0, Prob2), 
	append(Gnd0, Gnd2, Gnd1), 
	Prob1 is Prob0 + log(Prob2), 
	sepkeepbest(Tail, Left0, Left1, Worst0, Worst1, List0, List1, Next0, Next1).
	


/* separate(List, Low, Up, Next)
 * -----------------------------
 * This tail recursive predicate parses the input list and builds the list for
 * the lower bound, the upper bound and the pending goals.
 * The upper bound list contains both the items of the lower bound list and the
 * incomplete ones.
 *
 * INPUT
 *  - List: input list.
 *
 * OUTPUT
 *  - Low: list for lower bound.
 *  - Up: list for upper bound.
 *  - Next: list of pending goals.
 */
separate(List, Low, Up, Next) :-
%% Polarization: initial low, up and next lists are empty.
	separate(List, [], Low, [], Up, [], Next).

separate([], Low, Low, Up, Up, Next, Next) :- !.
%% Closing condition: stop if no more results (current lists are now final lists).

separate([Prob0-(Gnd0, [], [])|Tail], Low0, [Gnd0|Low1], Up0, [Prob0-(Gnd0, [], [])|Up1], Next0, Next1) :- !, 
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate([Prob0-(Gnd0, Var0, Goals)|Tail], Low0, Low1, Up0, [Prob0-(Gnd0, Var0, Goals)|Up1], Next0, [Prob0-(Gnd0, Var0, Goals)|Next1]) :- 
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate_main([], Low, Low, Up, Up, Next, Next) :- !.
%% Closing condition: stop if no more results (current lists are now final lists).

separate_main([Prob0-_Bound0-(Gnd0, [], [])|Tail], Low0, [Prob0-(Gnd0, [], [])|Low1], Up0, [Prob0-(Gnd0, [], [])|Up1], Next0, Next1) :- !, 
	separate_main(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate_main([Prob0-Bound0-(Gnd0, Var0, Goals)|Tail], Low0, Low1, Up0, [Prob0-Bound0-(Gnd0, Var0, Goals)|Up1], Next0, [Prob0-Bound0-(Gnd0, Var0, Goals)|Next1]) :- 
	separate_main(Tail, Low0, Low1, Up0, Up1, Next0, Next1).



/* explore(ProbBound, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1))
 * --------------------------------------------------------------------------
 * This tail recursive predicate reads current explanation and returns the 
 * explanation after the current iteration without dropping below the given 
 * probability bound.
 *
 * INPUT
 *  - ProbBound: the desired probability bound;
 *  - Prob0-(Gnd0, Var0, Goals0): current explanation
 *      - Gnd0: list of current ground choices,
 *      - Var0: list of current non-ground choices,
 *      - Prob0: probability of Gnd0,
 *      - Goals0: list of current goals.
 *
 * OUTPUT
 *  - Prob1-(Gnd1, Var1, Prob1, Goals1): explanation after current iteration
 *      - Gnd1: list of final ground choices,
 *      - Var1: list of final non-ground choices,
 *      - Prob1: probability of Gnd1,
 *      - Goals1: list of final goals.
 */
explore(_ProbBound, Prob-(Gnd, Var, []), Prob-(Gnd, Var, [])) :- !. 
%% Closing condition: stop if no more goals (input values are output values).

explore(ProbBound, Prob-(Gnd, Var, Goals), Prob-(Gnd, Var, Goals)) :-
	%% Closing condition: stop if bound has been reached (input values are output values).
	Prob =< ProbBound, !. 

% Negation, builtin
explore(ProbBound, Prob0-(Gnd0, Var0, [\+ Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	builtin(Head), !, 
	call((\+ Head)), 
	explore(ProbBound, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	%% Recursive call: consider next goal (building next values)

% Negation
explore(ProbBound, Prob0-(Gnd0, Var0, [\+ Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :- !, 
	list2and(HeadList, Head), % ...
	findall(Prob-(Gnd, Var, CurrentGoals), explore(ProbBound, 0.0-([], [], HeadList), Prob-(Gnd, Var, CurrentGoals)), List) ->
	separate(List, [], LowerBound, [], _UpperBound, [], PendingGoals), 
	(PendingGoals \= [] ->
		Var2 = Var0, 
		Gnd2 = Gnd0, 
		Goals1 = [\+ Head|Goals], 
		explore(ProbBound, Prob0-(Gnd2, Var2, Tail), Prob1-(Gnd1, Var1, Goals));
		%% Recursive call: consider next goal (building next values)
	
		choose_clausesc(Gnd0, Var0, LowerBound, Var), 
		get_prob(Var,  1, Prob), 
		append(Gnd0, Var, Gnd2), 
		Prob2 is Prob0 + log(Prob), 		
		explore(ProbBound, Prob2-(Gnd2, [], Tail), Prob1-(Gnd1, Var1, Goals1))).
		%% Recursive call: consider next goal (building next values)

% Main, builtin
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	builtin(Head), !, 
	call(Head), 
	explore(ProbBound, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, def_rule
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	def_rule(Head, Goals0), 
	append(Goals0, Tail, Goals2), 
	explore(ProbBound, Prob0-(Gnd0, Var0, Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, find_rulec
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	find_rulec(Head, (R, S, N), Goals, Var0, _Prob), 
	explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	
explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals)) :-
	(member_eq((N, R, S), Var0);
	member_eq((N, R, S), Gnd0)), !, 
	append(Goals, Goals0, Goals2), 
	explore(ProbBound, Prob0-(Gnd0, Var0, Goals2), Prob1-(Gnd1, Var1, Goals)).
	% Recursive call: consider next goal (building next values)

explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1)) :-
	append(Var0, [(N, R, S)], Var), 
	append(Goals, Goals0, Goals2), 
	get_prob(Var,  1, Prob), 
	append(Gnd0, Var, Gnd2), 
	Prob2 is Prob0 + log(Prob), 
	explore(ProbBound, Prob2-(Gnd2, [], Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)



/* keepbest(List, K, BestK)
 * ------------------------
 * This tail recursive predicate parses the given list of quads and returns the
 * list of its best k quads. If the given list of quads contains less than k
 * items, the predicate returns them all.
 *
 * INPUT
 *  - List: list of quads to parse.
 *  - K: desired number of quads.
 *
 * OUTPUT
 *  - BestK: final list of (at most) best k quads.
 */
keepbest(List, K, BestK) :-
	K > 0,
	keepbest(List, K, _Left, 0.0, _Worst, [], BestK).

/*keepbest([], _Left, _Worst, List, List).

keepbest([Prob-(_Gnd, _Var, _Goals)|Tail], 0, Worst, List0, List1) :-
	Prob =< Worst, !,
	keepbest(Tail, 0, Worst, List0, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], 0, Worst, List0, List1) :-
	Prob > Worst, !,
	discard(Prob-(Gnd, Var, Goals), List0, List2, Worst2),
	keepbest(Tail, 0, Worst2, List2, List1).
	
keepbest([Prob-(Gnd, Var, Goals)|Tail], Left, Worst, List0, List1) :-
	insert(Prob-(Gnd, Var, Goals), List0, Worst, List2, Worst2),
	Left2 is Left - 1,
	keepbest(Tail, Left2, Worst2, List2, List1).*/



keepbest([], Left, Left, Worst, Worst, List, List).

keepbest([Prob-(_Gnd, _Var, _Goals)|Tail], 0, Left1, Worst0, Worst1, List0, List1) :-
	Prob =< Worst0, !,
	keepbest(Tail, 0, Left1, Worst0, Worst1, List0, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], 0, Left1, Worst0, Worst1, List0, List1) :-
	Prob > Worst0, !,
	discard(Prob-(Gnd, Var, Goals), List0, List2, Worst2),
	keepbest(Tail, 0, Left1, Worst2, Worst1, List2, List1).
	
keepbest([Prob-(Gnd, Var, Goals)|Tail], Left0, Left1, Worst0, Worst1, List0, List1) :-
	insert(Prob-(Gnd, Var, Goals), List0, Worst0, List2, Worst2),
	Left2 is Left0 - 1,
	keepbest(Tail, Left2, Left1, Worst2, Worst1, List2, List1).



/* insert(Prob-(Gnd, Var, Goals), Sorted0, Worst0, Sorted1, Worst1)
 * ----------------------------------------------------------------
 * This tail recursive predicate inserts the given quad into the given sorted
 * list and returns the final sorted list. The input list must be sorted.
 * It also updates the prob value of the worst quad.
 * 
 * INPUT
 *  - Prob-(Gnd, Var, Goals): quad to insert.
 *  - Sorted0: sorted list to insert the quad into.
 *  - Worst0: current worst prob value.
 *
 * OUTPUT
 *  - Sorted1: the final sorted list.
 *  - Worst1: the final worst prob value.
 */
insert(Prob-(Gnd, Var, Goals), [], _Worst, [Prob-(Gnd, Var, Goals)], Prob).

insert(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst, [Prob-(Gnd, Var, Goals), Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst) :-
	Prob >= Prob_i, !.

insert(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst0, [Prob_i-(Gnd_i, Var_i, Goals_i)|Next], Worst1) :-
	Prob < Prob_i, !, 	
	insert(Prob-(Gnd, Var, Goals), Tail, Worst0, Next, Worst1).



/* discard(Prob-(Gnd, Var, Goals), Sorted0, Sorted1, Worst)
 * --------------------------------------------------------
 * This tail recursive predicate inserts the given quad into the given sorted
 * list, removes the last quad from it and returns the final sorted list.
 * The given sorted list contains at least one quad and must be sorted.
 * Previous worst prob value is not needed because it necessarely changes and
 * the new value is not known in advance.
 * It also updates the prob value of the worst quad.
 * 
 * INPUT
 *  - Prob-(Gnd, Var, Goals): quad to insert.
 *  - Sorted0: sorted list to insert the quad into.
 *
 * OUTPUT
 *  - Sorted1: the final sorted list.
 *  - Worst: the final worst prob value.
 */
discard(Prob-(Gnd, Var, Goals), [_Prob_i-(_Gnd_i, _Var_i, _Goals_i)], [Prob-(Gnd, Var, Goals)], Prob) :- !.

discard(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i), Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], [Prob-(Gnd, Var, Goals)|Next], Worst) :-
	Prob >= Prob_i, !, 
	discard(Prob_i-(Gnd_i, Var_i, Goals_i), [Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], Next, Worst).
	
discard(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i), Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], [Prob_i-(Gnd_i, Var_i, Goals_i)|Next], Worst) :-
	Prob < Prob_i, !,
	discard(Prob-(Gnd, Var, Goals), [Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], Next, Worst).



/* eval_lower(Number, Prob)
 * ------------------------
 * This predicate evaluates if there are proofs for the lower bound by 
 * running an external command (BDD resolution via files).
 */
eval_lower(Number, Prob) :- 
	low(Number, Prob).
	
eval_lower(Number, ProbLow) :-
	Number > 0,
	low(OldNumber, _),
	Number \= OldNumber,
	bdd_ptree_map(1, 'bddl.txt', 'bddl.par', 'bddl.map'),
	run_file('bddl.txt', 'bddl.par', NewProbLow),
	(NewProbLow = timeout ->
		low(_, ProbLow);
		
		ProbLow = NewProbLow,
		retract(low(_, _)),
		assert(low(Number, ProbLow))).



/* eval_upper(Number, Prob)
 * ------------------------
 * This predicate evaluates if there are proofs for the upper bound by 
 * running an external command (BDD resolution via files).
 */
eval_upper(0, ProbUp) :- !,
	low(_, ProbUp).

eval_upper(_Number, ProbUp) :-
	bdd_ptree_map(3, 'bddu.txt', 'bddu.par', 'bddu.map'),
	run_file('bddu.txt', 'bddu.par', NewProbUp),
	(NewProbUp = timeout->
		up(ProbUp);
		ProbUp = NewProbUp,
		retract(up(_)),
		assert(up(ProbUp))).



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



/* insert_full_ptree([Head|Tail], Trie)
 * ------------------------------------
 * This predicate inserts the ground part of the given list in a trie.
 */
insert_full_ptree([], _Trie).
	
insert_full_ptree([_Prob-(Gnd, _Var, _Goals)|Tail], Trie) :-
	reverse(Gnd, Gnd1),
	insert_ptree(Gnd1, Trie),
	insert_full_ptree(Tail, Trie).



/* insert_list_ptree([Head|Tail], Trie)
 * ------------------------------------
 * This predicate inserts the given list in a trie.
 */ 
insert_list_ptree([], _Trie).

insert_list_ptree([Head|Tail], Trie) :-
	reverse(Head, Head1),
	insert_ptree(Head1, Trie),
	insert_list_ptree(Tail, Trie).


