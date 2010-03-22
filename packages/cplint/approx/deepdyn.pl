/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File deepit.pl
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
 
/* solve(GoalsList, ProbLow, ProbUp, ResTime, BddTime)
 * ---------------------------------------------------
 * This predicate computes the probability of a given list of goals using an 
 * iterative deepening algorithm. It also returns the number of handled BDDs and 
 * CPUTime spent in performing resolution and in handling the BDDs.
 * 
 * INPUT
 *  - GoalsList: given list of goal to work on.
 *
 * OUTPUT
 *  - ProbLow: the resulting lowerbound probability for the given list of goals.
 *  - ProbUp: the resulting upperbound probability for the given list of goals.
 *  - ResTime: cpu time spent on performing resolution.
 *  - BddTime: cpu time spent on handling BDDs.
 */ 
solve(GoalsList, ProbLow,ProbUp, ResTime, BddTime):-
	setting(min_error, MinError), 
	setting(prob_step, ProbStep),
	
	LogProbStep is log(ProbStep),
	
	assert(low(0,0.0)),
	assert(up(1.0)),
	init_ptree(1),
	
	% NB: log(1.0) == 0.0 !!!	
	deepdyn([0.0-([], [], GoalsList)], 0, MinError, LogProbStep, ProbLow, ProbUp, 0, ResTime, 0, BddTime),
	
	delete_ptree(1),
	retract(low(_,_)),
	retract(up(_)).



/* solve_t(L0,Succ,PB,ProbL0,ProbU0) L0 is a list of quadruples (CG,C,P,G) where
CG is the list of ground choices, C is the list of non-ground choices,
P is the probability of CG, G is the list of goals to be resolved,
Succ is the list of explanations found so far, PB is the current prob bound,
ProbL0,ProbU0 are the lower/upper prob bounds */

deepdyn(GoalsList, Number, MinError, ProbStep, LowerProb1, UpperProb1, ResTime0, ResTime1, BddTime0, BddTime1) :-
	% Resetting the clocks...
	statistics(walltime, [_, _]),
	
	% Performing resolution...
	findall(Prob1-(Gnd1, Var1, Goals1), 
		(member(Prob0-(Gnd0, Var0, Goals0), GoalsList), ProbBound is log(Prob0) + ProbStep, explore(ProbBound, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1))),
		List),
	
	% Taking elapsed times...
	statistics(walltime, [_, ElapResTime]),
	ResTime2 is ResTime0 + ElapResTime/1000,
	
	% Building and solving equivalent bdds...
	init_ptree(2),
	init_ptree(3),
	separate(List, [], LowerList0, [], UpperList, [], Incomplete),
	insert_list_ptree(LowerList0, 1),
	insert_list_ptree(UpperList, 2),
	merge_ptree(1,2,3),
	
	length(LowerList0, DeltaLow),
	Next is Number + DeltaLow,
	eval_lower(Next, ProbLow),	
	
	length(UpperList, DeltaUp), 
	Temp is Next + DeltaUp,
	eval_upper(Temp, ProbUp), 	
	
	delete_ptree(2),
	delete_ptree(3),	
	
	% Taking elapsed times...
	statistics(walltime, [_, ElapBddTime]),
	BddTime2 is BddTime0 + (ElapBddTime / 1000),

	% Is the current error lower than the minimum error?
	(ProbUp - ProbLow < MinError ->	
		% Setting output parameters' values...
		LowerProb1 = ProbLow,
		UpperProb1 = ProbUp,
		ResTime1 = ResTime2, 
		BddTime1 = BddTime2;
		
		% Keeping on iterating with accumulated values...
		% sufficient without negation:
		% D1 = DB, 
		% necessary for negation
		deepdyn(Incomplete, Next, MinError, ProbStep, LowerProb1, UpperProb1, ResTime2, ResTime1, BddTime2, BddTime1)).



/* separate(List, Low, Up, Next)
 * ----------------------------------
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

separate([_Prob-(Gnd0, [], [])|Tail], Low0, [Gnd0|Low1], Up0, [Gnd0|Up1], Next0, Next1) :- !, 
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate([Prob0-(Gnd0, Var0, Goals)|Tail], Low0, Low1, Up0, [Gnd1|Up1], Next0, [Prob1-(Gnd1, Var1, Goals)|Next1]) :- 
	get_groundc(Var0, Gnd2, Var1, 1.0, Prob2), 
	append(Gnd0, Gnd2, Gnd1), 
	Prob1 is Prob0 + log(Prob2), 
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).



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
		get_groundc(Var, Gnd, Var2, 1, Prob), 
		append(Gnd0, Gnd, Gnd2), 
		Prob2 is Prob0 + log(Prob), 		
		explore(ProbBound, Prob2-(Gnd2, Var2, Tail), Prob1-(Gnd1, Var1, Goals1))).
		%% Recursive call: consider next goal (building next values)

% Main, builtin
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	builtin(Head), !, 
	call(Head), 
	get_groundc(Var0, Gnd, Var2, 1, Prob), 
	append(Gnd0, Gnd, Gnd2), 
	Prob2 is Prob0 + log(Prob), 
	explore(ProbBound, Prob2-(Gnd2, Var2, Tail), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, def_rule
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	def_rule(Head, Goals0), 
	append(Goals0, Tail, Goals2), 
	get_groundc(Var0, Gnd, Var2, 1, Prob), 
	append(Gnd0, Gnd, Gnd2), 
	Prob2 is Prob0 + log(Prob), 
	explore(ProbBound, Prob2-(Gnd2, Var2, Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, find_rulec
explore(ProbBound, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	find_rulec(Head, (R, S, N), Goals, Var0, _Prob), 
	explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	
explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals)) :-
	(member_eq((N, R, S), Var0);
	member_eq((N, R, S), Gnd0)), !, 
	append(Goals, Goals0, Goals2), 
	get_groundc(Var0, Gnd, Var2, 1, Prob), 
	append(Gnd0, Gnd, Gnd2), 
	Prob2 is Prob0 + log(Prob), 
	explore(ProbBound, Prob2-(Gnd2, Var2, Goals2), Prob1-(Gnd1, Var1, Goals)).
	% Recursive call: consider next goal (building next values)

explore_pres(ProbBound, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1)) :-
	append(Var0, [(N, R, S)], Var), 
	append(Goals, Goals0, Goals2), 
	get_groundc(Var, Gnd, Var2, 1, Prob), 
	append(Gnd0, Gnd, Gnd2), 
	Prob2 is Prob0 + log(Prob), 
	explore(ProbBound, Prob2-(Gnd2, Var2, Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)



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



/* insert_list_ptree([Head|Tail], Trie)
 * ------------------------------------
 * This predicate inserts the given list in a trie.
 */ 
insert_list_ptree([], _Trie).

insert_list_ptree([Head|Tail], Trie) :-
	reverse(Head, Head1),
	insert_ptree(Head1, Trie),
	insert_list_ptree(Tail, Trie).
