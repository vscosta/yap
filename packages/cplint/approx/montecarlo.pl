/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File: montecarlo.pl
 *	Solves LPADs with Monte Carlo (main predicate: solve(Goals, Prob, Samples, ResTime, BddTime)
 *	Copyright (c) 2009, Stefano Bragaglia
 *============================================================================*/
 
/* EXTERNAL FILE
 * -------------
 * The following libraries are required by the program to work fine.
 */

:- dynamic rule/4, def_rule/2, randx/1, randy/1, randz/1.

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(ugraphs)).
:- use_module(params).

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

/* Starting seed. */
randx(1).
randy(1).
randz(1).

/* newsample
 * ---------
 * This predicate programmatically generates and sets a new seed for the 
 * algorithm.
 */
newsample :-
	retract(randx(X)),
	randy(Y),
	randz(Z),
	(X =< 30269 ->
		SuccX is X + 1,
		assert(randx(SuccX));
		assert(randx(1)),
		retract(randy(_)),
		(Y =< 30307 ->
			SuccY is Y + 1,
			assert(randy(SuccY));
			assert(randy(1)),
			retract(randz(_)),
			(Z =< 30323 ->
				SuccZ is Z + 1,
				assert(randz(SuccZ));
				assert(randz(1))))),
	setrand(rand(X, Y, Z)).


/* solve(Goals, Samples, Time, Lower, Prob, Upper)
 * -----------------------------------------------
 * This predicate calls the Monte Carlo solving method for the current problem.
 * It requires the Goals to fulfil and returns the number of Samples considered,
 * the Time required, the extimated Probability and the Lower and Upper bounds.
 *
 * INPUT
 *  - Goals: given list of goals to fulfil.
 *
 * OUTPUT
 *  - Samples: number of samples considered to solve the problem.
 *  - Time: time required to solve the problem.
 *  - Lower: lower end of the confidence interval.
 *  - Prob: extimated probability.
 *  - Upper: upper end of the confidence interval.
 */
solve(Goals, Samples, Time, Lower, Prob, Upper) :-
	% Retrieving functional parameters...
	setting(k, K),
	setting(min_error, MinError),
	% Resetting the clocks...
	statistics(walltime, [_, _]),	
	% Performing resolution...
	montecarlo(0, 0, Goals, K, MinError, Samples, Lower, Prob, Upper),	
	% Taking elapsed times...
	statistics(walltime, [_, ElapTime]),
	% Setting values...
	Time is ElapTime/1000.



/* montecarlo(Count, Success, Goals, K, MinError, Samples, Lower, Prob, Upper)
 * ---------------------------------------------------------------------------
 * This tail recursive predicate solves the problem currently in memory with a
 * Monte Carlo approach. 
 * It requires the number of samples and successes so far (Count and Success),
 * the desired list of Goals to fulfil, the number K of samples to consider at
 * once and the threshold MinError for the binomial proportion confidence 
 * interval.
 * It returns the total number of Samples considered, the Lower and Upper ends 
 * of the the binomial proportion confidence interval and the extimated Prob.
 * 
 * INPUT
 *  - Count: number of samples considered so far.
 *  - Success: number of successfull samples considered so far.
 *  - Goals: list of goals to fulfil.
 *  - K: number of samples to consider at once.
 *  - MinError: threshold for the binomial proportion confidence interval.
 * 
 * OUTPUT
 *  - Samples: total number of samples considered.
 *  - Lower: lower end of the the binomial proportion confidence interval.
 *  - Prob: extimated probability.
 *  - Upper: upper end of the the binomial proportion confidence interval.
 *
 * NB: This method is based on the binomial proportion confidence interval and
 *     the Brown's rule of thumb to avoid the case the sample proportion is 
 *     exactly 0.0 or 1.0 and doesn't make use of BDDs.
 */
montecarlo(Count, Success, Goals, K, MinError, Samples, Lower, Prob, Upper) :-
	/* Decomment the following line if you want to test the algorithm with an
	   incremental seed for each sample.
	newsample,
	*/
	main(Goals, [], _Explan, Valid),
	N is Count + 1,
	S is Success + Valid,
	(N mod K =:= 0 ->
%		format("Advancement: ~t~d/~d~30+~n", [S, N]),
		P is S / N,
		D is N - S,
		Semi is 2 * sqrt(P * (1 - P) / N),
		Int is 2 * Semi,
	/*   N * P > 5;   N * S / N > 5;   S > 5
	 *   N (1 - P) > 5;   N (1 - S / N) > 5;   N (N - S) / N > 5;   N - S > 5
	 */
		((S > 5, D > 5, (Int < MinError; Int =:= 0)) ->
			Samples is N,
			Lower is P - Semi,
			Prob is P,
			Upper is P + Semi;
			montecarlo(N, S, Goals, K, MinError, Samples, Lower, Prob, Upper));
		montecarlo(N, S, Goals, K, MinError, Samples, Lower, Prob, Upper)).



/* null
 * ----
 * This is dummy predicate to use sparingly when needed. 
 * Typical uses are as spying predicate during tracing or as dead branch in 
 * ( -> ; ) predicate.
 */	
null.



/* main(Goals, Explan0, Explan1, Valid)
 * ------------------------------------
 * This tail recursive predicate looks for a solution to the given Goals
 * starting from the given Explan0 and returns the final Explan and 1 (0 otherwise) if it is a 
 * Valid sample for Montecarlo.
 */
main([], Explan, Explan, 1). 

main([\+ Goal|Tail], Explan0, Explan1, Valid) :-
	builtin(Goal), !,
	(call((\+ Goal)) ->
		main(Tail, Explan0, Explan1, Valid);
		Explan1 = Explan0,
		Valid = 0).
	
main([Goal|Tail], Explan0, Explan1, Valid) :-
	builtin(Goal), !,
	(call(Goal) ->
		main(Tail, Explan0, Explan1, Valid);
		Explan1 = Explan0,
		Valid = 0).

main([Goal|Tail], Explan0, Explan1, Valid) :-
	findall((IsSample, Goals, Step), explore([Goal|Tail], Explan0, IsSample, Goals, Step), List), 
	cycle(List, Explan0, Explan1, Valid).

	

/* explore([Goal|Tail], Explan, Valid, Goals, Step)
 * ------------------------------------------------
 * This predicate looks for a Body and the Step to reach it from the given Goal 
 * and Explan and returns 1 (0 otherwise) if they are a Valid sample for
 * Montecarlo. 
 * Please note that Body and Step are meaningfull only when Valid is 1.
 *
 * This comment has to be fixed.
 */
explore([Goal|Tail], _Explan, 1, Goals, []) :-
	def_rule(Goal, Body),
	append(Body, Tail, Goals).

explore([Goal|Tail], Explan, Valid, Goals, Step) :- 
	findrule(Goal, Explan, Valid, Body, (HeadID, RuleID, Subst)),
	append(Body, Tail, Goals),
	(member_eq((HeadID, RuleID, Subst), Explan) ->
		Step = [];
		Step = [(HeadID, RuleID, Subst)]).



/* findrule(Goal, Explan, Valid, Body, (HeadID, RuleID, Subst))
 * ---------------------------------------------------------------
 * This predicate finds a rule that matches with the given Goal and Explan and
 * returns 1 (0 otherwise) if it is a Valid sample for Montecarlo.
 * If the sample is Valid, the other return parameters are also meaningfull and
 * are the Body and (RuleID, Subst, HeadIS) of the rule that matches with the 
 * given Goal and Explan.
 *
 * This comment has to be fixed.
 */
findrule(Goal, Explan, Valid, Body, (HeadId, RuleId, Subst)) :-
	rule(Goal, _Prob, Required, RuleId, Subst, _Heads, HeadsList, Body),
	sample(HeadsList, HeadId),
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Explan),
	(HeadId =:= Required ->
		Valid = 1;
		Valid = 0).
	


/* sample(Heads, RuleId, HeadId, Subst)
 * ------------------------------------
 * This tail recursive predicate samples a random head among the given Heads of
 * the given RuleId and returns its HeadId and Subst.
 */
sample(HeadList, HeadId) :-
	random(Prob), 
	sample(HeadList, 0, 0, Prob, HeadId), !.

sample([_HeadTerm:HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index;
		sample(Tail, Succ, Next, Prob, HeadId)).



/* cycle([(IsSample, Body, [Step])|Tail], Explan0, Explan1, Found)
 * -----------------------------------------------------------------
 * This tail recursive predicate analyzes the given Body and Step to reach it
 * and returns 0 as it's not a Valid sample for Montecarlo. 
 * If it is Valid, it looks for a solution to the Body and the given Goals 
 * starting from the Step and the given Explan and returns 1 if it finds a
 * Valid one.
 * If it does not find it, it considers the next Body and Step and returns their
 * Valid value.
 *
 * NB: This comment needs to be updated.
 */
cycle([], Explan, Explan, 0).

cycle([(0, _Goals, Step)|Tail], Explan0, Explan1, IsSample) :- !,
	append(Step, Explan0, Explan2),
	cycle(Tail, Explan2, Explan1, IsSample).

cycle([(1, Goals, Step)|Tail], Explan0, Explan1, IsSample) :- 
	append(Step, Explan0, Explan),
	main(Goals, Explan, Explan2, Valid),
	(Valid == 1 ->
		Explan1 = Explan2,
		IsSample = 1;
		cycle(Tail, Explan2, Explan1, IsSample)).
