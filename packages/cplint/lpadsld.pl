/*
	LPAD and CP-Logic reasoning suite
	File lpadsld.pl
	Goal oriented interpreter for LPADs based on SLDNF
	Copyright (c) 2007, Fabrizio Riguzzi
*/
%:- set_prolog_flag(debug,on).
%:- set_prolog_flag(discontiguous_warnings,on).
%:- set_prolog_flag(single_var_warnings,on).
%:- source.
:-dynamic rule/5,rule_by_num/8,rule_uniform/8,def_rule/2,setting/2.

:-use_module(library(lists)).
:-use_module(library(ugraphs)).

:-load_foreign_files(['cplint'],[],init_my_predicates).

/* start of list of parameters that can be set by the user with
set(Parameter,Value) */
setting(epsilon_parsing,0.00001).
setting(save_dot,false).
setting(ground_body,true). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */
setting(min_error,0.01).
setting(initial_depth_bound,4).
setting(depth_bound,4).
setting(prob_threshold,0.00001).
setting(prob_bound,0.01).

/* end of list of parameters */

/* s(GoalsLIst,Prob) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with their
corresponding probability */
s(GoalsList,Prob):-
	solve(GoalsList,Prob).


solve(GoalsList,Prob):-
	setof(Deriv,find_deriv(GoalsList,Deriv),LDup),
	rem_dup_lists(LDup,[],L),
	build_formula(L,Formula,[],Var),
	var2numbers(Var,0,NewVar),
	(setting(save_dot,true)->
		format("Variables: ~p~n",[Var]),
		compute_prob(NewVar,Formula,Prob,1)
	;
		compute_prob(NewVar,Formula,Prob,0)
	).

solve(GoalsList,0.0):-
	\+ find_deriv(GoalsList,_Deriv).

/* s(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with 
their corresponding probability 
CPUTime1 is the cpu time for performing resolution
CPUTime2 is the cpu time for elaborating the BDD 
WallTime1 is the wall time for performing resolution
WallTime2 is the wall time for elaborating the BDD */


s(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	solve(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2).


solve(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(setof(Deriv,find_deriv(GoalsList,Deriv),LDup)->
		rem_dup_lists(LDup,[],L),
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		%print_mem,
		build_formula(L,Formula,[],Var,0,Conj),
		length(L,ND),
		length(Var,NV),
		format(user_error,"Disjunctions :~d~nConjunctions: ~d~nVariables ~d~n",[ND,Conj,NV]),
		var2numbers(Var,0,NewVar),
		(setting(save_dot,true)->
			format("Variables: ~p~n",[Var]),
			compute_prob(NewVar,Formula,Prob,1)
		;
			compute_prob(NewVar,Formula,Prob,0)
		),
		statistics(cputime,[_,CT2]),
		CPUTime2 is CT2/1000,
		statistics(walltime,[_,WT2]),
		WallTime2 is WT2/1000
	;
		%print_mem,
		Prob=0.0,
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		CPUTime2 =0.0,
		statistics(walltime,[_,WT2]),
		WallTime2 =0.0
	),!.
	/*,
	format(user_error,"~nMemory after inference~n",[]),
	print_mem.*/

/* iterative deepening, depth bounded
for negative goals, if their derivation is cut, then they are
added to the head of the list of goals to be resolved at the next depth bound*/
si(GoalsList,ProbL,ProbU,CPUTime):-
        statistics(cputime,[_,_]),
	setting(initial_depth_bound,D),
        solve_i([(GoalsList,[])],[],D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.


/* solve_i(L0,Succ,D,ProbL0,ProbU0): L0 is a list of couples (G,Der) where
G is a list of goals to be resolved and Der is an explanation, D is the 
current depth, ProbL0 is the lower bound of the prob and ProbU0 is the upper
bound 
*/
solve_i(L0,Succ,D,ProbL0,ProbU0):-
	findall((G1,Deriv),(member((G0,C0),L0),solvei(G0,D,C0,Deriv,G1)),L),
	%	print_mem,
	separate_ulbi(L,[],LL0,[],LU,[],Incomplete),
	append(Succ,LL0,LL),
	compute_prob_deriv(LL,ProbL),
	append(Succ,LU,LU1),
	compute_prob_deriv(LU1,ProbU),
	Err is ProbU-ProbL,
	setting(min_error,ME),
	(Err<ME->
		ProbU0=ProbU,
		ProbL0=ProbL
	;
		setting(depth_bound,DB),
		D1 is D+DB,
		solve_i(Incomplete,LL,D1,ProbL0,ProbU0)
	).

/* iterative deepening, problog style: each time
the derivation is restarted from the original goal */
sir(GoalsList,ProbL,ProbU,CPUTime):-
        statistics(cputime,[_,_]),
	setting(depth_bound,D),
        solveir(GoalsList,D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.


/* solveir(GoalsList,D,ProbL0,ProbU0) GoalsLIst is the list
of goals to be derived, D is the depth bound, ProbL0,ProbU0 are the lower
and upper bound. If for a certain depth bound the error is not smaller
than the threshold, the depth bound is increased and the derivation is 
restarted from the beginning */
solveir(GoalsList,D,ProbL0,ProbU0):-
	(setof(Deriv,find_derivr(GoalsList,D,Deriv),LDup)->
		rem_dup_lists(LDup,[],L),
	%	print_mem,
		separate_ulb(L,[],LL,[],LU),
		compute_prob_deriv(LL,ProbL),
		compute_prob_deriv(LU,ProbU),
		Err is ProbU-ProbL,
		setting(min_error,ME),
		(Err<ME->
			ProbU0=ProbU,
			ProbL0=ProbL
		;
			setting(depth_bound,DB),
			D1 is D+DB,
			solveir(GoalsList,D1,ProbL0,ProbU0)
		)
	;
	%	print_mem,
		ProbL0=0.0,
		ProbU0=0.0
	).

/* approximate algorithm cilog2 style: the explanations with a prob below the
threshold are cut  */
sic(GoalsList,ProbL,ProbU,CPUTime):-
        statistics(cputime,[_,_]),
	setting(depth_bound,D),
        solveic(GoalsList,D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.

solveic(GoalsList,D,ProbL0,ProbU0):-
	(setof((Deriv,P,Pruned),solvec(GoalsList,D,[],Deriv,1.0,P,Pruned),L)->
	%	print_mem,
		separate_ulbc(L,[],LL,0,Err),
		compute_prob_deriv(LL,ProbL0),
		ProbU0 is ProbL0+Err
		/*(ProbU>1.0->
			ProbU0=1.0
		;
			ProbU0=ProbU
		)*/
	;
	%	print_mem,
		ProbL0=0.0,
		ProbU0=0.0
	).

compute_prob_deriv(LL,ProbL):-
	build_formula(LL,FormulaL,[],VarL,0,_ConjL),
	%length(LL,NDL),
	%length(VarL,NVL),
	%format(user_error,"Disjunctions :~d~nConjunctions: ~d~nVariables ~d~n",[NDL,ConjL,NVL]),
	var2numbers(VarL,0,NewVarL),
	(FormulaL=[]->
		ProbL=0.0
	;
		(FormulaL=[[]|_]->
			ProbL=1.0
		;
			(setting(save_dot,true)->
			%	format("Variables: ~p~n",[VarL]),
				compute_prob(NewVarL,FormulaL,ProbL,1)
			;
				compute_prob(NewVarL,FormulaL,ProbL,0)
			)
		)
	).

print_mem:-
	statistics(global_stack,[GS,GSF]),
	statistics(local_stack,[LS,LSF]),
	statistics(heap,[HP,HPF]),
	statistics(trail,[TU,TF]),
	format(user_error,"~nGloabal stack used ~d execution stack free: ~d~n",[GS,GSF]),
	format(user_error,"Local stack used ~d execution stack free: ~d~n",[LS,LSF]),
	format(user_error,"Heap used ~d heap free: ~d~n",[HP,HPF]),
	format(user_error,"Trail used ~d Trail free: ~d~n",[TU,TF]).

find_deriv(GoalsList,Deriv):-
	solve(GoalsList,[],DerivDup),
	remove_duplicates(DerivDup,Deriv). 

find_derivr(GoalsList,DB,Deriv):-
        solver(GoalsList,DB,[],DerivDup),
	remove_duplicates(DerivDup,Deriv).


/* duplicate can appear in the C set because two different unistantiated clauses may become the 
same clause when instantiated */

/* sc(Goals,Evidence,Prob) compute the conditional probability of the list of goals
Goals given the list of goals Evidence 
Goals and Evidence can have variables, sc returns in backtracking all the solutions with their
corresponding probability 
*/
sc(Goals,Evidence,Prob):-
	solve_cond(Goals,Evidence,Prob).

solve_cond(Goals,Evidence,Prob):-
	(setof(DerivE,find_deriv(Evidence,DerivE),LDupE)->
		rem_dup_lists(LDupE,[],LE),
		(setof(DerivGE,find_deriv_GE(LE,Goals,DerivGE),LDupGE)->
			%print_mem,
			rem_dup_lists(LDupGE,[],LGE),
			build_formula(LE,FormulaE,[],VarE),
			var2numbers(VarE,0,NewVarE),
			build_formula(LGE,FormulaGE,[],VarGE),
			var2numbers(VarGE,0,NewVarGE),
			compute_prob(NewVarE,FormulaE,ProbE,0),
			call_compute_prob(NewVarGE,FormulaGE,ProbGE),
			Prob is ProbGE/ProbE
		;
			%print_mem,
			Prob=0.0
		)
	;
		%print_mem,
		Prob=undefined
	).
	/*,
	format(user_error,"~nMemory after inference~n",[]),
	print_mem. */

sci(Goals,Evidence,ProbL,ProbU,CPUTime):-
	statistics(cputime,[_,_]),
	setting(depth_bound,D),
	append(Goals,Evidence,GE),
        solve_condi([(GE,[])],[(Evidence,[])],[],[],D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.

solve_condi(LGoals,LEvidence,SuccGE,SuccE,D,ProbL0,ProbU0):-
	findall((GE1,DerivE),
		(member((GE,CE),LEvidence),solvei(GE,D,CE,DerivE,GE1)),
		LE),
	findall((GE1,DerivE),
		(member((GE,CE),LGoals),solvei(GE,D,CE,DerivE,GE1)),
		LGE),
	separate_ulbi(LE,[],LLE0,[],LUE0,[],IncE),
	append(SuccE,LUE0,LUE),
	compute_prob_deriv(LUE,ProbUE),
	(ProbUE\==0.0->
		separate_ulbi(LGE,[],LLGE0,[],LUGE0,[],IncGE),
		append(SuccGE,LLGE0,LLGE),
		compute_prob_deriv(LLGE,ProbLGE),
		ProbL is ProbLGE/ProbUE,
		append(SuccE,LLE0,LLE),
		compute_prob_deriv(LLE,ProbLE),
		(ProbLE\==0.0->
			append(SuccGE,LUGE0,LUGE),
			compute_prob_deriv(LUGE,ProbUGE),
			ProbU1 is ProbUGE/ProbLE
		;	
			ProbU1=1.0
		),
		(ProbU1>1.0->
			ProbU=1.0
		;
			ProbU=ProbU1
		),
		Err is ProbU-ProbL,
		setting(min_error,ME),
		(Err<ME->
			ProbU0=ProbU,
			ProbL0=ProbL
		;
			setting(depth_bound,DB),
			D1 is D+DB,
			solve_condi(IncGE,IncE,LLGE,LLE,D1,ProbL0,ProbU0)
		)
	;
		ProbL0=undefined,
		ProbU0=undefined
	).

/* iterative deepening, problog style */
scir(Goals,Evidence,ProbL,ProbU,CPUTime):-
	statistics(cputime,[_,_]),
	setting(depth_bound,D),
        solve_condir(Goals,Evidence,D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.

solve_condir(Goals,Evidence,D,ProbL0,ProbU0):-
	(call_residue(setof(DerivE,find_derivr(Evidence,D,DerivE),LDupE),_R0)->
		rem_dup_lists(LDupE,[],LE),
		append(Evidence,Goals,EG),
		(call_residue(setof(DerivGE,find_derivr(EG,D,DerivGE),LDupGE),_R1)->
			rem_dup_lists(LDupGE,[],LGE),
			separate_ulb(LGE,[],LLGE,[],LUGE),
			compute_prob_deriv(LLGE,ProbLGE),
			compute_prob_deriv(LUGE,ProbUGE),
			separate_ulb(LE,[],LLE,[],LUE),
			compute_prob_deriv(LLE,ProbLE),
			compute_prob_deriv(LUE,ProbUE),
			ProbL is ProbLGE/ProbUE,
			(ProbLE=0.0->
				ProbU1=1.0
			;
				ProbU1 is ProbUGE/ProbLE
			),
			(ProbU1>1.0->
				ProbU=1.0
			;
				ProbU=ProbU1
			),
			Err is ProbU-ProbL,
			setting(min_error,ME),
			(Err<ME->
				ProbU0=ProbU,
				ProbL0=ProbL
			;
				setting(depth_bound,DB),
				D1 is D+DB,
				solve_condir(Goals,Evidence,D1,ProbL0,ProbU0)
			)
		;
			ProbL0=0.0,
			ProbU0=0.0
		)	
	;
		ProbL0=undefined,
		ProbU0=undefined
	).

/* approximate algorithm cilog2 style */
scic(Goals,Evidence,ProbL,ProbU,CPUTime):-
	statistics(cputime,[_,_]),
	setting(depth_bound,D),
        solve_condic(Goals,Evidence,D,ProbL,ProbU),
	statistics(cputime,[_,CT]),
	CPUTime is CT/1000.

solve_condic(Goals,Evidence,D,ProbL0,ProbU0):-
	(call_residue(setof((DerivE,P,Pruned),solvec(Evidence,D,[],DerivE,1.0,P,Pruned),LE),_R0)->
		append(Evidence,Goals,EG),
		(call_residue(setof((DerivGE,P,Pruned),solvec(EG,D,[],DerivGE,1.0,P,Pruned),LGE),_R1)->
			separate_ulbc(LGE,[],LLGE,0.0,ErrGE),
			compute_prob_deriv(LLGE,ProbLGE),
			separate_ulbc(LE,[],LLE,0.0,ErrE),
			compute_prob_deriv(LLE,ProbLE),
			ProbUGE0 is ProbLGE+ErrGE,
			(ProbUGE0>1.0->
				ProbUGE=1.0
			;
				ProbUGE=ProbUGE0
			),
			ProbUE0 is ProbLE+ErrE,
			(ProbUE0>1.0->
				ProbUE=1.0
			;
				ProbUE=ProbUE0
			),
			ProbL0 is ProbLGE/ProbUE,
			(ProbLE=0.0->
				ProbU1=1.0
			;
				ProbU1 is ProbUGE/ProbLE
			),
			(ProbU1>1.0->
				ProbU0=1.0
			;
				ProbU0=ProbU1
			)
		;
			ProbL0=0.0,
			ProbU0=0.0
		)	
	;
		ProbL0=undefined,
		ProbU0=undefined
	).

/* sc(Goals,Evidence,Prob,Time1,Time2) compute the conditional probability of the list of goals
Goals given the list of goals Evidence 
Goals and Evidence can have variables, sc returns in backtracking all the solutions with their
corresponding probability 
Time1 is the time for performing resolution
Time2 is the time for elaborating the two BDDs
*/
sc(Goals,Evidence,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	solve_cond(Goals,Evidence,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2).

solve_cond(Goals,Evidence,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(setof(DerivE,find_deriv(Evidence,DerivE),LDupE)->
		rem_dup_lists(LDupE,[],LE),
		(setof(DerivGE,find_deriv_GE(LE,Goals,DerivGE),LDupGE)->
			rem_dup_lists(LDupGE,[],LGE),
			statistics(cputime,[_,CT1]),
			CPUTime1 is CT1/1000,
			statistics(walltime,[_,WT1]),
			WallTime1 is WT1/1000,
			build_formula(LE,FormulaE,[],VarE),
			var2numbers(VarE,0,NewVarE),
			build_formula(LGE,FormulaGE,[],VarGE),
			var2numbers(VarGE,0,NewVarGE),
			compute_prob(NewVarE,FormulaE,ProbE,0),
			call_compute_prob(NewVarGE,FormulaGE,ProbGE),
			Prob is ProbGE/ProbE,
			statistics(cputime,[_,CT2]),
			CPUTime2 is CT2/1000,
			statistics(walltime,[_,WT2]),
			WallTime2 is WT2/1000
		;
			Prob=0.0,
			statistics(cputime,[_,CT1]),
			CPUTime1 is CT1/1000,
			statistics(walltime,[_,WT1]),
			WallTime1 is WT1/1000,
			CPUTime2=0.0,
			WallTime2=0.0
		)
	;
		Prob=undefined,
			statistics(cputime,[_,CT1]),
			CPUTime1 is CT1/1000,
			statistics(walltime,[_,WT1]),
			WallTime1 is WT1/1000,
			CPUTime2=0.0,
			WallTime2=0.0
	).

solve_cond_goals(Goals,LE,0,Time1,0):-
	statistics(runtime,[_,_]),
	\+ find_deriv_GE(LE,Goals,_DerivGE),
	statistics(runtime,[_,T1]),
	Time1 is T1/1000.

call_compute_prob(NewVarGE,FormulaGE,ProbGE):-
	(FormulaGE=[]->
		ProbGE=0.0
	;
		(FormulaGE=[[]|_]->
                	ProbGE=1.0
		;
			(setting(save_dot,true)->
				format("Variables: ~p~n",[NewVarGE]),
				compute_prob(NewVarGE,FormulaGE,ProbGE,1)
			;
				compute_prob(NewVarGE,FormulaGE,ProbGE,0)
			)
		)
	).

find_deriv_GE(LD,GoalsList,Deriv):-
	member(D,LD),
	solve(GoalsList,D,DerivDup),
	remove_duplicates(DerivDup,Deriv).

find_deriv_GE(LD,GoalsList,DB,Deriv):-
        member(D,LD),
	solve(GoalsList,DB,D,DerivDup),
	remove_duplicates(DerivDup,Deriv).

/* solve(GoalsList,CIn,COut) takes a list of goals and an input C set
and returns an output C set
The C set is a list of triple (N,R,S) where
- N is the index of the head atom used, starting from 0
- R is the index of the non ground rule used, starting from 1
- S is the substitution of rule R, in the form of a list whose elements
	are of the form 'VarName'=value
*/
solve([],C,C):-!.

solve([bagof(V,EV^G,L)|T],CIn,COut):-!,
	list2and(GL,G),
	bagof((V,C),EV^solve(GL,CIn,C),LD),
	length(LD,N),
	build_initial_graph(N,GrIn),	
	build_graph(LD,0,GrIn,Gr),
	clique(Gr,Clique),
	build_Cset(LD,Clique,L,[],C1),
	remove_duplicates_eq(C1,C2),
	solve(T,C2,COut).

solve([bagof(V,G,L)|T],CIn,COut):-!,
	list2and(GL,G),
	bagof((V,C),solve(GL,CIn,C),LD),
	length(LD,N),
	build_initial_graph(N,GrIn),	
	build_graph(LD,0,GrIn,Gr),
	clique(Gr,Clique),
	build_Cset(LD,Clique,L,[],C1),
	remove_duplicates_eq(C1,C2),
	solve(T,C2,COut).


solve([setof(V,EV^G,L)|T],CIn,COut):-!,
	list2and(GL,G),
	setof((V,C),EV^solve(GL,CIn,C),LD),
	length(LD,N),
	build_initial_graph(N,GrIn),	
	build_graph(LD,0,GrIn,Gr),
	clique(Gr,Clique),
	build_Cset(LD,Clique,L1,[],C1),	
	remove_duplicates(L1,L),	
	solve(T,C1,COut).

solve([setof(V,G,L)|T],CIn,COut):-!,
	list2and(GL,G),
	setof((V,C),solve(GL,CIn,C),LD),
	length(LD,N),
	build_initial_graph(N,GrIn),	
	build_graph(LD,0,GrIn,Gr),
	clique(Gr,Clique),
	build_Cset(LD,Clique,L1,[],C1),	
	remove_duplicates(L1,L),	
	solve(T,C1,COut).

solve([\+ H |T],CIn,COut):-!,
	list2and(HL,H),
	(setof(D,find_deriv(HL,D),LDup)->
		rem_dup_lists(LDup,[],L),
		choose_clauses(CIn,L,C1),	
		solve(T,C1,COut)
	;
		solve(T,CIn,COut)
	).
	
solve([H|T],CIn,COut):-
	builtin(H),!,
	call(H),
	solve(T,CIn,COut).

solve([H|T],CIn,COut):-
	def_rule(H,B),
	append(B,T,NG),
	solve(NG,CIn,COut).
	
solve([H|T],CIn,COut):-
	find_rule(H,(R,S,N),B,CIn),
	solve_pres(R,S,N,B,T,CIn,COut).


solvei([],_DB,C,C,[]):-!.

solvei(G,0,C,C,G):-!.

solvei([\+ H |T],DB,CIn,COut,G):-!,
        list2and(HL,H),
	(findall((GH,D),solvei(HL,DB,CIn,D,GH),L)->
		separate_ulbi(L,[],LB,[],_UB,[],I),
		(I\=[]->
			C1=CIn,
			G=[\+ H|G1]
		;
			choose_clauses(CIn,LB,C1),
			G=G1
		),
		solvei(T,DB,C1,COut,G1)
	;
		solvei(T,DB,CIn,COut,G1)
	).
solvei([H|T],DB,CIn,COut,G):-
	builtin(H),!,
	call(H),
	solvei(T,DB,CIn,COut,G).

solvei([H|T],DB,CIn,COut,G):-
	def_rule(H,B),
	append(B,T,NG),
	DB1 is DB-1,
	solvei(NG,DB1,CIn,COut,G).

solvei([H|T],DB,CIn,COut,G):-
	find_rule(H,(R,S,N),B,CIn),
	DB1 is DB-1,
	solve_presi(R,S,N,B,T,DB1,CIn,COut,G).

solver([],_DB,C,C):-!.

solver(_G,0,C,[(_,pruned,_)|C]):-!.

solver([\+ H |T],DB,CIn,COut):-!,
        list2and(HL,H),
	(setof(D,find_derivr(HL,DB,D),LDup)->
		rem_dup_lists(LDup,[],L),
		separate_ulb(L,[],LB,[],UB),
		(\+ LB=UB->
			
			choose_clauses(CIn,LB,C0),
			C1=[(_,pruned,_)|C0]
		;
			choose_clauses(CIn,L,C1)
		),
		solver(T,DB,C1,COut)
	;
		solver(T,DB,CIn,COut)
	).
solver([H|T],DB,CIn,COut):-
	builtin(H),!,
	call(H),
	solver(T,DB,CIn,COut).

solver([H|T],DB,CIn,COut):-
	def_rule(H,B),
	append(B,T,NG),
	DB1 is DB-1,
	solver(NG,DB1,CIn,COut).

solver([H|T],DB,CIn,COut):-
	find_rule(H,(R,S,N),B,CIn),
	DB1 is DB-1,
	solve_presr(R,S,N,B,T,DB1,CIn,COut).


solvec([],_DB,C,C,P,P,false):-!.

solvec(_G,0,C,C,P,P,true):-!.

solvec(_G,_DB,C,C,P,P,true):-
	setting(prob_threshold,T),
	P=<T,!.

solvec([\+ H |T],DB,CIn,COut,P0,P1,Pruned):-!,
        list2and(HL,H),
	(setof((D,P,Pr),solvec(HL,DB,[],D,1,P,Pr),L)->
		separate_ulbc(L,[],LB,0.0,PP),
		(PP=\=0.0->
			
			choose_clausesc(CIn,LB,C1,P0,P2),
			Pruned=true,
			solvec(T,DB,C1,COut,P2,P1,_)

		;
			choose_clausesc(CIn,LB,C1,P0,P2),
			solvec(T,DB,C1,COut,P2,P1,Pruned)
		)
	;
		solve(T,DB,CIn,COut,P0,P1,Pruned)
	).

solvec([H|T],DB,CIn,COut,P0,P1,Pruned):-
	builtin(H),!,
	call(H),
	solvec(T,DB,CIn,COut,P0,P1,Pruned).

solvec([H|T],DB,CIn,COut,P0,P1,Pruned):-
	def_rule(H,B),
	append(B,T,NG),
	DB1 is DB-1,
	solvec(NG,DB1,CIn,COut,P0,P1,Pruned).

solvec([H|T],DB,CIn,COut,P0,P1,Pruned):-
	find_rulec(H,(R,S,N),B,CIn,P),
	DB1 is DB-1,
	solve_presc(R,S,N,B,T,DB1,CIn,COut,P,P0,P1,Pruned).


solve_pres(R,S,N,B,T,CIn,COut):-
	member_eq((N,R,S),CIn),!,
	append(B,T,NG),
	solve(NG,CIn,COut).
	
solve_pres(R,S,N,B,T,CIn,COut):-
	append(CIn,[(N,R,S)],C1),
	append(B,T,NG),
	solve(NG,C1,COut).

solve_presi(R,S,N,B,T,DB,CIn,COut,G):-
        member_eq((N,R,S),CIn),!,
	append(B,T,NG),
	solvei(NG,DB,CIn,COut,G).

solve_presi(R,S,N,B,T,DB,CIn,COut,G):-
	append(CIn,[(N,R,S)],C1),
	append(B,T,NG),
	solvei(NG,DB,C1,COut,G).


solve_presr(R,S,N,B,T,DB,CIn,COut):-
        member_eq((N,R,S),CIn),!,
	append(B,T,NG),
	solver(NG,DB,CIn,COut).

solve_presr(R,S,N,B,T,DB,CIn,COut):-
	append(CIn,[(N,R,S)],C1),
	append(B,T,NG),
	solver(NG,DB,C1,COut).


solve_presc(R,S,N,B,T,DB,CIn,COut,_,P0,P1,Pruned):-
        member_eq((N,R,S),CIn),!,
	append(B,T,NG),
	solvec(NG,DB,CIn,COut,P0,P1,Pruned).

solve_presc(R,S,N,B,T,DB,CIn,COut,P,P0,P1,Pruned):-
	append(CIn,[(N,R,S)],C1),
	append(B,T,NG),
	P2 is P0*P,
	solvec(NG,DB,C1,COut,P2,P1,Pruned).


build_initial_graph(N,G):-
	listN(0,N,Vert),
	add_vertices([],Vert,G).


build_graph([],_N,G,G).
	
build_graph([(_V,C)|T],N,GIn,GOut):-
	N1 is N+1,
	compatible(C,T,N,N1,GIn,G1),
	build_graph(T,N1,G1,GOut).
	
compatible(_C,[],_N,_N1,G,G).	

compatible(C,[(_V,H)|T],N,N1,GIn,GOut):-
	(compatible(C,H)->
		add_edges(GIn,[N-N1,N1-N],G1)
	;
		G1=GIn
	),
	N2 is N1 +1,
	compatible(C,T,N,N2,G1,GOut).

compatible([],_C).

compatible([(N,R,S)|T],C):-
	not_present_with_a_different_head(N,R,S,C),
	compatible(T,C).

not_present_with_a_different_head(_N,_R,_S,[]).

not_present_with_a_different_head(N,R,S,[(N,R,S)|T]):-!,
	not_present_with_a_different_head(N,R,S,T).

not_present_with_a_different_head(N,R,S,[(_N1,R,S1)|T]):-
	S\=S1,!,
	not_present_with_a_different_head(N,R,S,T).

not_present_with_a_different_head(N,R,S,[(_N1,R1,_S1)|T]):-
	R\=R1,
	not_present_with_a_different_head(N,R,S,T).
		


build_Cset(_LD,[],[],C,C).
		
build_Cset(LD,[H|T],[V|L],CIn,COut):-
	nth0(H,LD,(V,C)),
	append(C,CIn,C1),
	build_Cset(LD,T,L,C1,COut).
	
	
/* find_rule(G,(R,S,N),Body,C) takes a goal G and the current C set and
returns the index R of a disjunctive rule resolving with G together with
the index N of the resolving head, the substitution S and the Body of the 
rule */
find_rule(H,(R,S,N),Body,C):-
	rule(H,_P,N,R,S,_,Head,Body),
	member_head(H,Head,0,N),
	not_already_present_with_a_different_head(N,R,S,C).

find_rule(H,(R,S,Number),Body,C):-
	rule_uniform(H,R,S,_,1/_Num,_L,Number,Body),
	not_already_present_with_a_different_head(Number,R,S,C).

find_rulec(H,(R,S,N),Body,C,P):-
	rule(H,_P,N,R,S,_,Head,Body),
	member_headc(H,Head,0,N,P),
	not_already_present_with_a_different_head(N,R,S,C).


not_already_present_with_a_different_head(_N,_R,_S,[]).


not_already_present_with_a_different_head(N,R,S,[(N1,R,S1)|T]):-
	not_different(N,N1,S,S1),!,
	not_already_present_with_a_different_head(N,R,S,T).
		
not_already_present_with_a_different_head(N,R,S,[(_N1,R1,_S1)|T]):-
	R\==R1,
	not_already_present_with_a_different_head(N,R,S,T).


not_different(_N,_N1,S,S1):-
	S\=S1,!.	

not_different(N,N1,S,S1):-
	N\=N1,!,
	dif(S,S1).	

not_different(N,N,S,S).


member_head(H,[(H:_P)|_T],N,N).

member_head(H,[(_H:_P)|T],NIn,NOut):-
	N1 is NIn+1,
	member_head(H,T,N1,NOut).

member_headc(H,[(H:P)|_T],N,N,P).

member_headc(H,[(_H:_P)|T],NIn,NOut,P):-
	N1 is NIn+1,
	member_headc(H,T,N1,NOut,P).


/* choose_clauses(CIn,LC,COut) takes as input the current C set and 
the set of C sets for a negative goal and returns a new C set that 
excludes all the derivations for the negative goals */
choose_clauses(C,[],C).

choose_clauses(CIn,[D|T],COut):-
	member((N,R,S),D),
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_a_head(N,R,S,CIn,C1),
	choose_clauses(C1,T,COut).

	
choose_clauses(CIn,[D|T],COut):-
	member((N,R,S),D),
	new_head(N,R,S,N1),
	\+ already_present(N1,R,S,CIn),
	impose_dif_cons(R,S,CIn),
	choose_clauses([(N1,R,S)|CIn],T,COut).

choose_clausesc(C,[],C,P,P).

choose_clausesc(CIn,[D|T],COut,P0,P1):-
	member((N,R,S),D),
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_a_headc(N,R,S,CIn,C1,P0,P2),
	choose_clausesc(C1,T,COut,P2,P1).

	
choose_clausesc(CIn,[D|T],COut,P0,P1):-
	member((N,R,S),D),
	new_head(N,R,S,N1),
	\+ already_present(N1,R,S,CIn),
	impose_dif_cons(R,S,CIn),
	rule_by_num(R,S,_Numbers,Head,_Body),
	nth0(N1, Head, (_H:P), _Rest),
	P2 is P0*P,
	choose_clausesc([(N1,R,S)|CIn],T,COut,P2,P1).


choose_clauses_DB(C,[],C).

choose_clauses_DB(CIn,[D|T],COut):-
        member((N,R,S),D),
	ground((N,R,S)),
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_a_head(N,R,S,CIn,C1),
	choose_clauses_DB(C1,T,COut).

choose_clauses_DB(CIn,[D|T],COut):-
        member((N,R,S),D),
	ground((N,R,S)),!,
	new_head(N,R,S,N1),
	\+ already_present(N1,R,S,CIn),
	impose_dif_cons(R,S,CIn),
	choose_clauses_DB([(N1,R,S)|CIn],T,COut).


impose_dif_cons(_R,_S,[]):-!.

impose_dif_cons(R,S,[(_NH,R,SH)|T]):-!,
	dif(S,SH),
	impose_dif_cons(R,S,T).

impose_dif_cons(R,S,[_H|T]):-
	impose_dif_cons(R,S,T).
	
/* instantiation_present_with_the_same_head(N,R,S,C)
takes rule R with substitution S and selected head N and a C set
and asserts dif constraints for all the clauses in C of which RS
is an instantitation and have the same head selected */
instantiation_present_with_the_same_head(_N,_R,_S,[]).

instantiation_present_with_the_same_head(N,R,S,[(NH,R,SH)|T]):-
	\+ \+ S=SH,!,
	dif_head_or_subs(N,R,S,NH,SH,T).

instantiation_present_with_the_same_head(N,R,S,[_H|T]):-
	instantiation_present_with_the_same_head(N,R,S,T).

dif_head_or_subs(N,R,S,NH,_SH,T):-
	dif(N,NH),
	instantiation_present_with_the_same_head(N,R,S,T).

dif_head_or_subs(N,R,S,N,SH,T):-
	dif(S,SH),
	instantiation_present_with_the_same_head(N,R,S,T).

/* case 1 of Select: a more general rule is present in C with
a different head, instantiate it */
choose_a_headc(N,R,S,[(NH,R,SH)|T],[(NH,R,SH)|T],P,P):-
	S=SH, 
	dif(N,NH).

/* case 2 of Select: a more general rule is present in C with
a different head, ensure that they do not generate the same
ground clause */
choose_a_headc(N,R,S,[(NH,R,SH)|T],[(NH,R,S),(NH,R,SH)|T],P0,P1):-
	\+ \+ S=SH, S\==SH, 
	dif(N,NH),
	dif(S,SH),
	rule_by_num(R,S,_Numbers,Head,_Body),
	nth0(NH, Head, (_H:P), _Rest),
	P1 is P0*P.

choose_a_headc(N,R,S,[H|T],[H|T1],P0,P1):-
	choose_a_headc(N,R,S,T,T1,P0,P1).

/* case 1 of Select: a more general rule is present in C with
a different head, instantiate it */
choose_a_head(N,R,S,[(NH,R,SH)|T],[(NH,R,SH)|T]):-
	S=SH, 
	dif(N,NH).

/* case 2 of Select: a more general rule is present in C with
a different head, ensure that they do not generate the same
ground clause */
choose_a_head(N,R,S,[(NH,R,SH)|T],[(NH,R,S),(NH,R,SH)|T]):-
	\+ \+ S=SH, S\==SH, 
	dif(N,NH),
	dif(S,SH).

choose_a_head(N,R,S,[H|T],[H|T1]):-
	choose_a_head(N,R,S,T,T1).


/* select a head different from N for rule R with
substitution S, return it in N1 */
new_head(N,R,S,N1):-
	rule_by_num(R,S,Numbers,Head,_Body),
	Head\=uniform(_,_,_),!,
	nth0(N, Numbers, _Elem, Rest),
	member(N1,Rest).

new_head(N,R,S,N1):-
	rule_by_num(R,S,Numbers,uniform(_A:1/Tot,_L,_Number),_Body),
	listN(0,Tot,Numbers),
	nth0(N, Numbers, _Elem, Rest),
	member(N1,Rest).

already_present_with_a_different_head(N,R,S,[(NH,R,SH)|_T]):-
	\+ \+ S=SH,NH \= N.

already_present_with_a_different_head(N,R,S,[_H|T]):-
	already_present_with_a_different_head(N,R,S,T).


/* checks that a rule R with head N and selection S is already
present in C (or a generalization of it is in C) */ 
already_present(N,R,S,[(N,R,SH)|_T]):-
	S=SH.

already_present(N,R,S,[_H|T]):-
	already_present(N,R,S,T).

/* rem_dup_lists removes the C sets that are a superset of 
another C sets further on in the list of C sets */
/* rem_dup_lists removes the C sets that are a superset of 
another C sets further on in the list of C sets */
rem_dup_lists([],L,L).

rem_dup_lists([H|T],L0,L):-
	(member_subset(H,T);member_subset(H,L0)),!,
	rem_dup_lists(T,L0,L).

rem_dup_lists([H|T],L0,L):-
	rem_dup_lists(T,[H|L0],L).

member_subset(E,[H|_T]):-
	subset_my(H,E),!.

member_subset(E,[_H|T]):-
	member_subset(E,T).

separate_ulbi([],L,L,U,U,I,I):-!.

separate_ulbi([([],H)|T],L0,[H|L1],U0,[H|U1],I0,I1):-
	!,
	separate_ulbi(T,L0,L1,U0,U1,I0,I1).

separate_ulbi([(G,H)|T],L0,L1,U0,[H1|U1],I0,[(G,H)|I1]):-
        get_ground(H,H1),
	separate_ulbi(T,L0,L1,U0,U1,I0,I1).


separate_ulb([],L,L,U,U):-!.

separate_ulb([H|T],L0,[H|L1],U0,[H|U1]):-
	ground(H),!,
	separate_ulb(T,L0,L1,U0,U1).

separate_ulb([H|T],L0,L1,U0,[H1|U1]):-
        get_ground(H,H1),
	separate_ulb(T,L0,L1,U0,U1).


separate_ulbc([],L,L,P,P):-!.

separate_ulbc([(_H,P,true)|T],L0,L1,P0,P1):-!,
	P2 is P0+P,
	separate_ulbc(T,L0,L1,P2,P1).

separate_ulbc([(H,_P,false)|T],L0,[H|L1],P0,P1):-
	separate_ulbc(T,L0,L1,P0,P1).

get_ground([],[]):-!.

get_ground([H|T],[H|T1]):-
	ground(H),!,
	get_ground(T,T1).

get_ground([_H|T],T1):-
	get_ground(T,T1).


/* predicates for building the formula to be converted into a BDD */

/* build_formula(LC,Formula,VarIn,VarOut) takes as input a set of C sets
LC and a list of Variables VarIn and returns the formula and a new list
of variables VarOut 
Formula is of the form [Term1,...,Termn]
Termi is of the form [Factor1,...,Factorm]
Factorj is of the form (Var,Value) where Var is the index of
the multivalued variable Var and Value is the index of the value
*/
build_formula([],[],Var,Var,C,C).

build_formula([D|TD],[F|TF],VarIn,VarOut,C0,C1):-
	length(D,NC),
	C2 is C0+NC,
	build_term(D,F,VarIn,Var1),
	build_formula(TD,TF,Var1,VarOut,C2,C1).

build_formula([],[],Var,Var).

build_formula([D|TD],[F|TF],VarIn,VarOut):-
	build_term(D,F,VarIn,Var1),
	build_formula(TD,TF,Var1,VarOut).


build_term([],[],Var,Var).

build_term([(_,pruned,_)|TC],TF,VarIn,VarOut):-!,
	build_term(TC,TF,VarIn,VarOut).

build_term([(N,R,S)|TC],[[NVar,N]|TF],VarIn,VarOut):-
	(nth0_eq(0,NVar,VarIn,(R,S))->
		Var1=VarIn
	;
		append(VarIn,[(R,S)],Var1),
		length(VarIn,NVar)
	),
	build_term(TC,TF,Var1,VarOut).

/* nth0_eq(PosIn,PosOut,List,El) takes as input a List,
an element El and an initial position PosIn and returns in PosOut
the position in the List that contains an element exactly equal to El
*/
nth0_eq(N,N,[H|_T],El):-
	H==El,!.

nth0_eq(NIn,NOut,[_H|T],El):-
	N1 is NIn+1,
	nth0_eq(N1,NOut,T,El).

/* var2numbers converts a list of couples (Rule,Substitution) into a list
of triples (N,NumberOfHeadsAtoms,ListOfProbabilities), where N is an integer 
starting from 0 */
var2numbers([],_N,[]).

var2numbers([(R,S)|T],N,[[N,ValNumber,Probs]|TNV]):-
	find_probs(R,S,Probs),
	length(Probs,ValNumber),
	N1 is N+1,
	var2numbers(T,N1,TNV).

find_probs(R,S,Probs):-
	rule_by_num(R,S,_N,Head,_Body),
	get_probs(Head,Probs).
	
get_probs(uniform(_A:1/Num,_P,_Number),ListP):-
	Prob is 1/Num,
	list_el(Num,Prob,ListP).

get_probs([],[]).

get_probs([_H:P|T],[P1|T1]):-
	P1 is P,
	get_probs(T,T1).

list_el(0,_P,[]):-!.

list_el(N,P,[P|T]):-
	N1 is N-1,
	list_el(N1,P,T).

/* end of predicates for building the formula to be converted into a BDD */list_el(0,_P,[]):-!.


/* start of predicates for parsing an input file containing a program */

/* p(File) parses the file File.cpl. It can be called more than once without 
exiting yap */
p(File):-
	parse(File).

parse(File):-
	atom_concat(File,'.cpl',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
        retractall(rule_by_num(_,_,_,_,_)),
        retractall(rule(_,_,_,_,_,_,_,_)),
        retractall(def_rule(_,_)),
	retractall(rule_uniform(_,_,_,_,_,_,_,_)),
	process_clauses(C,1).

process_clauses([(end_of_file,[])],_N):-!.

process_clauses([((H:-B),V)|T],N):-
	H=uniform(A,P,L),!,
	list2and(BL,B),
	process_body(BL,V,V1),
	remove_vars([P],V1,V2),
	append(BL,[length(L,Tot),nth0(Number,L,P)],BL1),
	append(V2,['Tot'=Tot],V3),
	assertz(rule_by_num(N,V3,_NH,uniform(A:1/Tot,L,Number),BL1)),
	assertz(rule_uniform(A,N,V3,_NH,1/Tot,L,Number,BL1)),
	N1 is N+1,
	process_clauses(T,N1).

process_clauses([((H:-B),V)|T],N):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	list2and(BL,B),
	process_body(BL,V,V1),
	length(HL,LH),
	listN(0,LH,NH),
	assert_rules(HL,0,HL,BL,NH,N,V1),
	assertz(rule_by_num(N,V1,NH,HL,BL)),
	N1 is N+1,
	process_clauses(T,N1).

process_clauses([((H:-B),V)|T],N):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	list2and(BL,B),
	process_body(BL,V,V1),
	length(HL,LH),
	listN(0,LH,NH),
	assert_rules(HL,0,HL,BL,NH,N,V1),
	assertz(rule_by_num(N,V1,NH,HL,BL)),
	N1 is N+1,
	process_clauses(T,N1).
	
process_clauses([((H:-B),_V)|T],N):-!,
	list2and(BL,B),
	assert(def_rule(H,BL)),
	process_clauses(T,N).

process_clauses([(H,V)|T],N):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	length(HL,LH),
	listN(0,LH,NH),
	assert_rules(HL,0,HL,[],NH,N,V),
	assertz(rule_by_num(N,V,NH,HL,[])),
	N1 is N+1,
	process_clauses(T,N1).

process_clauses([(H,V)|T],N):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	length(HL,LH),
	listN(0,LH,NH),
	assert_rules(HL,0,HL,[],NH,N,V),
	assertz(rule_by_num(N,V,NH,HL,[])),
	N1 is N+1,
	process_clauses(T,N1).
	
process_clauses([(H,_V)|T],N):-
	assert(def_rule(H,[])),
	process_clauses(T,N).

assert_rules([],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules(['':_P],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules([H:P|T],Pos,HL,BL,NH,N,V1):-
	assertz(rule(H,P,Pos,N,V1,NH,HL,BL)),
	Pos1 is Pos+1,
	assert_rules(T,Pos1,HL,BL,NH,N,V1).


/* if the annotation in the head are not ground, the null atom is not added
and the eventual formulas are not evaluated */
	
process_head(HL,NHL):-
	(ground_prob(HL)->
		process_head_ground(HL,0,NHL)
	;
		NHL=HL
	).

ground_prob([]).

ground_prob([_H:PH|T]):-
	ground(PH),
	ground_prob(T).

process_head_ground([H:PH],P,[H:PH1|Null]):-
	PH1 is PH,
	PNull is 1-P-PH1,
	setting(epsilon_parsing,Eps),
	EpsNeg is - Eps,
	PNull > EpsNeg,
	(PNull>Eps->
		Null=['':PNull]
	;
		Null=[]
	).

process_head_ground([H:PH|T],P,[H:PH1|NT]):-
	PH1 is PH,
	P1 is P+PH1,
	process_head_ground(T,P1,NT).

/* setof must have a goal of the form B^G where B is a term containing the existential variables */
process_body([],V,V).

process_body([setof(A,B^_G,_L)|T],VIn,VOut):-!,
	get_var(A,VA),
	get_var(B,VB),
	remove_vars(VA,VIn,V1),
	remove_vars(VB,V1,V2),
	process_body(T,V2,VOut).

process_body([setof(A,_G,_L)|T],VIn,VOut):-!,
	get_var(A,VA),
	remove_vars(VA,VIn,V1),
	process_body(T,V1,VOut).

process_body([bagof(A,B^_G,_L)|T],VIn,VOut):-!,
	get_var(A,VA),
	get_var(B,VB),
	remove_vars(VA,VIn,V1),
	remove_vars(VB,V1,V2),
	process_body(T,V2,VOut).

process_body([bagof(A,_G,_L)|T],VIn,VOut):-!,
	get_var(A,VA),
	remove_vars(VA,VIn,V1),
	process_body(T,V1,VOut).

process_body([_H|T],VIn,VOut):-!,
	process_body(T,VIn,VOut).

get_var_list([],[]).

get_var_list([H|T],[H|T1]):-
	var(H),!,
	get_var_list(T,T1).

get_var_list([H|T],VarOut):-!,
	get_var(H,Var),
	append(Var,T1,VarOut),
	get_var_list(T,T1).

get_var(A,[A]):-
	var(A),!.

get_var(A,V):-
	A=..[_F|Args],
	get_var_list(Args,V).

remove_vars([],V,V).

remove_vars([H|T],VIn,VOut):-
	delete_var(H,VIn,V1),
	remove_vars(T,V1,VOut).

delete_var(_H,[],[]).

delete_var(V,[VN=Var|T],[VN=Var|T1]):-
	V\==Var,!,
	delete_var(V,T,T1).

delete_var(_V,[_H|T],T).

/* predicates for reading in the program clauses */
read_clauses(S,Clauses):-
	(setting(ground_body,true)->
		read_clauses_ground_body(S,Clauses)
	;
		read_clauses_exist_body(S,Clauses)
	).


read_clauses_ground_body(S,[(Cl,V)|Out]):-
	read_term(S,Cl,[variable_names(V)]),
	(Cl=end_of_file->
		Out=[]
	;
		read_clauses_ground_body(S,Out)
	).


read_clauses_exist_body(S,[(Cl,V)|Out]):-
	read_term(S,Cl,[variable_names(VN)]),
	extract_vars_cl(Cl,VN,V),
	(Cl=end_of_file->
		Out=[]
	;
		read_clauses_exist_body(S,Out)
	).


extract_vars_cl(end_of_file,[]).

extract_vars_cl(Cl,VN,Couples):-
	(Cl=(H:-_B)->
		true
	;
		H=Cl
	),
	extract_vars(H,[],V),
	pair(VN,V,Couples).
	

pair(_VN,[],[]).

pair([VN= _V|TVN],[V|TV],[VN=V|T]):-
	pair(TVN,TV,T).	
	

extract_vars(Var,V0,V):-
	var(Var),!,
	(member_eq(Var,V0)->
		V=V0
	;
		append(V0,[Var],V)
	).
	
extract_vars(Term,V0,V):-
	Term=..[_F|Args],
	extract_vars_list(Args,V0,V).


extract_vars_list([],V,V).

extract_vars_list([Term|T],V0,V):-
	extract_vars(Term,V0,V1),
	extract_vars_list(T,V1,V).

	
listN(N,N,[]):-!.

listN(NIn,N,[NIn|T]):-
	N1 is NIn+1,
	listN(N1,N,T).
/* end of predicates for parsing an input file containing a program */

/* start of utility predicates */
list2or([X],X):-
	X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
	list2or(T,Ta).

list2and([X],X):-
	X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
	list2and(T,Ta).

member_eq(A,[H|_T]):-
	A==H,!.
	
member_eq(A,[_H|T]):-
	member_eq(A,T).

subset_my([],_).

subset_my([H|T],L):-
	member_eq(H,L),
	subset_my(T,L).

remove_duplicates_eq([],[]).

remove_duplicates_eq([H|T],T1):-
	member_eq(H,T),!,
	remove_duplicates_eq(T,T1).

remove_duplicates_eq([H|T],[H|T1]):-
	remove_duplicates_eq(T,T1).

builtin(_A is _B).
builtin(_A > _B).
builtin(_A < _B).
builtin(_A >= _B).
builtin(_A =< _B).
builtin(_A =:= _B).
builtin(_A =\= _B).
builtin(true).
builtin(false).
builtin(_A = _B).
builtin(_A==_B).
builtin(_A\=_B).
builtin(_A\==_B).
builtin(length(_L,_N)).
builtin(member(_El,_L)).
builtin(average(_L,_Av)).
builtin(max_list(_L,_Max)).
builtin(min_list(_L,_Max)).
builtin(nth0(_,_,_)).
builtin(nth(_,_,_)).
average(L,Av):-
	sum_list(L,Sum),
	length(L,N),
	Av is Sum/N.

clique(Graph,Clique):-
	vertices(Graph,Candidates),
	extend_cycle(Graph,Candidates,[],[],Clique).
	
extend_cycle(G,[H|T],Not,CS,CSOut):-
	neighbours(H, G, Neigh),
	intersection(Neigh,T,NewCand),
	intersection(Neigh,Not,NewNot),
	extend(G,NewCand,NewNot,[H|CS],CSOut).

extend_cycle(G,[H|T],Not,CS,CSOut):-
	extend_cycle(G,T,[H|Not],CS,CSOut).

extend(_G,[],[],CompSub,CompSub):-!.

extend(G,Cand,Not,CS,CSOut):-
	extend_cycle(G,Cand,Not,CS,CSOut).
	
/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
	retract(setting(Parameter,_)),
	assert(setting(Parameter,Value)).

/* end of utility predicates */
