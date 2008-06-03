/*
	LPAD and CP-Logic reasoning suite
	File cpl.pl
	Computes the semantics of CP-logic programs
	Copyright (c) 2007, Fabrizio Riguzzi
*/

:-use_module(lpad,[slg/3,setting/2,set/2]).

:-use_module(semcpl,[build/0,print/0]).

:-use_module(library(lists)).

p(File):-
	lpad:p(File).

sc(Goals,Evidences,Prob,CPUTime1,0.0,WallTime1,0.0):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	lpad:convert_to_goal(Goals,Goal),
	lpad:convert_to_goal(Evidences,Evidence),
	solve_cond(Goal,Evidence,Prob),
	statistics(cputime,[_,CT1]),
	CPUTime1 is CT1/1000,
	statistics(walltime,[_,WT1]),
	WallTime1 is WT1/1000.

sc(Goals,Evidences,Prob):-
	lpad:convert_to_goal(Goals,Goal),
	lpad:convert_to_goal(Evidences,Evidence),
	solve_cond(Goal,Evidence,Prob).


solve_cond(Goal,Evidence,Prob):-
	(setof((DerivE,D),slg(Evidence,DerivE,D),LCouplesE)->
		separate(LCouplesE,LCDupE,LDefClE),
		lpad:rem_dup_lists(LCDupE,[],LCE),
		lpad:build_formula(LCE,FormulaE,[],VarE),
		lpad:var2numbers(VarE,0,NewVarE),
		lpad:compute_prob(NewVarE,FormulaE,ProbE,0),
		solve_cond_goals(Goal,LCE,ProbGE,LGE,LDefClGE),
		(setof((R,S),N^(member(C,LGE),member((N,R,S),C)),LDisClGE)->
			true
		;
			LDisClGE=[]
		),
		append(LDefClGE,LDefClE,LDefDup),
		remove_duplicates(LDefDup,LDef),
		append(LDisClGE,LDef,LCl),
		test_validity(LCl),
		Prob is ProbGE/ProbE
	;
		format("P(Evidence)=0~n",[]),
		Prob=undefined
	).

solve_cond_goals(Goals,LE,ProbGE,LGE,LDefClGE):-
	(setof((DerivGE,D),find_deriv_GE(LE,Goals,DerivGE,D),LCouplesGE)->
		separate(LCouplesGE,LCDupGE,LDefClGE),
		lpad:rem_dup_lists(LCDupGE,[],LGE),
		lpad:build_formula(LGE,FormulaGE,[],VarGE),
		lpad:var2numbers(VarGE,0,NewVarGE),
		lpad:call_compute_prob(NewVarGE,FormulaGE,ProbGE)
	;
		ProbGE=0
	).

find_deriv_GE(LD,GoalsList,Deriv,Def):-
	member(D,LD),
	lpad:slg(GoalsList,D,DerivDup,[],Def),
	remove_duplicates(DerivDup,Deriv).
	
s(GoalsList,Prob):-
	lpad:convert_to_goal(GoalsList,Goal),
	solve(Goal,Prob).

s(GoalsList,Prob,CPUTime1,0.0,WallTime1,0.0):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	lpad:convert_to_goal(GoalsList,Goal),
	solve(Goal,Prob),
	statistics(cputime,[_,CT1]),
	CPUTime1 is CT1/1000,
	statistics(walltime,[_,WT1]),
	WallTime1 is WT1/1000.

solve(Goal,Prob):-
	(setof((C,D),slg(Goal,C,D),LCouples)->
		separate(LCouples,LCDup,LDefCl),
		(member(unsound,LCDup)->
			format("Unsound program ~n",[]),
			Prob=unsound
		;
			lpad:rem_dup_lists(LCDup,[],L),
			(ground(L)->
				lpad:build_formula(L,Formula,[],Var),
				lpad:var2numbers(Var,0,NewVar),
				(setting(savedot,true)->
					format("Variables: ~p~n",[Var]),
					lpad:compute_prob(NewVar,Formula,_Prob,1)
				;
					lpad:compute_prob(NewVar,Formula,Prob,0)
				),
				(setof((R,S),N^(member(C,LCDup),member((N,R,S),C)),LDisCl)->
					true
				;
					LDisCl=[]
				),
				append(LDisCl,LDefCl,LCl),
				test_validity(LCl)
			;
				format("It requires the choice of a head atom from a non ground head~n~p~n",[L]),
				Prob=non_ground
			)
		)
	;
		Prob=0
	).

test_validity(L):-
	retractall(semcpl:root(_)),
	retractall(semcpl:clauses(_)),
	retractall(semcpl:herbrand_base(_)),
	retractall(semcpl:node(_,_,_,_,_)),
	retractall(semcpl:new_number(_)),
	assert(semcpl:new_number(0)),
	get_clauses_hb(L,LC,HBDup),
	remove_duplicates(HBDup,HB0),
	delete(HB0, '' ,HB),
	assert(semcpl:herbrand_base(HB)),
	assert(semcpl:clauses(LC)),
	build.

get_clauses_hb([],[],[]):-!.

get_clauses_hb([(R,S)|T],[r(Head,Body)|TR],HB):-
	lpad:rule(R,S,_,Head,Body),!,
	get_atoms(Head,Atoms),
	append(Atoms,HB0,HB),
	get_clauses_hb(T,TR,HB0).

get_clauses_hb([(R,S)|T],[r([Head:1],Body)|TR],HB):-
	lpad:def_rule(R,S,Head,Body),
	append([Head],HB0,HB),
	get_clauses_hb(T,TR,HB0).

get_atoms([],[]):-!.

get_atoms([H:_P|T],[H|TA]):-
	get_atoms(T,TA).
	
separate([],[],[]):-!.

separate([(C,D)|T],[C|TC],Cl):-
	append(D,Cl0,Cl),
	separate(T,TC,Cl0).
	
