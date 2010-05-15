/*
	LPAD and CP-Logic reasoning suite
	File lpad.pl
	Goal-oriented interpreter for LPADs based on SLG
	Copyright (c) 2007, Fabrizio Riguzzi
  Based on the SLG System, see below
*/

/***************************************************************************/
/*                                                                         */
/* The SLG System                                                          */
/* Authors: Weidong Chen and David Scott Warren                            */
/* Copyright (C) 1993 Southern Methodist University                        */
/*               1993 SUNY at Stony Brook                                  */
/* See file COPYRIGHT_SLG for copying policies and disclaimer.             */
/*                                                                         */
/***************************************************************************/

/*==========================================================================
  File               : slg.pl
  Last Modification  : November 1, 1993 by Weidong Chen
===========================================================================
  File               : lpad.pl
  Last Modification  : November 14, 2007 by Fabrizio Riguzzi
===========================================================================*/

/* ----------- beginning of system dependent features ---------------------
   To run the SLG system under a version of Prolog other than Quintus,
   comment out the following Quintus-specific code, and include the code
   for the Prolog you are running.
*/
:- module(lpad, [s/2,s/6,
		  sc/3,sc/7,
		  p/1,
		  slg/3,setting/2,set/2
			]).
:-source.
 :- dynamic wfs_trace/0.
 :-use_module(library(ugraphs)).
 :-use_module(library(lists)).
 :- use_module(library(charsio)).


:- op(1200,xfx,<--).
:- op(900,xfx,<-).




/* SLG tracing:
   xtrace: turns SLG trace on, which prints out tables at various 
           points
   xnotrace: turns off SLG trace
*/
xtrace :- 
    ( wfs_trace -> 
      true 
    ; assert(wfs_trace)
    ).
xnotrace :- 
    ( wfs_trace -> 
      retractall(wfs_trace) 
    ; true
    ).

/* isprolog(Call): Call is a Prolog subgoal */
isprolog(Call) :- 
	builtin(Call).

/* slg(Call):
   It returns all true answers of Call under the well-founded semantics
   one by one.
*/
slg(Call,C,D):-
	slg(Call,[],C,[],D).
	
slg(Call,C0,C,D0,D):-
        ( isprolog(Call) ->
          call(Call),
          C=C0,
          D=D0
        ; oldt(Call,Tab,C0,C1,D0,D1),
        	delete(D1,(goal(_),_),D),
          ground(Call,Ggoal),
          find(Tab,Ggoal,Ent),
          ent_to_anss(Ent,Anss),
          member_anss(d(Call,Delay),Anss),
          (Delay=[]->
						C=C1
					;
						write('Unsound program'),
						nl,
						C=unsound
					)
        ).

get_new_atom(Atom):-
	retract(new_number(N)),
	N1 is N+1,
	assert(new_number(N1)),
	number_atom(N,NA),
	atom_concat('$call',NA,Atom).
	
s(GoalsList,Prob):-
	convert_to_goal(GoalsList,Goal),
	solve(Goal,Prob).

s(GoalsList,Prob,CPUTime1,0.0,WallTime1,0.0):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	convert_to_goal(GoalsList,Goal),
	solve(Goal,Prob),
	statistics(cputime,[_,CT1]),
	CPUTime1 is CT1/1000,
	statistics(walltime,[_,WT1]),
	WallTime1 is WT1/1000.

convert_to_goal([Goal],Goal):-Goal \= (\+ _) ,!.

convert_to_goal(GoalsList,Head):-
	get_new_atom(Atom),
	extract_vars(GoalsList,[],V),
	Head=..[Atom|V],
	assertz(def_rule(goal(Atom),_,Head,GoalsList)).


solve(Goal,Prob):-
	(setof(C,D^slg(Goal,C,D),LDup)->
		(member(unsound,LDup)->
			format("Unsound program ~n",[]),
			Prob=unsound
		;
			rem_dup_lists(LDup,[],L),
			(ground(L)->
				build_formula(L,Formula,[],Var),
				var2numbers(Var,0,NewVar),
				(setting(save_dot,true)->
					format("Variables: ~p~n",[Var]),
					compute_prob(NewVar,Formula,_Prob,1)
				;
					compute_prob(NewVar,Formula,Prob,0)
				)
			;
				format("It requires the choice of a head atom from a non ground head~n~p~n",[L]),
				Prob=non_ground
			)
		)
	;
		Prob=0
	).

compute_prob(Var,For,Prob,_):-
	compute_prob_term(Var,For,0,Prob).

compute_prob_term(_Var,[],Prob,Prob).

compute_prob_term(Var,[H|T],Prob0,Prob):-
	compute_prob_factor(Var,H,1,PF),
	Prob1 is Prob0 + PF,
	compute_prob_term(Var,T,Prob1,Prob).
	
compute_prob_factor(_Var,[],PF,PF).

compute_prob_factor(Var,[[N,Value]|T],PF0,PF):-
	nth0(N,Var,[_N,_NH,ListProb]),
	nth0(Value,ListProb,P),
	PF1 is PF0*P,
	compute_prob_factor(Var,T,PF1,PF).

sc(Goals,Evidences,Prob):-
	convert_to_goal(Goals,Goal),
	convert_to_goal(Evidences,Evidence),
	solve_cond(Goal,Evidence,Prob).

sc(Goals,Evidences,Prob,CPUTime1,0.0,WallTime1,0.0):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	convert_to_goal(Goals,Goal),
	convert_to_goal(Evidences,Evidence),
	solve_cond(Goal,Evidence,Prob),
	statistics(cputime,[_,CT1]),
	CPUTime1 is CT1/1000,
	statistics(walltime,[_,WT1]),
	WallTime1 is WT1/1000.

solve_cond(Goal,Evidence,Prob):-
	(setof(DerivE,D^slg(Evidence,DerivE,D),LDupE)->
		rem_dup_lists(LDupE,[],LE),
		build_formula(LE,FormulaE,[],VarE),
		var2numbers(VarE,0,NewVarE),
		compute_prob(NewVarE,FormulaE,ProbE,0),
		solve_cond_goals(Goal,LE,ProbGE),
		Prob is ProbGE/ProbE
	;
		format("P(Evidence)=0~n",[]),
		Prob=undefined
	).

solve_cond_goals(Goals,LE,ProbGE):-
	(setof(DerivGE,find_deriv_GE(LE,Goals,DerivGE),LDupGE)->
		rem_dup_lists(LDupGE,[],LGE),
		build_formula(LGE,FormulaGE,[],VarGE),
		var2numbers(VarGE,0,NewVarGE),
		call_compute_prob(NewVarGE,FormulaGE,ProbGE)
	;
		ProbGE=0
	).

solve_cond_goals(Goals,LE,0):-
	\+ find_deriv_GE(LE,Goals,_DerivGE).

find_deriv_GE(LD,GoalsList,Deriv):-
	member(D,LD),
	slg(GoalsList,D,DerivDup,[],_Def),
	remove_duplicates(DerivDup,Deriv).

call_compute_prob(NewVarGE,FormulaGE,ProbGE):-
	(setting(save_dot,true)->
		format("Variables: ~p~n",[NewVarGE]),
		compute_prob(NewVarGE,FormulaGE,ProbGE,1)
	;
		compute_prob(NewVarGE,FormulaGE,ProbGE,0)
	).


/* emptytable(EmptTab): creates an initial empty stable.
*/
emptytable(0:[]).

/* slgall(Call,Anss):
   slgall(Call,Anss,N0-Tab0,N-Tab):
   If Call is a prolog call, findall is used, and Tab = Tab0;
   If Call is an atom of a tabled predicate, SLG evaluation
   is carried out.
*/
slgall(Call,Anss) :-
	slgall(Call,Anss,0:[],_).
slgall(Call,Anss,N0:Tab0,N:Tab) :-
        ( isprolog(Call) ->
          findall(Call,Call,Anss),
	  N = N0, Tab = Tab0
        ; ground(Call,Ggoal),
          ( find(Tab0,Ggoal,Ent) ->
            ent_to_anss(Ent,Answers),
            Tab = Tab0
          ; new_init_call(Call,Ggoal,Ent,[],S1,1,Dfn1),
            add_tab_ent(Ggoal,Ent,Tab0,Tab1),
            oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,N0:[],N:_TP),
            find(Tab,Ggoal,NewEnt),
            ent_to_anss(NewEnt,Answers)
          ),
          ansstree_to_list(Answers,Anss,[])
        ).


/* oldt(QueryAtom,Table,C0,C,D0,D): top level call for SLG resolution.
   It returns a table consisting of answers for each relevant
   subgoal. For stable predicates, it basically extract the 
   relevant set of ground clauses by solving Prolog predicates
   and other well-founded predicates.
*/
oldt(Call,Tab,C0,C,D0,D) :-
    new_init_call(Call,Ggoal,Ent,[],S1,1,Dfn1),
    add_tab_ent(Ggoal,Ent,[],Tab1),
    oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,0:[],_TP,C0,C1,D0,D,PC),
    add_PC_to_C(PC,C1,C),
    ( wfs_trace -> 
      nl, write('Final '), display_table(Tab), nl
    ; true 
    ).

/* oldt(Call,Ggoal,Tab0,Tab,Stack0,Stack,DFN0,DFN,Dep0,Dep,TP0,TP,C0,C,D0,D,PC)
   explores the initial set of edges, i.e., all the 
   program clauses for Call. Ggoal is of the form 
   Gcall-Gdfn, where Gcall is numbervar of Call and Gdfn
   is the depth-first number of Gcall. Tab0/Tab,Stack0/Stack,
   DFN0/DFN, and Dep0/Dep are accumulators for the table, 
   the stack of subgoals, the DFN counter, and the dependencies.
   TP0/TP is the accumulator for newly created clauses during
   the processing of general clauss with universal disjunctions
   in the body. These clauses are created in order to guarantee
   polynomial data complexity in processing clauses with
   universal disjuntions in the body of a clause. The newly 
   created propositions are represented by numbers.
   C0/C are accumulators for disjunctive clauses used in the derivation of Call: 
	 they are list of triples (N,R,S) where N is the number of the head atom used
   (starting from 0), R is the number of the rule used (starting from 1) and
   S is the substitution of the variables in the head atom used. S is a list 
	 of elements of the form Varname=Term.
	 D0/D are accumulators for definite clauses: they are list of couples (R,S),
	 where R is a rule number and S is a substitution.
	 PC is a list of disjunctive rules selected but not used in the derivation,
	 they are added to the C set afterwards if they are consistent with C
	 (PC stands for Possible C, i.e., possible additions to the C set).
*/
oldt(Call,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D,PC) :-
    ( number(Call) ->
      TP0 = (_ : Tcl),
      find(Tcl,Call,Clause),
      edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,
				C0,C,D0,D)
    ; find_rules(Call,Frames,C0,PC),
	    map_oldt(Frames,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,
				C0,C,D0,D)
    ),
    comp_tab_ent(Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* find_rules(Call,Frames,C,PossC)
	finds rules for Call. Frames is the list of clauses that resolve with Call. It
	is a list of terms of the form
	rule(d(Call,[]),Body,R,N,S)
	C is the current set of disjunctive clauses together with the head selected
	PossC is the list of possible disjunctive clauses together with the head
	selected: they are the clauses with an head that does not unify with Call. It
	is a list of terms of the form
	rule(d(Call,[]),Body,R,N,S)
*/
find_rules(Call,Frames,C,PossC):-
	findall(rule(d(Call,[]),Body,def(N),_,Subs,_),def_rule(N,Subs,Call,Body),Fr1),
	find_disj_rules(Call,Fr2,C,PossC),
	append(Fr1,Fr2,Frames).
	
/* find_disj_rules(Call,Fr,C,PossC):-	
	finds disjunctive rules for Call. 
*/
find_disj_rules(Call,Fr,C,[]):-	
	findall(rule(d(Call,[]),Body,R,N,S,LH),
	find_rule(Call,(R,S,N),Body,LH),Fr).

find_disj_rulesold(Call,Fr,C,PossC):-	
	findall(rule(d(Call,[]),Body,R,S,N,LH),
	find_rule(Call,(R,S,N),Body,LH),LD),
	(setof((R,LH),(Call,Body,S,N)^member(rule(d(Call,[]),Body,R,S,N,LH),LD),LR)->		
		choose_rules(LR,LD,[],Fr,C,[],PossC)
	;
		Fr=[],
		PossC=[]
	).



/* choose_rules(LR,LD,Fr0,Fr,C,PossC0,PossC)
	 LR is a list of couples (R,LH) where R is a disjunctive rule number and LH is
	 a list of head atoms numbers, from 0 to length(head)-1
	 LD is the list of disjunctive clauses resolving with Call. Its elements are
	 of the form
	 rule(d(Call,[]),Body,R,N,S)
	 Fr0/Fr are accumulators for the matching disjunctive clauses
	 PossC0/PossC are accumulators for the additional disjunctive clauses

*/
choose_rules([],Fr,Fr,_C,PC,PC).

choose_rules([rule(d(Call,[]),Body,R,S,N1,LH)|LD],Fr0,Fr,C,PC0,PC):-
	member(N,LH),
	(N=N1->
	% the selected head resolves with Call
		consistent(N,R,S,C),
		Fr=[rule(d(Call,[]),Body,R,N,S)|Fr1],
		PC=PC1
	;
	% the selected head does not resolve with Call
		consistent(N,R,S,C),
		Fr=[rule(d('$null',[]),Body,R,N,S)|Fr1],
		PC=PC1
	),
	choose_rules(LD,Fr0,Fr1,C,PC0,PC1).

choose_rulesold([],_LD,Fr,Fr,_C,PC,PC).

choose_rulesold([(R,LH)|LR],LD,Fr0,Fr,C,PC0,PC):-
	member(N,LH),
	(member(rule(d(Call,[]),Body,R,S,N,LH),LD)->
	% the selected head resolves with Call
		consistent(N,R,S,C),
		Fr=[rule(d(Call,[]),Body,R,N,S)|Fr1],
		PC=PC1
	;
	% the selected head does not resolve with Call
		findall(S,member(rule(d(Call,[]),Body,R,S,_N,LH),LD),LS),
		% this is done to handle the case in which there are
		% multiple instances of rule R with different substitutions
		(merge_subs(LS,S)-> 
		% all the substitutions are consistent, their merge is used
			consistent(N,R,S,C),
			Fr=Fr1,
			PC=[rule(d(_Call,[]),Body,R,N,S)|PC1]
		;
		% the substitutions are inconsistent, the empty substitution is used
			rule(R,S,_LH,_Head,_Body),
			consistent(N,R,S,C),
			Fr=Fr1,
			PC=[rule(d(_Call,[]),Body,R,N,S)|PC1]
		)
	),
	choose_rules(LR,LD,Fr0,Fr1,C,PC0,PC1).

merge_subs([],_S).

merge_subs([S|ST],S):-
	merge_subs(ST,S).

merge_subs([],_Call,_S).

merge_subs([(S,Call)|ST],Call,S):-
	merge_subs(ST,Call,S).
	
/* consistent(N,R,S,C)
	 head N of rule R with substitution S is consistent with C
*/
	
consistent(_N,_R,_S,[]):-!.

consistent(N,R,S,[(_N,R1,_S)|T]):-
% different rule
	R\=R1,!,
	consistent(N,R,S,T).
	
consistent(N,R,S,[(N,R,_S)|T]):-
% same rule, same head
	consistent(N,R,S,T).

consistent(N,R,S,[(N1,R,S1)|T]):-
% same rule, different head
	N\=N1,
% different substitutions
	dif(S,S1),
	consistent(N,R,S,T).



map_oldt([],_Ggoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP,C,C,D,D).
map_oldt([Clause|Frames],Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,
	C0,C,D0,D) :-
	edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,
		C0,C1,D0,D1),
	find(Tab0,Ggoal,Ent),
	ent_to_comp(Ent,Comp),
	(Comp \== true->
  		map_oldt(Frames,Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,
		Dep1,Dep,TP1,TP,C1,C,D1,D)
  	;
  		Tab=Tab1,S=S1,Dfn=Dfn1,Dep=Dep1,TP=TP1,C=C1,D=D1
	).

/* edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Clause may be one of the following forms:
          rule(d(H,Dlist),Blist)
          rule(d(H,all(Dlist)),all(Blist))
   where the second form is for general clauses with a universal
   disjunction of literals in the body. Dlist is a list of delayed 
   literals, and Blist is the list of literals to be solved.
   Clause represents a directed edge from Ggoal to the left most 
   subgoal in Blist.
*/
edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    Clause = rule(Ans,B,Rule,Number,Sub,LH),
    ( B == [] ->
      ans_edge(rule(Ans,B,Rule,Number,Sub,LH),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
    ; B = [Lit|_] ->
      ( Lit = (\+N) ->
        neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
      ; pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
      )
    ; B = all(Bl) ->
      ( Bl == [] ->
        ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Bl = [Lit|_],
        ( Lit = (\+N) ->
          aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        ; apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      )
    ).
/* add_ans_to_C(rule(Head,Body,R,N,S),C0,C,D0,D):-
	 adds rule rule(Head,Body,R,N,S) to the C set if it is disjunctive
	 or to the D set if it is definite. The rule is added only if it is consistent
	 with the current C set
*/
add_ans_to_C(rule(_,_,def(N),_,S,_),Tab,C,C,D,[(N,S)|D],true):-!.

add_ans_to_C(rule(Goal,_B,R,N,S,LH),Tab,C0,C,D,D,HeadSelected):-
	Goal=d(G,_D),
  (find(Tab,G,Ent)->
	  ent_to_anss(Ent,Anss),
  	(member_anss(d(G,[]),Anss)->
			C=C0,
			HeadSelected=false
		;
			add_to_C(R,N,S,LH,C0,C,HeadSelected)
		)
	;
		add_to_C(R,N,S,LH,C0,C,HeadSelected)
	).
			
			
			
add_to_C(R,N,S,LH,C0,C,HeadSelected):-
	member(N1,LH),
	(N1=N->
		HeadSelected=true
	;
		HeadSelected=false
	),
	\+ already_present_with_a_different_head(N1,R,S,C0),
	(already_present_with_the_same_head(N1,R,S,C0)->
		C=C0
	;
		C=[(N1,R,S)|C0]
	).
/* already_present_with_the_same_head(N,R,S,C)
	 succeeds if rule R is present in C with head N and substitution S
*/
already_present_with_the_same_head(N,R,S,[(N,R,S)|_T]):-!.

already_present_with_the_same_head(N,R,S,[(_N,_R,_S)|T]):-!,
	already_present_with_the_same_head(N,R,S,T).

/* already_present_with_a_different_head(N,R,S,C)
	 succeeds if rule R is present in C with susbtitution S and a head different
	 from N
*/

already_present_with_a_different_head(N,R,S,[(N1,R,S1)|_T]):-
	different_head(N,N1,S,S1),!.

already_present_with_a_different_head(N,R,S,[(_N1,_R1,_S1)|T]):-
	already_present_with_a_different_head(N,R,S,T).

different_head(N,N1,S,S1):-
	N\=N1,S=S1, !.

/* add_PC_to_C(PossC,C0,C)
	 adds the rules in PossC to C if they are consistent with it, otherwise it
	 fails
*/
add_PC_to_C([],C,C).

add_PC_to_C([rule(H,B,R,N,S)|T],C0,C):-
	add_ans_to_C(rule(H,B,R,N,S),C0,C1,[],[]),
	add_PC_to_C(T,C1,C).

ans_edge(rule(Ans,B,Rule,Number,Sub,LH),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
	add_ans_to_C(rule(Ans,B,Rule,Number,Sub,LH),Tab0,C0,C1,D0,D1,HeadSelected),
	(HeadSelected=false->
		Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0, C=C1, D=D1
	;
		(add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab1) -> 
	      		(Mode = new_head -> 
	        		returned_ans(Ans,Ggoal,RAns),
	        		map_nodes(Nodes,RAns,Tab1,Tab,
					S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C1,C,D1,D)
			;
				Mode = no_new_head ->
	        			Tab = Tab1, S = S0, Dfn = Dfn0, Dep = Dep0, 
					TP = TP0, C=C1, D=D1
	      		)
		; 
			Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0, C=C1, D=D1
		)
	).

neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    Clause = rule(_,[\+N|_],_R,_N,_Sub,_LH),
    ( ground(N) -> true
    ; write('Flounder: '), write(\+N), nl, fail
    ),
    Node = (Ggoal:Clause),
    Ngoal = N,                 % N is already ground
    ( isprolog(N) ->           % if N is a Prolog predicate
      ( call(N) ->             %    then just call
        Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, C=C0, D=D0, TP = TP0
      ; apply_subst(Node,d(\+ N,[]),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C1,D0,D)
      )
    ; ( find(Tab0,Ngoal,Nent) ->
        Tab2 = Tab0, S2 = S0, Dfn2 = Dfn0, Dep2 = Dep0, TP2 = TP0, C2=C0, D2=D0
      ; new_init_call(N,Ngoal,Ent,S0,S1,Dfn0,Dfn1),
				add_tab_ent(Ngoal,Ent,Tab0,Tab1),
				oldt(N,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,maxint-maxint,Ndep,TP0,TP2,C0,C1,D0,D2,PC),
				add_PC_to_C(PC,C1,C2),
				compute_mins(Dep0,Ndep,pos,Dep2),
        find(Tab2,Ngoal,Nent)
      ),
      ent_to_comp(Nent,Ncomp),
      ent_to_anss(Nent,Nanss),
      ( succeeded(Nanss) ->
				Tab = Tab2, S = S2, Dfn = Dfn2, Dep = Dep2, TP = TP2, C =C2, D=D2
      ; failed(Nanss), Ncomp == true ->
        apply_subst(Node,d(\+N,[]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP,C2,C,D2,D)
      ; apply_subst(Node,d(\+N,[\+N]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP,C2,C,D2,D)
      )
    ).

pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    Clause = rule(_H,[N|_B],_R,_N,_Sub,_LH),
    Node = (Ggoal:Clause),
    ground(N,Ngoal),
    ( isprolog(N) ->
      findall(d(N,[]),call(N),Nanss),
      map_anss_list(Nanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,pos,Tab0,Tab1,Dep0,Dep1),
          map_anss(Nanss,Node,Ngoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep1,Dep,TP0,TP,C0,C,D0,D)
        ; % N is completed. 
          map_anss(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
        )
      ; % otherwise N is new
        new_pos_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP,C0,C1,D0,D,PC),
        add_PC_to_C(PC,C1,C),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_H,all([\+N|_B])),
    Node = (Ggoal:Clause),
    ground(N,Ngoal),
    ( isprolog(N) ->
      findall(d(N,[]),call(N),Nanss),
      return_to_disj_list(Nanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,aneg,Tab0,Tab,Dep0,Dep),
          S = S0, Dfn = Dfn0, TP = TP0
        ; % N is completed. 
          return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_aneg_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(d(H,D),all([N|B])),
    ( ground(N) -> true
    ; write('Flounder in a universal disjunction: '), 
      write(N), 
      nl, 
      fail
    ),
    pos_edge(rule(d(H,[]),[N]),Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    edge_oldt(rule(d(H,D),all(B)),Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

apply_subst(Ggoal:Cl,d(An,Vr),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    copy_term(Cl,rule(d(Ac,Vc),Body,R,N,Sub,LH)),
    ( Body = [Call|NBody] ->
      Call = An,
      append(Vr,Vc,Vn)
    ; Body = all([Call|Calls]),
      % Call = An,              % An is the numbervar-ed version of Call.
      ( Vc == [] ->
        Vn = all(Vr)
      ; Vc = all(Vc0),
        append(Vr,Vc0,Vn0),
        Vn = all(Vn0)
      ),
      NBody = all(Calls)
    ),
    edge_oldt(rule(d(Ac,Vn),NBody,R,N,Sub,LH),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D).

/* map_nodes(Nodes,Ans,....):
   return Ans to each of the waiting nodes in Nodes, where a node
   is of the form Ggoal:Clause.
*/  
map_nodes([],_Ans,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP,C,C,D,D).
map_nodes([Node|Nodes],Ans,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,C0,C1,D0,D1),
    map_nodes(Nodes,Ans,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP,C1,C,D1,D).

map_anss([],_Node,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP,C,C,D,D).
map_anss(l(_GH,Lanss),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    ( Lanss == [] ->
      Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0, C=C0, D=D0
    ; Lanss = [Ans|_],
      returned_ans(Ans,Ngoal,RAns),
      apply_subst(Node,RAns,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D)
    ).
map_anss(n2(T1,_,T2),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,C0,C1,D0,D1),
    map_anss(T2,Node,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP,C1,C,D1,D).
map_anss(n3(T1,_,T2,_,T3),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,C0,C1,D0,D1),
    map_anss(T2,Node,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,Dep1,Dep2,TP1,TP2,C1,C2,D1,D2),
    map_anss(T3,Node,Ngoal,Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP,C2,C,D2,D).

map_anss_list([],_Node,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP,C,C,D,D).
map_anss_list([Ans|Lanss],Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP,C0,C,D0,D) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1,C0,C1,D0,D1),
    map_anss_list(Lanss,Node,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP,C1,C,D1,D).

/* return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Nanss: an answer table for Ngoal
   Node: is of the form (Ggoal:Clause), where Clause is of the form
         rule(d(H,D),all([\+N|B]))
   It carries out resolution of each answer with Clause, and constructs
   a new clause rule(Head,NBody), where the body is basically a 
   conjunction of all the resolvents. If a resolvent is a disjunction
   or a non-ground literal, a new proposition is created (which is 
   actually represented by a number), which has a clause whose body
   is the resolvent.
*/
return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal : Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0 : Tcl0),
    negative_return_all(Nanss,Body,Ngoal,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_all([],_Body,_Ngoal,NBody,NBody,N,N,Tcl,Tcl).
negative_return_all(l(_GH,Lanss),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    ( Lanss == [] ->
      NBody0 = NBody, N = N0, Tcl = Tcl0
    ; Lanss = [Ans|_],
      negative_return_one(Ans,Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl)
    ).
negative_return_all(n2(T1,_,T2),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody,N1,N,Tcl1,Tcl).
negative_return_all(n3(T1,_,T2,_,T3),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody2,N1,N2,Tcl1,Tcl2),
    negative_return_all(T3,Body,Ngoal,NBody2,NBody,N2,N,Tcl2,Tcl).

negative_return_one(d(H,Tv),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[\+Call|Bs]),
    H = Call,
    ( Tv == [] ->                    % no delay
      ( (Bs = [Lit], ground(Lit)) -> % resovlent is a ground literal
        NBody0 = [Lit|NBody],
        N = N0, Tcl = Tcl0
      ; Lit = N0,                    % otherwise, replace it with a number
        N is N0+1,
        NBody0 = [Lit|NBody],
        Clause = rule(d(Lit,[]),all(Bs)),
        add_tab_ent(Lit,Clause,Tcl0,Tcl)
      )
    ; ( ground(H) ->                 % if there is delay, always replace with number
	NewTv = [\+H]
      ; ground(H,GH),
	NewTv = [Ngoal - (\+GH)]
      ),
      Lit = N0,
      N is N0+1,
      NBody0 = [Lit|NBody],
      Clause = rule(d(Lit,all(NewTv)),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl)
    ).

return_to_disj_list(Lanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal : Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0 : Tcl0),
    negative_return_list(Lanss,Body,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_list([],_Body,NBody,NBody,N,N,Tcl,Tcl).
negative_return_list([d(H,[])|Lanss],Body,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[\+Call|Bs]),
    H = Call,
    ( Bs = [Lit], ground(Lit) ->
      NBody0 = [Lit|NBody1],
      N1 = N0, Tcl1 = Tcl0
    ; Lit = N0,
      N1 is N0+1,
      NBody0 = [Lit|NBody1],
      Clause = rule(d(Lit,[]),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl1)
    ),
    negative_return_list(Lanss,Body,NBody1,NBody,N1,N,Tcl1,Tcl).

/* comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   check if Ggoal and subgoals on top of it on the stack are
   completely evaluated.
*/
comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Dep0 == maxint-maxint ->
      process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
    ; update_mins(Ggoal,Dep0,pos,Tab0,Tab1,Gdfn,Gdep),
      Gdep = Gpmin-Gnmin,
      ( Gdfn @=< Gpmin, Gnmin == maxint ->
        process_pos_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Gdfn @=< Gpmin, Gdfn @=< Gnmin ->
        process_neg_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Tab = Tab1, S0 = S, Dfn = Dfn0, Dep = Gdep, TP = TP0
      )
    ).

process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfs_trace ->
      write('Stack: '), nl, display_stack(S0,Tab0),
      write('Completed call found: '), write(Ggoal), nl, 
      display_table(Tab0),
      write('Completing calls ......'), nl, nl
    ; true
    ),
    pop_subgoals(Ggoal,S0,S1,[],Scc),
    complete_comp(Scc,Tab0,Tab1,Alist,[]),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn0,Dfn,maxint-maxint,Dep,TP0,TP).

/* pop_subgoals(Ggoal,S0,S,Scc0,Scc)
   pop off the stack subgoals up to and including Ggoal
*/
pop_subgoals(Ggoal,S0,S,Scc0,Scc) :-
    S0 = [Sent|S1],
    ( Ggoal == Sent ->
      S = S1, 
      Scc = [Sent|Scc0]
    ; pop_subgoals(Ggoal,S1,S,[Sent|Scc0],Scc)
    ).

/* complete_comp(Scc,Tab0,Tab,Alist0,Alist):
   process the list Scc of subgoals that are 
   completely evaluated.
*/
complete_comp([],Tab,Tab,Alist,Alist).
complete_comp([Ggoal|Scc],Tab0,Tab,Alist0,Alist) :-
    complete_one(Ggoal,Tab0,Tab1,Alist0,Alist1),
    complete_comp(Scc,Tab1,Tab,Alist1,Alist).

/* complete_one(Ggoal,Tab0,Tab,Alist0,Alist)
   process one subgoal that has been completely 
   evaluated:
   1. set its Nodes and Negs to [] and Comp to true;
   2. simplify its answers and set up links
      for further simplification later;
   3. use the truth value of Ggoal to simplify
      answers of other complete subgoals (possibly 
      including itself).
   4. set Alist0/Alist: a list of negation nodes with
      universal disjunctions with associated answers
      for the selected negative literal.
*/
complete_one(Ggoal,Tab0,Tab,Alist0,Alist) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab1),
    Ent0 = e(_Nodes,ANegs,Anss0,Delay,_Comp,Gdfn,Slist0),
    Ent = e([],[],Anss,Delay,true,Gdfn,Slist),
    ( Delay == true ->
      reduce_ans(Anss0,Anss,Tab0),
      setup_simp_links(Anss,Ggoal,Slist0,Slist1,Tab1,Tab2)
    ; % Delay == false
      Anss = Anss0,
      Tab2 = Tab1,
      Slist1 = Slist0
    ),
    extract_known(Ggoal,Anss,Slist1,Slist,Klist),
    simplify(Klist,Tab2,Tab,[]),
    ( ANegs == [] ->
      Alist0 = Alist
    ; Alist0 = [(Anss,Ggoal)-ANegs|Alist]
    ).

setup_simp_links([],_,Slist,Slist,Tab,Tab).
setup_simp_links(l(GH,Lanss),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab).
setup_simp_links(n2(T1,_,T2),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist,Tab1,Tab).
setup_simp_links(n3(T1,_,T2,_,T3),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist2,Tab1,Tab2),
    setup_simp_links(T3,Ggoal,Slist2,Slist,Tab2,Tab).

/* setup_simp_link_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab)
   Ggoal-GH is to tell what portion of answers of Ggoal can be 
   simplified.
*/
setup_simp_links_list([],_,_,Slist,Slist,Tab,Tab).
setup_simp_links_list([d(_,D)|Anss],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = all(Ds) ->
      true
    ; Ds = D
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links_list(Anss,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* A link ((Ggoal-GH):Lit) in an entry for Ngoal means that 
   the literal Lit in an answer with head GH in Ggoal can 
   be potentially simplified if we know answers for Ngoal.
*/
links_from_one_delay([],_,_,Slist,Slist,Tab,Tab).
links_from_one_delay([D|Ds],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = (\+ Ngoal) ->
      ( Ggoal == Ngoal ->
        Tab1 = Tab0,
	Slist1 = [GHead:D|Slist0]
      ; add_link_to_ent(Tab0,Ngoal,GHead:D,Tab1),
	Slist1 = Slist0
      )
    ; D = (Ngoal-_) ->
      ( Ggoal == Ngoal ->
        Slist1 = [GHead:D|Slist0],
        Tab1 = Tab0
      ; Slist1 = Slist0,
        add_link_to_ent(Tab0,Ngoal,GHead:D,Tab1)
      )
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* extract_known(Ggoal,Anss,Links,Slist,Klist):
   Given Ggoal and its answers Anss, and its 
   simplification Links, it partitioned Links 
   into Slist and Klist of links, where Klist 
   is a list of links that are known to be either
   true or false.

   Klist is either of the form Val-Links, or a
   list of the form Val-Link. In case of non-ground
   calls, the corresponding portion of Anss has to 
   be searched.
*/
extract_known(Ggoal,Anss,Links,Slist,Klist) :-
    ( failed(Anss) ->
      Klist = fail-Links,
      Slist = []
    ; Anss = l(GH,Lanss) ->
      ( Ggoal == GH ->       % Ground or most general call
	( memberchk(d(_,[]),Lanss) ->
	  Klist = succ-Links,
	  Slist = []
        ; Klist = [],
	  Slist = Links
        )
      ; % non-ground call
	extract_known_anss(Links,Anss,[],Slist,[],Klist)
      )
    ; % non-ground call
      extract_known_anss(Links,Anss,[],Slist,[],Klist)
    ).
      
extract_known_anss([],_,Slist,Slist,Klist,Klist).
extract_known_anss([Link|Links],Anss,Slist0,Slist,Klist0,Klist) :-
    Link = (_:Lit),
    extract_lit_val(Lit,Anss,true,Val),
    ( Val == undefined ->
      Slist1 = [Link|Slist0],
      Klist1 = Klist0
    ; Slist1 = Slist0,
      Klist1 = [Val-Link|Klist0]
    ),
    extract_known_anss(Links,Anss,Slist1,Slist,Klist1,Klist).

/* extract_lit_val(Lit,Anss,Comp,Val):
   extract the truth value of Lit according to Anss and Comp.
   In case of a non-ground calls, the corresponding portion
   of Anss has to be searched.
*/
extract_lit_val(Lit,Anss,Comp,Val) :-
    ( Lit = (\+ _) ->
      ( succeeded(Anss) ->
        Val = fail
      ; failed(Anss), Comp == true ->
        Val = succ
      ; Val = undefined
      )
    ; Lit = (_ - (\+GH)) ->
      ( find(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(d(GH,[]),Lanss)) ->
          Val = fail
        ; Lanss == [], Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      )
    ; Lit = (_-GH) ->
      ( find(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(d(GH,[]),Lanss)) ->
          Val = succ
        ; Lanss == [], Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      )
    ).

/* simplify(KnownLinks,Tab0,Tab,Abd):
   Given a list of KnownLinks, Tab0 and Abd,
   it tries to simplify answers according to
   KnownLinks. When a subgoal is found to be
   true or false according to answers, 
   consistency with assumed truth values in Abd
   is checked.
*/
simplify([],Tab,Tab,_Abd).
simplify([Val-Link|Klist],Tab0,Tab,Abd) :-
    simplify_one(Val,Link,Tab0,Tab1,Abd),
    simplify(Klist,Tab1,Tab,Abd).
simplify(Val-Links,Tab0,Tab,Abd) :-
    simplify_list(Links,Val,Tab0,Tab,Abd).

simplify_list([],_,Tab,Tab,_Abd).
simplify_list([Link|Links],Val,Tab0,Tab,Abd) :-
    Link = (_ : Lit),
    ( ( Lit = (\+_); Lit = (_ - (\+_)) ) ->
      ( Val = fail -> LVal = succ; LVal = fail )
    ; LVal = Val
    ),
    simplify_one(LVal,Link,Tab0,Tab1,Abd),
    simplify_list(Links,Val,Tab1,Tab,Abd).

simplify_one(Val,Link,Tab0,Tab,Abd) :-
    Link = ((Ngoal - GH) : Lit),
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    Ent0 = e(Nodes,ANegs,Anss0,Delay,Comp,Dfn,Slist0),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      simplify_anss(Lanss0,Val,Lit,[],Lanss,C),
      ( C == true ->
	( find(Abd,GH,Aval) ->
	  ( Aval == true, Lanss == [] -> % deduced result inconsistent with assumption
	    fail
	  ; Aval == false, memberchk( d(_ , []), Lanss) ->
	    fail
	  ; true
          )
	; true
        ),
        extract_known(Ngoal,Anss,Slist0,Slist,Klist),
        simplify(Klist,Tab1,Tab,Abd)
      ; Tab = Tab0
      )
    ; Tab = Tab0
    ).

/* simplify_anss(List,Val,Lit,Lanss0,Lanss,C):
   Given a List of answers, Val of Lit, it 
   simplifies the List and construct a new list
   Lanss0/Lanss of answers. C is unified with true
   if some simplification is carried out.

   As soon as a true answer is detected, all
   other answers with the same head are deleted.
*/
simplify_anss([],_,_,Anss,Anss,_).
simplify_anss([Ans|Rest],Val,Lit,Anss0,Anss,C) :-
    ( simplified_ans(Ans,Val,Lit,NewAns,C) ->
      ( NewAns = d(_,[]) ->
        Anss = [NewAns]
      ; Anss1 = [NewAns|Anss0],
        simplify_anss(Rest,Val,Lit,Anss1,Anss,C)
      )
    ; C = true,
      simplify_anss(Rest,Val,Lit,Anss0,Anss,C)
    ).

simplified_ans(Ans,Val,Lit,NewAns,C) :-
    Ans = d(H,Ds),
    ( Ds == [] ->
      NewAns = Ans
    ; Ds = all(Dlist) ->
      ( Val == fail ->
        delete_lit(Dlist,Lit,NewDlist,[],C),
        ( NewDlist == [] ->
          fail
        ; NewAns = d(H,all(NewDlist))
        )
      ; % Val == succ ->
        ( memberchk(Lit,Dlist) ->
          NewAns = d(H,[]),
          C = true
        ; NewAns = Ans
        )
      )
    ; % Ds is a conjunction
      ( Val == fail ->
        ( memberchk(Lit,Ds) ->
          fail
        ; NewAns = Ans
        )
      ; % Val == succ ->
        delete_lit(Ds,Lit,NewDs,[],C),
        NewAns = d(H,NewDs)
      )
    ).

/* delete_lit(Delays,Lit,Ds0,Ds,C):
   deletes Lit from Delays. Delays is 
   a list of delayed literals and it
   is guaranteed to have no duplicates.
*/
delete_lit([],_,Ds,Ds,_).
delete_lit([D|Rest],Lit,Ds0,Ds,C) :-
    ( D == Lit ->
      Ds0 = Rest,
      C = true
    ; Ds0 = [D|Ds1],
      delete_lit(Rest,Lit,Ds1,Ds,C)
    ).

% return answers to negative nodes within universal disjunctions
return_aneg_nodes([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
return_aneg_nodes([(Anss,Ngoal)-ANegs|Alist],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anegs(ANegs,Anss,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_anegs([],_Anss,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anegs([Node|ANegs],Anss,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    return_to_disj(Anss,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anegs(ANegs,Anss,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* process a component of subgoals that may be involved in 
   negative loops.
*/
process_neg_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfs_trace ->
      write('Stack: '), nl, display_stack(S0,Tab0),
      write('Possible negative loop: '), write(Ggoal), nl, 
      display_table(Tab0)
    ; true
    ),
    extract_subgoals(Ggoal,S0,Scc,[]),
    reset_nmin(Scc,Tab0,Tab1,Ds,[]),
    ( wfs_trace ->
      write('Delaying: '), display_dlist(Ds)
    ; true
    ),
    delay_and_cont(Ds,Tab1,Tab2,S0,S1,Dfn0,Dfn1,maxint-maxint,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab2,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* extract_subgoals(Ggoal,S0,Scc0,Scc)
   extract subgoals that may be involved in negative loops,
   but leave the stack of subgoals intact.
*/
extract_subgoals(Ggoal,[Sent|S],[Sent|Scc0],Scc) :-
    ( Ggoal == Sent ->
      Scc0 = Scc
    ; extract_subgoals(Ggoal,S,Scc0,Scc)
    ).

/* reset_nmin(Scc,Tab0,Tab,Dnodes0,Dnodes)
   reset NegLink and collect all waiting nodes that need to be 
   delayed. Dnodes0/Dnodes is a difference list.
*/
reset_nmin([],Tab,Tab,Ds,Ds).
reset_nmin([Ggoal|Scc],Tab0,Tab,Ds0,Ds) :-
    get_and_reset_negs(Tab0,Ggoal,ANegs,Tab1),
    ( ANegs == [] ->
      Ds0 = Ds1
    ; Ds0 = [Ggoal-ANegs|Ds1]
    ),
    reset_nmin(Scc,Tab1,Tab,Ds1,Ds).

delay_and_cont([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
delay_and_cont([Ggoal-Negs|Dnodes],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_nodes(Negs,d(\+Ggoal,[\+Ggoal]),Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    delay_and_cont(Dnodes,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

recomp_scc([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
recomp_scc([Ggoal|Scc],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    comp_tab_ent(Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* routines for incremental update of dependency information
*/

/* update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep)
   update the PosLink and NegLink of Ggoal according to 
   Dep and Sign
*/
update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn:Gdep0,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn:Gdep,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    compute_mins(Gdep0,Dep,Sign,Gdep).

/* update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep)
   There is a lookup edge (Node) from Ggoal to Ngoal 
   with Sign. It adds Node to the corresponding waiting list
   in Ngoal and then update the dependencies of Ggoal.
*/
update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep) :-
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    ( Sign == pos ->
      pos_to_newent(Ent0,Ent,Node)
    ; Sign == aneg ->
      aneg_to_newent(Ent0,Ent,Node)
    ),
    Ent0 = e(_,_,_,_,_,_Ndfn:Ndep,_),
    compute_mins(Dep0,Ndep,Sign,Dep),
    update_mins(Ggoal,Ndep,Sign,Tab1,Tab,_,_).

/* update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep)
   There is an edge with Sign from Ggoal to Ngoal, where Ngoal is 
   a new subgoal. Ndep is the final dependency information of 
   Ngoal. Dep0/Dep is for the most recent enclosing new call.
   This predicate is called after Ngoal is solved.
*/
update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep) :-
    find(Tab0,Ngoal,Nent),
    ent_to_comp(Nent,Ncomp),
    ( Ncomp == true ->
      ( Ndep == maxint-maxint ->
        Tab = Tab0, Dep = Dep0
      ; update_mins(Ggoal,Ndep,pos,Tab0,Tab,_,_),
        compute_mins(Dep0,Ndep,pos,Dep)
      )
    ; update_mins(Ggoal,Ndep,Sign,Tab0,Tab,_,_),
      compute_mins(Dep0,Ndep,Sign,Dep)
    ).

compute_mins(Gpmin-Gnmin,Npmin-Nnmin,Sign,Newpmin-Newnmin) :-
    ( Sign == pos ->
      min(Gpmin,Npmin,Newpmin),
      min(Gnmin,Nnmin,Newnmin)
    ; % (Sign == neg; Sign == aneg) ->
      Newpmin=Gpmin,
      min(Gnmin,Npmin,Imin), 
      min(Imin,Nnmin,Newnmin)
    ).
    
min(X,Y,M) :- ( X @< Y -> M=X; M=Y ).

%%%%%%%%%%%%%%% Local table manipulation predicates %%%%%%%%%%

/* Table Entry Structure:
   For each Call, its table entry is identified with its number-vared
   version -- Ggoal. Its value is a term of the form

    e(Nodes,ANegs,Anss,Delay,Comp,Dfn:Dep,Slist)

   where
     Nodes:  positive suspension list
     ANegs:  negative suspension list (for universal disjunction clauss)
     Anss:   another table.
     Delay:  whether Anss contains any answer with delay
     Comp:   whether Call is completely evaluated or not
     Dfn:    depth-first number of Gcall
     Dep:    (PosLink-NegLink) --- dependency information
     Slist:  a list of nodes whose answers may be simplified
             if the truth value of Ggoal is known. Each element of Slist
         is of the form (Ngoal-GH):Literal.
   Stack Entry Structure:
     Ggoal
*/

/* routines for accessing individual fields of an entry
*/
ent_to_nodes(e(Nodes,_,_,_,_,_,_),Nodes).
ent_to_anegs(e(_,ANegs,_,_,_,_,_),ANegs).
ent_to_anss(e(_,_,Anss,_,_,_,_),Anss).
ent_to_delay(e(_,_,_,Delay,_,_,_),Delay).
ent_to_comp(e(_,_,_,_,Comp,_,_),Comp).
ent_to_dfn(e(_,_,_,_,_,Dfn,_),Dfn).
ent_to_slist(e(_,_,_,_,_,_,Slist),Slist).

get_and_reset_negs(Tab0,Ggoal,ANegs,Tab) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn: (Gpmin - _),Slist),
    Ent = e(Nodes,[],Anss,Delay,Comp,Gdfn:Gpmin-maxint,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab).

/* adding a new table entry
*/
add_tab_ent(Ggoal,Ent,Tab0,Tab) :- 
    addkey(Tab0,Ggoal,Ent,Tab).

/* The following three routines are for creating
   new calls
*/
/* a new call with empty suspensions 
*/
new_init_call(Call,Ggoal,Ent,S0,S,Dfn0,Dfn) :-
    ground(Call,Ggoal),
    S = [Ggoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[],[],false,false,Dfn0:Dfn0-maxint,[]).

/* a new call with an initial negative suspension from 
   inside a universal disjunction
*/
new_aneg_call(Ngoal,Neg,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[Neg],[],false,false,Dfn0:Dfn0-maxint,[]).

/* a new call with an initial positive suspension
*/
new_pos_call(Ngoal,Node,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([Node],[],[],false,false,Dfn0:Dfn0-maxint,[]).

/* routines for adding more information to a
   table entry.
*/
aneg_to_newent(Ent0,Ent,ANeg) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,[ANeg|ANegs],Anss,Delay,Comp,Dfn,Slist).

pos_to_newent(Ent0,Ent,Node) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e([Node|Nodes],ANegs,Anss,Delay,Comp,Dfn,Slist).

add_link_to_ent(Tab0,Ggoal,Link,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    link_to_newent(Ent0,Ent,Link).

link_to_newent(Ent0,Ent,Link) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,[Link|Slist]).

/* routines for manipulating answers */
ansstree_to_list([],L,L).
ansstree_to_list(l(_GH,Lanss),L0,L) :-
    attach(Lanss,L0,L).
ansstree_to_list(n2(T1,_M,T2),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L).
ansstree_to_list(n3(T1,_M2,T2,_M3,T3),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L2),
    ansstree_to_list(T3,L2,L).

attach([],L,L).
attach([d(H,B)|R],[X|L0],L) :-
    ( B == [] ->
      X = H
    ; X = (H <- B)
    ),
    attach(R,L0,L).

member_anss(Ans,Anss) :-
	member_anss_1(Anss,Ans).

member_anss_1(l(_,Lanss),Ans) :-
	member(Ans,Lanss).
member_anss_1(n2(T1,_,T2),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ).
member_anss_1(n3(T1,_,T2,_,T3),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ; member_anss_1(T3,Ans)
        ).

/* failed(Anss): Anss is empty */
failed([]).
failed(l(_,[])).

/* succeeded(Anss): Anss contains a single definite answer */
succeeded(l(_,Lanss)) :-
	memberchk(d(_,[]),Lanss).

/* add_ans(Tab0,Goal,Ans,Nodes,Mode,Tab):
   If Ans is not subsumed by any existing answer then
      Ans is added to Anss(Goal);
      If some existing answer also has head H then
         Mode = no_new_head
      else 
         Mode = new_head
   else
      fail.
*/
add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab) :-
	Ans = d(H,[]),
	ground(H),
	H=Ggoal,!,
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
      new_ans_ent1(Ent0,Ent,Ans,Nodes,Mode).


add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    Ans = d(H,Ds),
    ( Ds == [] ->
      new_ans_ent(Ent0,Ent,Ans,Nodes,Mode)
    ; setof(X,member(X,Ds),NewDs),
      new_ans_ent(Ent0,Ent,d(H,NewDs),Nodes,Mode)
    ).

new_ans_ent1(Ent0,Ent,Ans,Nodes,Mode) :-
    Ent0 = e(Nodes,ANegs,Anss0,Delay0,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,true,Dfn,Slist),
    Ans = d(H,[]),
    ( updatevs(Anss0,H,Lanss0,Lanss,Anss) ->
        Lanss = [Ans],
      Mode = no_new_head
    ; addkey(Anss0,H,[Ans],Anss),
      Mode = new_head
    ),
      Delay = Delay0
    .


new_ans_ent(Ent0,Ent,Ans,Nodes,Mode) :-
    Ent0 = e(Nodes,ANegs,Anss0,Delay0,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ans = d(H,D),
    ground(H,GH),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      ( D == [] ->
        \+(memberchk(d(_,[]),Lanss0)),
        Lanss = [Ans]
      ; not_subsumed_ans(Ans,Lanss0),
        Lanss = [Ans|Lanss0]
      ),
      Mode = no_new_head
    ; addkey(Anss0,GH,[Ans],Anss),
      Mode = new_head
    ),
    ( D == [] -> 
      Delay = Delay0
    ; Delay = true
    ).

/* returned_ans(Ans,Ggoal,RAns):
   determines whether SLG resolution or SLG factoring should 
   be applied.
*/
returned_ans(d(H,Tv),Ggoal,d(H,NewTv)) :-
    ( Tv = [] ->
      NewTv = []
    ; ground(H,GH),
      NewTv = [Ggoal-GH]
    ).

% reduce a list of answers, by reducing delay list, and by subsumption
reduce_ans(Anss0,Anss,Tab) :-
    reduce_completed_ans(Anss0,Anss,Tab).

% simplify all the delay lists in a list of answers.
reduce_completed_ans([],[],_Tab).
reduce_completed_ans(l(GH,Lanss0),l(GH,Lanss),Tab) :-
    reduce_completed_anslist(Lanss0,[],Lanss,Tab).
reduce_completed_ans(n2(T1,M,T2),n2(NT1,M,NT2),Tab) :-
    reduce_completed_ans(T1,NT1,Tab),
    reduce_completed_ans(T2,NT2,Tab).
reduce_completed_ans(n3(T1,M2,T2,M3,T3),n3(NT1,M2,NT2,M3,NT3),Tab) :-
    reduce_completed_ans(T1,NT1,Tab),
    reduce_completed_ans(T2,NT2,Tab),
    reduce_completed_ans(T3,NT3,Tab).

reduce_completed_anslist([],Lanss,Lanss,_Tab).
reduce_completed_anslist([d(G,D0)|List],Lanss0,Lanss,Tab) :-
    ( D0 = all(Dlist1) ->
      ( filter_delays(Dlist1,[],Dlist,disj,V,Tab) ->
        ( V == true ->       % true answer
          Lanss = [d(G,[])]
        ; Dlist == [] ->     % false answer, ignore
          reduce_completed_anslist(List,Lanss0,Lanss,Tab)
        ; reduce_completed_anslist(List,[d(G,all(Dlist))|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ; ( filter_delays(D0,[],D,conj,_V,Tab) ->
	( D == [] ->
	  Lanss = [d(G,[])]
        ; reduce_completed_anslist(List,[d(G,D)|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ).

% simplify a delay list by the completed table: delete true negations,
%    fail if a false one.
filter_delays([],Fds,Fds,_DC,_V,_Tab).
filter_delays([Lit|Ds],Fds0,Fds,DC,V,Tab) :-
    lit_to_call(Lit,Gcall),
    find(Tab,Gcall,Gent),
    ent_to_comp(Gent,Gcomp),
    ent_to_anss(Gent,Ganss),
    extract_lit_val(Lit,Ganss,Gcomp,Val),
    ( Val == succ ->
      ( DC == conj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      ; DC == disj ->
        V = true
      )
    ; Val == fail ->
      ( DC == conj ->
        fail
      ; DC == disj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      )
    ; % Val == undefined
      filter_delays(Ds,[Lit|Fds0],Fds,DC,V,Tab)
    ).

lit_to_call(\+G,G).
lit_to_call(Gcall-_,Gcall).

not_subsumed_ans(Ans,Lanss0) :-
    \+
    ( numbervars(Ans,0,_),
      subsumed_ans1(Ans,Lanss0)
    ).

% succeed if answer is subsumed by any in list1 or 2.
subsumed_ans(Tv,List1,List2) :- 
    \+ 
    (numbervars(Tv,0,_),
     \+ subsumed_ans1(Tv,List1),
     \+ subsumed_ans1(Tv,List2)
    ).

% check if a delay is subsumed one of the element in the list
subsumed_ans1(d(T,V),List) :-
    member(d(T,V1),List),
    ( V1 == []
    ; V = all(LV), V1 = all(LV1) ->
      subset(LV,LV1)
    ; subset(V1,V)
    ).

/****************** auxiliary routines *******************/
% variantchk/2 finds a variant in a list of atoms.
variantchk(G,[G1|_]) :- variant(G,G1), !.
variantchk(G,[_|L]) :- variantchk(G,L).

variant(A, B) :-
    A == B
     ->    true
     ;     subsumes_chk(A, B),
           subsumes_chk(B, A),
           A = B.
/*
subsumes_chk(General, Specific) :-
        \+ (    numbervars(Specific, 0, _),
                \+ General = Specific
         ).
*/
ground(O,C) :- ground(O) -> C = O ; copy_term(O,C), numbervars(C,0,_).

subset([],_).
subset([E|L1],L2) :- memberchk(E,L2), subset(L1,L2).

reverse([],R,R).
reverse([Goal|Scc],R0,R) :- reverse(Scc,[Goal|R0],R).

/***************** routines for debugging *******************/

% Debugging help: pretty-prints strongly connected components and local table.
display_stack(Stack,Tab) :-
    reverse(Stack,[],Rstack),
    display_st(Rstack,Tab).
display_st([],_Tab).
display_st([Ggoal|Scc],Tab) :-
    find(Tab,Ggoal,Ent),
    ent_to_dfn(Ent,Dfn:Pmin-Nmin),
    tab(2), 
    write(Ggoal-Dfn),
    write(':  '),
    write('Pmin='),
    write(Pmin),
    write(';  '),
    write('Nmin='),
    write(Nmin),
    write(';  '),
    nl,
    display_st(Scc,Tab).

display_dlist([]) :- nl,nl.
display_dlist([Ngoal-_|Dlist]) :-
    write(\+ Ngoal), 
    write('; '), 
    display_dlist(Dlist).

display_table(Tab) :-
    write('Table: '), 
    nl,
    write_tab(Tab).

display_final(Tab) :-
    write(' Final Set of Answers: '), 
    nl,
    display_final1(Tab).
display_final1([]).
display_final1(l(_,e(_,_,Anss,_,_,_,_))) :-
    write_anss(Anss).
display_final1(n2(X,_,Y)) :- 
    display_final1(X),
    display_final1(Y).
display_final1(n3(X,_,Y,_,Z)) :- 
    display_final1(X),
    display_final1(Y),
    display_final1(Z).

write_tab([]).
write_tab(l(G,e(Nodes,ANegs,Anss,_,Comp,Dfn:_,_))) :-
    write(' Entry: '),
    write(G-Dfn),
    write(': '),
    ( Comp == true -> 
      write('Complete!')
    ; write('Incomplete!') 
    ), 
    nl,
    ( Anss == [] -> 
      true
    ; write('   Anss: '), 
      nl,
      write_anss(Anss)
    ),
    ( ( Comp == true; Nodes == []) -> 
      true 
    ; write('   Nodes: '),
      write(Nodes),
      nl
    ),
    ( ( Comp == true; ANegs == []) ->
      true
    ; write('   ANegs: '),
      write(ANegs),
      nl
    ).
write_tab(n2(X,_,Y)) :- 
    write_tab(X),
    write_tab(Y).
write_tab(n3(X,_,Y,_,Z)) :- 
    write_tab(X),
    write_tab(Y),
    write_tab(Z).

write_anss([]).
write_anss(l(_,Lanss)) :-
    write_anss_list(Lanss).
write_anss(n2(T1,_,T2)) :-
    write_anss(T1),
    write_anss(T2).
write_anss(n3(T1,_,T2,_,T3)) :-
    write_anss(T1),
    write_anss(T2),
    write_anss(T3).

write_anss_list([]).
write_anss_list([Ans|Anss]) :-
    write_ans(Ans),
    write_anss_list(Anss).

write_ans(d(H,Ds)) :-
    write('         '), 
    write(H),
    ( Ds == [] -> 
      true
    ; write(' :- '),
      ( Ds = all([D|Ds1]) ->
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,'; ')
      ; Ds = [D|Ds1],
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,', ')
      )
    ), 
    write('.'), 
    nl.
write_delay([],_).
write_delay([D|Ds1],Sep) :-
    write(Sep),
    ( D = (_Gcall-GH) -> 
      write(GH)
    ; write(D) 
    ),
    write_delay(Ds1,Sep).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
This is a set of routines that supports indexed tables. Tables
are sets of key-value_list pairs. With each key is associated a list
of values. It uses 2-3 trees for the index (modified by D.S. Warren
from Ivan Bratko: ``Prolog Programming for Artificial
Intelligence'', Addison Wesley, 1986). Operations are: 

Keys must be ground! (so numbervar them)

addkey(Tree,Key,V,Tree1) adds a new Key with value V, returning 
    new Tree1. Fails if the key is already there.

find(Tree,Key,V) finds the entry with Key and returns associated
    values in V.

updatevs(Tree,Key,OldV,NewV,Tree1) replaces value of entry with key
    Key and value OldV with NewV.
*/


addkey([],X,V,l(X,V)):-!.
addkey(Tree,X,V,Tree1) :-
	ins2(Tree,X,V,Trees),
	cmb0(Trees,Tree1).


find(l(X,V),Xs,V) :- X == Xs.
find(n2(T1,M,T2),X,V) :-
	M @=< X
	 ->	find(T2,X,V)
	 ;	find(T1,X,V).
find(n3(T1,M2,T2,M3,T3),X,V) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	find(T3,X,V)
		 ;	find(T2,X,V)
		)
	 ;	find(T1,X,V).


% updatevs(Tab0,X,Ov,Nv,Tab) updates Tab0 to Tab, by replacing
% Ov of entry with key X by Nv.
/*
updatevs(Tab0,X,Ov,Nv,Tab) :-
	updatevs(Tab0,X,Ov,Nv),
	Tab = Tab0.

updatevs(Tab,X,Ov,Nv) :-
	( Tab = l(Xs,Ov), Xs == X ->
	  setarg(2,Tab,Nv)
        ; Tab = n2(T1,M,T2) ->
	  ( M @=< X ->
	    updatevs(T2,X,Ov,Nv)
	  ; updatevs(T1,X,Ov,Nv)
          )
        ; Tab = n3(T1,M2,T2,M3,T3) ->
	  ( M2 @=< X ->
	    ( M3 @=< X ->
	      updatevs(T3,X,Ov,Nv)
	    ; updatevs(T2,X,Ov,Nv)
	    )
	  ; updatevs(T1,X,Ov,Nv)
          )
        ).
*/

updatevs(l(X,Ov),Xs,Ov,Nv,l(X,Nv)) :- X == Xs.
updatevs(n2(T1,M,T2),X,Ov,Nv,n2(NT1,M,NT2)) :-
	M @=< X
	 ->	NT1=T1, updatevs(T2,X,Ov,Nv,NT2)
	 ;	NT2=T2, updatevs(T1,X,Ov,Nv,NT1).
updatevs(n3(T1,M2,T2,M3,T3),X,Ov,Nv,n3(NT1,M2,NT2,M3,NT3)) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	NT2=T2, NT1=T1, updatevs(T3,X,Ov,Nv,NT3)
		 ;	NT1=T1, NT3=T3, updatevs(T2,X,Ov,Nv,NT2)
		)
	 ;	NT2=T2, NT3=T3, updatevs(T1,X,Ov,Nv,NT1).

ins2(n2(T1,M,T2),X,V,Tree) :- 
	M @=< X
	 ->	ins2(T2,X,V,Tree1),
		cmb2(Tree1,T1,M,Tree)
	 ;	ins2(T1,X,V,Tree1),
		cmb1(Tree1,M,T2,Tree).
ins2(n3(T1,M2,T2,M3,T3),X,V,Tree) :- 
	M2 @=< X
	 ->	(M3 @=< X
		 ->	ins2(T3,X,V,Tree1),
			cmb4(Tree1,T1,M2,T2,M3,Tree)
		 ;	ins2(T2,X,V,Tree1),
			cmb5(Tree1,T1,M2,M3,T3,Tree)
		)
	 ;	ins2(T1,X,V,Tree1),
		cmb3(Tree1,M2,T2,M3,T3,Tree).
ins2(l(A,V),X,Vn,Tree) :-
	A @=< X
	 ->	(X @=< A
		 ->	fail
		 ;	Tree = t(l(A,V),X,l(X,Vn))
		)
	 ;	Tree = t(l(X,Vn),A,l(A,V)).

cmb0(t(Tree),Tree).
cmb0(t(T1,M,T2),n2(T1,M,T2)).

cmb1(t(NT1),M,T2,t(n2(NT1,M,T2))).
cmb1(t(NT1a,Mb,NT1b),M,T2,t(n3(NT1a,Mb,NT1b,M,T2))).

cmb2(t(NT2),T1,M,t(n2(T1,M,NT2))).
cmb2(t(NT2a,Mb,NT2b),T1,M,t(n3(T1,M,NT2a,Mb,NT2b))).

cmb3(t(NT1),M2,T2,M3,T3,t(n3(NT1,M2,T2,M3,T3))).
cmb3(t(NT1a,Mb,NT1b),M2,T2,M3,T3,t(n2(NT1a,Mb,NT1b),M2,n2(T2,M3,T3))).

cmb4(t(NT3),T1,M2,T2,M3,t(n3(T1,M2,T2,M3,NT3))).
cmb4(t(NT3a,Mb,NT3b),T1,M2,T2,M3,t(n2(T1,M2,T2),M3,n2(NT3a,Mb,NT3b))).

cmb5(t(NT2),T1,M2,M3,T3,t(n3(T1,M2,NT2,M3,T3))).
cmb5(t(NT2a,Mb,NT2b),T1,M2,M3,T3,t(n2(T1,M2,NT2a),Mb,n2(NT2b,M3,T3))).




:-dynamic rule/5,def_rule/4,setting/2.

/* start of list of parameters that can be set by the user with
set(Parameter,Value) */
setting(epsilon_parsing,0.00001).
setting(save_dot,false).
setting(ground_body,true).

/* find_rule(G,(R,S,N),Body,C) takes a goal G and the current C set and
returns the index R of a disjunctive rule resolving with G together with
the index N of the resolving head, the substitution S and the Body of the 
rule */
find_rule(H,(R,S,N),Body,LH):-
	rule(R,S,_,Head,Body),
	member_head(H,Head,0,N),
	length(Head,NH),
	listN(0,NH,LH).

find_rule(H,(R,S,Number),Body,C):-
	rule(R,S,_,uniform(H:1/_Num,_P,Number),Body),
	not_already_present_with_a_different_head(Number,R,S,C).


not_already_present_with_a_different_head(_N,_R,_S,[]).

not_already_present_with_a_different_head(N,R,S,[(N1,R,S1)|T]):-
	not_different(N,N1,S,S1),!,
	not_already_present_with_a_different_head(N,R,S,T).
		
not_already_present_with_a_different_head(N,R,S,[(_N1,R1,_S1)|T]):-
	R\==R1,
	not_already_present_with_a_different_head(N,R,S,T).


not_different(N,N,S,S).

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


rem_dup_lists_tab([],L,L).

rem_dup_lists_tab([(H,_Tab)|T],L0,L):-
	(member_subset_tab(H,T);member_subset_tab(H,L0)),!,
	rem_dup_lists_tab(T,L0,L).

rem_dup_lists_tab([(H,Tab)|T],L0,L):-
	rem_dup_lists_tab(T,[(H,Tab)|L0],L).


member_subset_tab(E,[(H,_Tab)|_T]):-
	subset_my(H,E),!.

member_subset_tab(E,[_H|T]):-
	member_subset_tab(E,T).
	
/* predicates for building the formula to be converted into a BDD */

/* build_formula(LC,Formula,VarIn,VarOut) takes as input a set of C sets
LC and a list of Variables VarIn and returns the formula and a new list
of variables VarOut 
Formula is of the form [Term1,...,Termn]
Termi is of the form [Factor1,...,Factorm]
Factorj is of the form (Var,Value) where Var is the index of
the multivalued variable Var and Value is the index of the value
*/
build_formula([],[],Var,Var).

build_formula([D|TD],[F|TF],VarIn,VarOut):-
	build_term(D,F,VarIn,Var1),
	build_formula(TD,TF,Var1,VarOut).

build_term([],[],Var,Var).

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
	rule(R,S,_N,Head,_Body),
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
	
/* p(File) parses the file File.cpl. It can be called more than once without 
exiting yap */
p(File):-
	parse(File).

parse(File):-
	atom_concat(File,'.cpl',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
	retractall(rule(_,_,_,_,_)),
	retractall(def_rule(_,_,_,_)),
	retractall(new_number(_)),
	assert(new_number(0)),
	process_clauses(C,1),!.

process_clauses([(end_of_file,[])],_N).

process_clauses([((H:-B),V)|T],N):-
	H=uniform(A,P,L),!,
	list2and(BL,B),
	process_body(BL,V,V1),
	remove_vars([P],V1,V2),
	append(BL,[length(L,Tot),nth0(Number,L,P)],BL1),
	append(V2,['Tot'=Tot],V3),
	assertz(rule(N,V3,_NH,uniform(A:1/Tot,L,Number),BL1)),
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
	assertz(rule(N,V1,NH,HL,BL)),
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
	assertz(rule(N,V1,NH,HL,BL)),
	N1 is N+1,
	process_clauses(T,N1).
	
process_clauses([((H:-B),V)|T],N):-!,
	list2and(BL,B),
	assert(def_rule(N,V,H,BL)),
	N1 is N+1,
	process_clauses(T,N1).

process_clauses([(H,V)|T],N):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	length(HL,LH),
	listN(0,LH,NH),
	assertz(rule(N,V,NH,HL,[])),
	N1 is N+1,
	process_clauses(T,N1).

process_clauses([(H,V)|T],N):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,HL),
	length(HL,LH),
	listN(0,LH,NH),
	assertz(rule(N,V,NH,HL,[])),
	N1 is N+1,
	process_clauses(T,N1).
	
process_clauses([(H,V)|T],N):-
	assert(def_rule(N,V,H,[])),
	N1 is N+1,
	process_clauses(T,N1).

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
	A==H.
	
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

clique([],[]):-!.

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

