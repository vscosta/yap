/*
	LPAD and CP-Logic reasoning suite
	File lpadclpbn.pl
	Goal oriented interpreter for LPADs based on SLDNF
	Copyright (c) 2008, Fabrizio Riguzzi
	Inference is performed translating the portion of the LPAD related to the goal
	into CLP(BN)
*/

:- module(lpadclpbn, [p/1,
		  s/2,sc/3,s/6,sc/7,set/2,setting/2]).


:-dynamic rule/4,def_rule/2,setting/2.

:-use_module(library(lists)).
:-use_module(library(ugraphs)).
:-use_module(library(avl)).
:-use_module(library(clpbn)).

:-set_clpbn_flag(suppress_attribute_display,true).

:-set_clpbn_flag(bnt_model,propositional).

/* start of list of parameters that can be set by the user with
set(Parameter,Value) */
setting(epsilon_parsing,0.00001).
setting(save_dot,false).
setting(ground_body,true). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */

setting(cpt_zero,0.0001). 

/* end of list of parameters */

/* s(GoalsList,Prob) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with 
their corresponding probability */
s(GL,P):-
	setof(Deriv,find_deriv(GL,Deriv),LDup),!,
	append_all(LDup,[],L),
	remove_head(L,L1),
	remove_duplicates(L1,L2),
	build_ground_lpad(L2,0,CL),
	convert_to_clpbn(CL,GL,LV,P).

s(_GL,0.0).
/* sc(GoalsList,EvidenceList,Prob) compute the probability of a list of goals 
GoalsList given EvidenceList. Both lists can have variables, sc returns in 
backtracking all the solutions with their corresponding probability 
Time1 is the time for performing resolution
Time2 is the time for performing bayesian inference */
sc(GL,GLC,P):-
	(setof(Deriv,find_deriv(GLC,Deriv),LDupC)->	
		(setof(Deriv,find_deriv(GL,Deriv),LDup)->
			append_all(LDup,[],L),
			remove_head(L,L1),
			append_all(LDupC,[],LC),
			remove_head(LC,LC1),
			append(L1,LC1,LD),
			remove_duplicates(LD,LD1),
			build_ground_lpad(LD1,0,CL),
			convert_to_clpbn(CL,GL,LV,P,GLC)
		;
			P=0.0
		)
	;
		P=undef
	).



/* s(GoalsList,Prob,Time1,Time2) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with 
their corresponding probability 
Time1 is the time for performing resolution
Time2 is the time for performing bayesian inference */
s(GL,P,Time1,Time2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(setof(Deriv,find_deriv(GL,Deriv),LDup)->
		append_all(LDup,[],L),
		remove_head(L,L1),
		remove_duplicates(L1,L2),
		build_ground_lpad(L2,0,CL),
		convert_to_clpbn(CL,GL,LV,P),
		statistics(cputime,[_,CT2]),
		Time1 is CT2/1000,
		statistics(walltime,[_,WT2]),
		Time2 is WT2/1000
	;
		P=0.0,
		statistics(cputime,[_,CT1]),
		Time1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		Time2 is WT1/1000
	).
	
s(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	solve(GoalsList,Prob,CPUTime1,CPUTime2,WallTime1,WallTime2).


solve(GL,P,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(setof(Deriv,find_deriv(GL,Deriv),LDup)->
		append_all(LDup,[],L),
		remove_head(L,L1),
		remove_duplicates(L1,L2),
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		print_mem,
		build_ground_lpad(L2,0,CL),
		convert_to_clpbn(CL,GL,LV,P),
		statistics(cputime,[_,CT2]),
		CPUTime2 is CT2/1000,
		statistics(walltime,[_,WT2]),
		WallTime2 is WT2/1000
	;
		print_mem,
		P=0.0,
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		CPUTime2 =0.0,
		statistics(walltime,[_,WT2]),
		WallTime2 =0.0
	),!,
	format(user_error,"Memory after inference~n",[]),
	print_mem.	

/* sc(GoalsList,EvidenceList,Prob) compute the probability of a list of goals 
GoalsList given EvidenceList. Both lists can have variables, sc returns in 
backtracking all the solutions with their corresponding probability */

sc(GL,GLC,P,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(setof(Deriv,find_deriv(GLC,Deriv),LDupC)->
		(setof(Deriv,find_deriv(GL,Deriv),LDup)->
			append_all(LDup,[],L),
			remove_head(L,L1),
			append_all(LDupC,[],LC),
			remove_head(LC,LC1),
			append(L1,LC1,LD),
			remove_duplicates(LD,LD1),
			statistics(cputime,[_,CT1]),
			CPUTime1 is CT1/1000,
			statistics(walltime,[_,WT1]),
			WallTime1 is WT1/1000,
			print_mem,
			build_ground_lpad(LD1,0,CL),
			convert_to_clpbn(CL,GL,LV,P,GLC),
			statistics(cputime,[_,CT2]),
			CPUTime2 is CT2/1000,
			statistics(walltime,[_,WT2]),
			WallTime2 is WT2/1000
		;
			P=0.0,
			print_mem,
			format(user_error,"Porb 0~n",[]),
			statistics(cputime,[_,CT1]),
			CPUTime1 is CT1/1000,
			statistics(walltime,[_,WT1]),
			WallTime1 is WT1/1000,
			CPUTime2 =0.0,
			WallTime2 =0.0
		)
	;
		P=undef,
		print_mem,
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		CPUTime2 =0.0,
		WallTime2 =0.0,
		print_mem,
		format(user_error,"Undef~n",[])
	),
	format(user_error,"Memory after inference~n",[]),
	print_mem.

remove_head([],[]).

remove_head([(_N,R,S)|T],[(R,S)|T1]):-
	remove_head(T,T1).

append_all([],L,L):-!.

append_all([LIntH|IntT],IntIn,IntOut):-
    append(IntIn,LIntH,Int1),
    append_all(IntT,Int1,IntOut).

process_goals([],[],[]).

process_goals([H|T],[HG|TG],[HV|TV]):-
	H=..[F,HV|Rest],
	HG=..[F|Rest],
	process_goals(T,TG,TV).

build_ground_lpad([],_N,[]).

build_ground_lpad([(R,S)|T],N,[(N1,Head1,Body1)|T1]):-
	rule(R,S,_,Head,Body),
	N1 is N+1,
	merge_identical(Head,Head1),
	remove_built_ins(Body,Body1),
	build_ground_lpad(T,N1,T1).

remove_built_ins([],[]):-!.

remove_built_ins([\+H|T],T1):-
	builtin(H),!,
	remove_built_ins(T,T1).

remove_built_ins([H|T],T1):-
	builtin(H),!,
	remove_built_ins(T,T1).

remove_built_ins([H|T],[H|T1]):-
	remove_built_ins(T,T1).

merge_identical([],[]):-!.

merge_identical([A:P|T],[A:P1|Head]):-
	find_identical(A,P,T,P1,T1),
	merge_identical(T1,Head).

find_identical(_A,P,[],P,[]):-!.

find_identical(A,P0,[A:P|T],P1,T1):-!,
	P2 is P0+P,
	find_identical(A,P2,T,P1,T1).

find_identical(A,P0,[H:P|T],P1,[H:P|T1]):-
	find_identical(A,P0,T,P1,T1).

convert_to_clpbn(CL,GL,LV,P,GLC):-
	find_ground_atoms(CL,[],GAD),
	remove_duplicates(GAD,GANull),
	delete(GANull,'',GA),
	atom_vars(GA,[],AV,AVL),
	choice_vars(CL,[],CV,CVL),
	add_rule_tables(CL,CVL,AV),
	add_atoms_tables(AVL,GA,CL,CV,AV),
	add_ev(GLC,AV),
	get_prob(GL,AV,AVL,CVL,P).

add_ev([],_AV).

add_ev([\+ H|T],AV):-!,
	avl_lookup(H,V,AV),
	clpbn:put_atts(V,evidence(0)),
	add_ev(T,AV).

add_ev([H|T],AV):-
	avl_lookup(H,V,AV),
	clpbn:put_atts(V,evidence(1)),
	add_ev(T,AV).

convert_to_clpbn(CL,GL,LV,P):-
	find_ground_atoms(CL,[],GAD),
	remove_duplicates(GAD,GANull),
	delete(GANull,'',GA),
	atom_vars(GA,[],AV,AVL),
	choice_vars(CL,[],CV,CVL),
	add_rule_tables(CL,CVL,AV),
	add_atoms_tables(AVL,GA,CL,CV,AV),
	get_prob(GL,AV,AVL,CVL,P).
	
get_prob(GL,AV,AVL,CVL,P):-	
	build_table_conj(GL,Table),
	find_atoms_body(GL,Atoms),
	lookup_gvars(GL,AV,Parents,[],_Signs),
	{G=goal with p([f,t],Table, Parents)},
	append(AVL,CVL,Vars),
	append(Vars,[G],Vars1),
	clpbn:clpbn_run_solver([G], Vars1,_State),
	clpbn_display:get_atts(G, [posterior(Vs,Vals,[_P0,P],AllDiffs)]).
	
lookup_gvars([],_AV,[],S,S).	

lookup_gvars([\+ H|T],AV,[HV|T1],Sign0,Sign2):-	!,
	avl_lookup(H,HV,AV),
	clpbn:get_atts(HV, [key(K)]), 
	avl_insert(K,f,Sign0,Sign1),
	lookup_gvars(T,AV,T1,Sign1,Sign2).
	
lookup_gvars([H|T],AV,[HV|T1],Sign0,Sign2):-	
	avl_lookup(H,HV,AV),
	clpbn:get_atts(HV, [key(K)]), 
	avl_insert(K,t,Sign0,Sign1),
	lookup_gvars(T,AV,T1,Sign1,Sign2).
	
add_atoms_tables([],[],_CL,_CV,_AV).

add_atoms_tables([H|T],[HA|TA],CL,CV,AV):-
	find_rules_with_atom(HA,CL,R),
	parents(R,CV,Par),
	build_table_atoms(HA,R,Table),
	{H = HA with p([f,t],Table,Par)},
	add_atoms_tables(T,TA,CL,CV,AV).

build_table_conj(R,Table):-
	build_col_conj(R,t,f,[],Row1),
	build_col_conj(R,t,t,Row1,Table).

build_col_conj([],Tr,Final,Row0,Row1):-
	(Tr=Final->
		append(Row0,[1.0],Row1)
	;
		setting(cpt_zero,Zero),
		append(Row0,[Zero],Row1)
	).

build_col_conj([\+H|RP],Tr,Final,Row0,Row2):-!,
	build_col_conj(RP,Tr,Final,Row0,Row1),
	build_col_conj(RP,f,Final,Row1,Row2).

build_col_conj([H|RP],Tr,Final,Row0,Row2):-
	build_col_conj(RP,f,Final,Row0,Row1),
	build_col_conj(RP,Tr,Final,Row1,Row2).

build_table_atoms(H,R,Table):-
	build_col(H,R,f,f,[],Row1),
	build_col(H,R,t,f,Row1,Table).

build_col(A,[],Tr,Found,Row0,Row1):-
	(Tr=Found->
		append(Row0,[1.0],Row1)
	;
		setting(cpt_zero,Zero),
		append(Row0,[Zero],Row1)
	).

build_col(A,[(N,H)|RP],Tr,Found,Row0,Row1):-
	build_col_cycle(A,H,RP,Tr,Found,Row0,Row1).

build_col_cycle(_A,[],_RP,_Tr,_Found,Row,Row).
	
build_col_cycle(A,[A:P|T],RP,Tr,Found,Row0,Row2):-!,
	build_col(A,RP,Tr,t,Row0,Row1),
	build_col_cycle(A,T,RP,Tr,Found,Row1,Row2).
	
build_col_cycle(A,[_|T],RP,Tr,Found,Row0,Row2):-
	build_col(A,RP,Tr,Found,Row0,Row1),
	build_col_cycle(A,T,RP,Tr,Found,Row1,Row2).

parents([],_CV,[]).

parents([(N,_H)|T],CV,[V|T1]):-
	avl_lookup(N,V,CV),
	parents(T,CV,T1).

find_rules_with_atom(_A,[],[]).

find_rules_with_atom(A,[(N,Head,Body)|T],[(N,Head)|R]):-
	member(A:P,Head),!,
	find_rules_with_atom(A,T,R).

find_rules_with_atom(A,[_H|T],R):-
	find_rules_with_atom(A,T,R).

add_rule_tables([],[],_AV).

add_rule_tables([(N,Head,Body)|T],[CV|TCV],AV):-
	find_atoms_head(Head,Atoms,Probs),
	get_parents(Body,AV,Par),
	build_table(Probs,Body,Table),
	{CV=ch(N) with p(Atoms,Table,Par)},
	add_rule_tables(T,TCV,AV).

build_table([P],L,Row):-!,
	build_col(L,t,P,1.0,Row).

build_table([HP|TP],L,Tab):-
	setting(cpt_zero,Zero),
	build_col(L,t,HP,Zero,Row),
	append(Row,Row1,Tab),
	build_table(TP,L,Row1).

build_col([],t,HP,_PNull,[HP]):-!.

build_col([],f,_HP,PNull,[PNull]):-!.


build_col([\+ H|T],Truth,P,PNull,Row):-!,
	build_col(T,Truth,P,PNull,Row1),
	append(Row1,Row2,Row),
	build_col(T,f,P,PNull,Row2).

build_col([_H|T],Truth,P,PNull,Row):-
	build_col(T,f,P,PNull,Row1),
	append(Row1,Row2,Row),
	build_col(T,Truth,P,PNull,Row2).	
	
get_parents([],_AV,[]).
		
get_parents([\+ H|T],AV,[V|T1]):-!,
	avl_lookup(H,V,AV),
	get_parents(T,AV,T1).

get_parents([H|T],AV,[V|T1]):-!,
	avl_lookup(H,V,AV),
	get_parents(T,AV,T1).

choice_vars([],Tr,Tr,[]).

choice_vars([(N,_H,_B)|T],Tr0,Tr1,[NV|T1]):-
	avl_insert(N,NV,Tr0,Tr2),
	choice_vars(T,Tr2,Tr1,T1).

atom_vars([],Tr,Tr,[]).

atom_vars([H|T],Tr0,Tr1,[VH|VT]):-
	avl_insert(H,VH,Tr0,Tr2),
	atom_vars(T,Tr2,Tr1,VT).

find_ground_atoms([],GA,GA).

find_ground_atoms([(_N,Head,Body)|T],GA0,GA1):-
	find_atoms_head(Head,AtH,_P),
	append(GA0,AtH,GA2),
	find_atoms_body(Body,AtB),
	append(GA2,AtB,GA3),
	find_ground_atoms(T,GA3,GA1).

find_atoms_body([],[]).

find_atoms_body([\+H|T],[H|T1]):-!,
	find_atoms_body(T,T1).

find_atoms_body([H|T],[H|T1]):-
	find_atoms_body(T,T1).


find_atoms_head([],[],[]).

find_atoms_head([H:P|T],[H|TA],[P|TP]):-
	find_atoms_head(T,TA,TP).

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
/* duplicate can appear in the C set because two different unistantiated clauses may become the 
same clause when instantiated */



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

solve_pres(R,S,N,B,T,CIn,COut):-
	member_eq((N,R,S),CIn),!,
	append(B,T,NG),
	solve(NG,CIn,COut).
	
solve_pres(R,S,N,B,T,CIn,COut):-
	append(CIn,[(N,R,S)],C1),
	append(B,T,NG),
	solve(NG,C1,COut).

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
	rule(R,S,_,Head,Body),
	member_head(H,Head,0,N),
	not_already_present_with_a_different_head(N,R,S,C).

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
	rule(R,S,Numbers,Head,_Body),
	Head\=uniform(_,_,_),!,
	nth0(N, Numbers, _Elem, Rest),
	member(N1,Rest).

new_head(N,R,S,N1):-
	rule(R,S,Numbers,uniform(_A:1/Tot,_L,_Number),_Body),
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
	retractall(rule(_,_,_,_,_)),
	retractall(def_rule(_,_)),
	process_clauses(C,1).

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
	process_head([H:1.0],HL),
	list2and(BL,B),
	process_body(BL,V,V1),
	length(HL,LH),
	listN(0,LH,NH),
	assertz(rule(N,V1,NH,HL,BL)),
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
	process_head([H:1.0],HL),
	length(HL,LH),
	listN(0,LH,NH),
	assertz(rule(N,V,NH,HL,[])),
	N1 is N+1,
	process_clauses(T,N1).

/* if the annotation in the head are not ground, the null atom is not added
and the eventual formulas are not evaluated */
	
process_head(HL,NHL):-
	(ground_prob(HL)->
		process_head_ground(HL,0.0,NHL)
	;
		NHL=HL
	).

ground_prob([]).

ground_prob([_H:PH|T]):-
	ground(PH),
	ground_prob(T).

process_head_ground([H:PH],P,[H:PH1,'':PNull1]):-!,
	PH1 is PH,
	PNull is 1.0-P-PH1,
	(PNull>=0.0->
		PNull1 =PNull
	;
		PNull1=0.0
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
