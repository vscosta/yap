/*
	LPAD and CP-Logic reasoning suite
	File lpadclpbn.pl
	Goal oriented interpreter for LPADs based on SLDNF
	Copyright (c) 2008, Fabrizio Riguzzi
	Inference is performed translating the portion of the LPAD related to the goal
	into CLP(BN)
*/

:- set_prolog_flag(unknown,error).
:- set_prolog_flag(profiling,on).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).
:-source.
%:- module(lpadclpbn, [p/1,
%		  s/2,sc/3,s/6,sc/7,set/2,setting/2]).


:-dynamic rule/5,def_rule/2,setting/2.

:-use_module(library(lists)).
:-use_module(library(undgraphs)).
:-use_module(library(dgraphs)).
:-use_module(library(avl)).

:-use_module(library(matrix)).


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

%setting(order,top_sort). 
setting(order,min_def). 
%setting(order,max_card). 

/* end of list of parameters */

/* s(GoalsList,Prob) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with 
their corresponding probability */
s(GL,P):-
	get_ground_portion(GL,CL),!,
	convert_to_bn(CL,GL,[],P).

s(_GL,0.0).
/* sc(GoalsList,EvidenceList,Prob) compute the probability of a list of goals 
GoalsList given EvidenceList. Both lists can have variables, sc returns in 
backtracking all the solutions with their corresponding probability 
Time1 is the time for performing resolution
Time2 is the time for performing bayesian inference */

sc(GL,GL,1.0).

sc(GL,GLC,P):-
	get_ground_portion(GL,GLC,CL,Undef),!,
	(Undef=yes->
		P=undef
	;
		convert_to_bn(CL,GL,GLC,P)
	).

sc(_GL,_GLC,0.0).

get_ground_portion(GL,CL):-
	setof(Deriv,find_deriv(GL,Deriv),LDup),
	append_all(LDup,[],L),
	remove_head(L,LD),
	remove_duplicates(LD,LD1),
	build_ground_lpad(LD1,0,CL).

get_ground_portion(GL,GLC,CL,Undef):-
	setof(Deriv,find_deriv(GL,Deriv),LDup),
	(setof(Deriv,find_deriv(GLC,Deriv),LDupC)->
		append_all(LDup,[],L),
		remove_head(L,L1),
		append_all(LDupC,[],LC),
		remove_head(LC,LC1),
		append(L1,LC1,LD),
		remove_duplicates(LD,LD1),
		build_ground_lpad(LD1,0,CL),
		Undef=no
	;
		Undef=yes
	).


/* s(GoalsList,Prob,Time1,Time2) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with 
their corresponding probability 
Time1 is the time for performing resolution
Time2 is the time for performing bayesian inference */
s(GL,P,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(get_ground_portion(GL,CL)->
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		print_mem,
		convert_to_bn(CL,GL,[],P),
		statistics(cputime,[_,CT2]),
		CPUTime2 is CT2/1000,
		statistics(walltime,[_,WT2]),
		WallTime2 is WT2/1000

	;
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		print_mem,
		CPUTime2=0.0,
		WallTime2=0.0,
		P=0.0
	),
	format(user_error,"~nMemory after inference~n",[]),
	print_mem.

print_mem:-
	statistics(global_stack,[GS,GSF]),
	statistics(local_stack,[LS,LSF]),
	statistics(heap,[HP,HPF]),
	statistics(trail,[TU,TF]),
	format(user_error,"~nGloabal stack used ~d execution stack free: ~d~n",[GS,GSF]),
	format(user_error,"Local stack used ~d execution stack free: ~d~n",[LS,LSF]),
	format(user_error,"Heap used ~d heap free: ~d~n",[HP,HPF]),
	format(user_error,"Trail used ~d Trail free: ~d~n",[TU,TF]).


/* sc(GoalsList,EvidenceList,Prob) compute the probability of a list of goals 
GoalsList given EvidenceList. Both lists can have variables, sc returns in 
backtracking all the solutions with their corresponding probability */

sc(GL,GL,1.0,0.0,0.0,0.0,0.0).

sc(GL,GLC,P,CPUTime1,CPUTime2,WallTime1,WallTime2):-
	statistics(cputime,[_,_]),
	statistics(walltime,[_,_]),
	(get_ground_portion(GL,GLC,CL,Undef)->
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		print_mem,
		(Undef=yes->
			P=undef,
			CPUTime2=0.0,
			WallTime2=0.0
		;
			convert_to_bn(CL,GL,GLC,P),
			statistics(cputime,[_,CT2]),
			CPUTime2 is CT2/1000,
			statistics(walltime,[_,WT2]),
			WallTime2 is WT2/1000
		)
	;
		print_mem,
		statistics(cputime,[_,CT1]),
		CPUTime1 is CT1/1000,
		statistics(walltime,[_,WT1]),
		WallTime1 is WT1/1000,
		CPUTime2=0.0,
		WallTime2=0.0,
		P=0.0
	),
	format(user_error,"~nMemory after inference~n",[]),
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

convert_to_bn(CL,GL,GLC,P):-
	find_ground_atoms(CL,[],GADup),
	remove_duplicates(GADup,GANull),
	delete(GANull,'',GA),
	undgraph_new(Graph0),
	rule_factors(CL,[],HetF,HomFR,Graph0,Graph1),
	identity_facotrs(GA,_GAD,IF,Graph1,Graph2),
	setting(order,Order)->
	(Order=top_sort->
		dgraph_top_sort(Graph2,SortedAtoms)
	;
		dgraph_to_undgraph(Graph2,Graph3),
		undgraph_vertices(Graph3,SortedAtoms0),
		(Order=max_card->
			max_card_order(SortedAtoms0,[],SortedAtoms,Graph3)
		;
			SortedAtoms=SortedAtoms0
		)
	),
	find_atoms_body(GL,QAtoms),
	append(HomFR,IF,HomF),
	vel(HomF,HetF,QAtoms,GLC,Graph3,SortedAtoms,OutptutTable),
	get_prob_goal(GL,QAtoms,SortedAtoms,OutptutTable,P).


max_card_order([],SortedAtoms,SortedAtoms,_Graph):-!.

max_card_order(Atoms,SortedAtoms0,SortedAtoms1,Graph):-
	find_max_card(Atoms,SortedAtoms0,Graph,null,-1,At),
	delete(Atoms,At,Atoms1),
	max_card_order(Atoms1,[At|SortedAtoms0],SortedAtoms1,Graph).

find_max_card([],_SortedAtoms,_Graph,At,_MaxCard,At):-!.

find_max_card([HVar|T],SortedAtoms,Graph,MaxAt0,MaxCard0,MaxAt1):-
	(
		(HVar \= d(_At);HVar=ch(_N))
	;
		HVar = d(Var),
		member(Var,SortedAtoms)
	),!,
	find_card(SortedAtoms,Graph,HVar,0,Card),
	(Card>MaxCard0->
		MaxCard2=Card,
		MaxAt2=HVar
	;
		MaxCard2=MaxCard0,
		MaxAt2=MaxAt0
	),
	find_max_card(T,SortedAtoms,Graph,MaxAt2,MaxCard2,MaxAt1).

find_max_card([_HVar|T],SortedAtoms,Graph,MaxAt0,MaxCard0,MaxAt1):-
	find_max_card(T,SortedAtoms,Graph,MaxAt0,MaxCard0,MaxAt1).

find_card([],_Graph,_At,Card,Card):-!.

find_card([H|T],Graph,At,Card0,Card1):-
	(undgraph_edge(H,At,Graph)->
		Card2 is Card0+1
	;
		Card2 = Card0
	),
	find_card(T,Graph,At,Card2,Card1).


compute_min_def([],_Eliminated,Graph0,Graph1,MinVar,MinVar,_MinDef0):-
	undgraph_del_vertices(Graph0,[MinVar],Graph1).

compute_min_def([HVar|TVars],Eliminated,Graph0,Graph1,MinVar0,MinVar1,MinDef0):-
	(
		(HVar=d(_At);HVar=ch(_N))
	;
		member(d(HVar),Eliminated)
	),!,
	compute_def(HVar,Graph0,Def),
	(Def<MinDef0->
		MinDef2=Def,
		MinVar2=HVar
	;	
		MinDef2=MinDef0,
		MinVar2=MinVar0
	),
	compute_min_def(TVars,Eliminated,Graph0,Graph1,MinVar2,MinVar1,MinDef2).

compute_min_def([_HVar|TVars],Eliminated,Graph0,Graph1,MinVar0,MinVar1,MinDef0):-
	compute_min_def(TVars,Eliminated,Graph0,Graph1,MinVar0,MinVar1,MinDef0).	
	
	
compute_def(Node,UndGraph,Def):-
	undgraph_neighbors(Node,UndGraph,AdjNodes),
	undgraph_new(SecGraph0),
	section_graph([Node|AdjNodes],UndGraph,SecGraph0,SecGraph1),
	undgraph_complement(SecGraph1,SecGraphC),
	undgraph_edges(SecGraphC, Edges),
	length(Edges,Def).


section_graph([],_Graph,SG,SG):-!.

section_graph([H|T],Graph,SecGraph0,SecGraph1):-!,
	undgraph_neighbors(H,Graph,Neig),
	new_edges(Neig,H,Edges),
	undgraph_add_edges(SecGraph0,Edges,SecGraph2),
	section_graph(T,Graph,SecGraph2,SecGraph1).

new_edges([],_V,[]):-!.

new_edges([H|T],V,[V-H|TE]):-
	new_edges(T,V,TE).

get_prob_goal(GL,QAtoms,SortedAtoms,f(M,_D,_S),P):-
	positions(QAtoms,SortedAtoms,VarsPos),
	keysort(VarsPos,Vars1Pos),
	split_map(Vars1Pos,Vars1),
	get_index(Vars1,GL,Index),
	matrix_get(M,Index,P).

get_index([],_GL,[]):-!.

get_index([H|Vars1],GL,[1|Index]):-
	member(H,GL),!,
	get_index(Vars1,GL,Index).

get_index([H|Vars1],GL,[0|Index]):-
	member(\+H,GL),
	get_index(Vars1,GL,Index).


vel(IF,RF,QAtoms,GLC,Graph,SortedAtoms,OutptutTable):-
	fix_evidence(RF,RF1,GLC),
	fix_evidence(IF,IF1,GLC),
	sort_tables(RF1,RF2,SortedAtoms),
	sort_tables(IF1,IF2,SortedAtoms),
	find_atoms_body(GLC,AtomsC),
	delete_all(QAtoms,SortedAtoms,SortedAtoms1),
	delete_all(AtomsC,SortedAtoms1,SortedAtoms2),	
	vel_cycle(SortedAtoms2,IF2,RF2,Graph,SortedAtoms,[],_Eliminated,OutptutTable).


fix_evidence([],[],_Ev):-!.

fix_evidence([f(Tab,Dep,Sz)|T],[f(Tab1,Dep1,Sz1)|T1],Ev):-
	simplify_evidence(Ev,Tab,Dep,Sz,Tab1,Dep1,Sz1),
	fix_evidence(T,T1,Ev).


simplify_evidence([], Table, Deps, Sizes, Table, Deps, Sizes).
simplify_evidence([V|VDeps], Table0, Deps0, Sizes0, Table, Deps, Sizes) :-!,
	project_from_CPT(V,tab(Table0,Deps0,Sizes0),tab(Table1,Deps1,Sizes1)),
	simplify_evidence(VDeps, Table1, Deps1, Sizes1, Table, Deps, Sizes).




project_from_CPT(\+H,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	nth0(N,Deps, H),!,
	matrix_select(Table, N, 0, NewTable),
	matrix_dims(NewTable, NSzs),
	delete(Deps,H,NDeps).
project_from_CPT(H,tab(Table,Deps,_),tab(NewTable,NDeps,NSzs)) :-
	nth0(N,Deps, H),!,
	matrix_select(Table, N, 1, NewTable),
	matrix_dims(NewTable, NSzs),
	delete(Deps,H,NDeps).
project_from_CPT(_H,tab(Table,Deps,S),tab(Table,Deps,S)).



sort_tables([],[],_SortedAtoms):-!.

sort_tables([f(Mat,Vars,_Sz)|T],[f(Mat1,Vars1,Sz1)|T1],SortedAtoms):-
	reorder_CPT(Vars,SortedAtoms,Vars1,Map),
	matrix_shuffle(Mat,Map,Mat1),
	matrix_dims(Mat1,Sz1),
	sort_tables(T,T1,SortedAtoms).


delete_all([],L,L):-!.

delete_all([H|T],L0,L1):-
	delete(L0,H,L2),
	delete_all(T,L2,L1).

mapping(Vs0,Vs,Map) :-
	add_indices(Vs0,0,I1s),
	add_indices( Vs,I2s),
	keysort(I1s,Ks),
	keysort(I2s,Ks),
	split_map(I2s, Map).

add_indices([],[]).
add_indices([V|Vs0],[V-_|I1s]) :-
	add_indices(Vs0,I1s).

split_map([], []).
split_map([_-M|Is], [M|Map]) :-
	split_map(Is, Map).

split_pos([], []).
split_pos([V-_|Is], [V|Map]) :-
	split_pos(Is, Map).

positions([],_SA,[]):-!.

positions([HV|Vars],SortedAtoms,[Pos-HV|VarsPos]):-
	nth(Pos,SortedAtoms,HV),!,
	positions(Vars,SortedAtoms,VarsPos).


reorder_CPT(Vars,SortedAtoms,Vars1,Map):-
	positions(Vars,SortedAtoms,VarsPos),
	keysort(VarsPos,Vars1Pos),
	split_map(Vars1Pos,Vars1),
	mapping(Vars,Vars1,Map).

add_indices([],_,[]).
add_indices([V|Vs0],I0,[V-I0|Is]) :-
	I is I0+1,
	add_indices(Vs0,I,Is).

vel_cycle([],HomFact,HetFact,_Graph,SortedAtoms,Eliminated,Eliminated,f(Mat1,Dep,Sz)):-!,
	combine_factors(HomFact,HetFact,SortedAtoms,f(Mat,Dep,Sz)),
	normalise_CPT(Mat,Mat1).

vel_cycle(Vars0,HomFact,HetFact,Graph0,SortedAtoms,Eliminated0,Eliminated1,OutputTable):-
	(setting(order,min_def)->
		compute_min_def(Vars0,Eliminated0,Graph0,Graph1,null,MinVar,+inf),
		delete(Vars0,MinVar,Vars1)
	;
		Vars0=[MinVar|Vars1]
	),
	sum_out1(MinVar,HomFact,HetFact,HomFact1,HetFact1,SortedAtoms),
	append(Eliminated0,[MinVar],Eliminated2),
	vel_cycle(Vars1,HomFact1,HetFact1,Graph1,SortedAtoms,Eliminated2,Eliminated1,OutputTable).

normalise_CPT(MAT,NMAT) :-
	matrix_sum(MAT, Sum),
	matrix_op_to_all(MAT,/,Sum,NMAT).

combine_factors(HomFacts,HetFacts,SortedAtoms,Fact):-
	combine_tables(HetFacts,HetFact,SortedAtoms),
	multiply_tables([HetFact|HomFacts],Fact,SortedAtoms).


sum_out1(Var,Hom,Het,Hom2,Het2,SortedAtoms):-
	get_factors_with_var(Hom,Var,HomFacts,Hom1),
	multiply_tables(HomFacts,HomFact,SortedAtoms),
	get_factors_with_var(Het,Var,HetFacts,Het1),
	combine_tables(HetFacts,HetFact,SortedAtoms),
	update_factors(Var,HomFact,HetFact,Hom1,Hom2,Het1,Het2,SortedAtoms).

update_factors(_Var,[],[],Hom,Hom,Het,Het,_SortedAtoms):-!.

update_factors(Var,HomFact,[],Hom,[Fact|Hom],Het,Het,_SortedAtoms):-!,
	sum_var(Var,HomFact,Fact).

update_factors(Var,[],HetFact,Hom,Hom,Het,[Fact|Het],_SortedAtoms):-
	sum_var(Var,HetFact,Fact).

update_factors(Var,HomFact,HetFact,Hom,Hom,Het,[Fact1|Het],SortedAtoms):-
	multiply_CPTs(HomFact,HetFact,Fact,SortedAtoms),
	sum_var(Var,Fact,Fact1).

sum_var(Var,f(Table,Deps,_),f(NewTable,NDeps,NSzs)):-
	nth0(N,Deps, Var),!,
	delete(Deps,Var,NDeps),
	matrix_sum_out(Table, N, NewTable),
	matrix_dims(NewTable, NSzs).

combine_tables([],[],_SortedAtoms):-!.

combine_tables([Fact],Fact,_SortedAtoms):-!.

combine_tables([Fact1,Fact2|T],Fact,SortedAtoms):-
	combine_CPTs(Fact1,Fact2,Fact0,SortedAtoms),
	combine_tables([Fact0|T],Fact,SortedAtoms).
	
get_factors_with_var([],_V,[],[]):-!.

get_factors_with_var([f(Table,Vars,Sz)|T],Var,[f(Table,Vars,Sz)|TFV],TRest):-
	member(Var,Vars),!,
	get_factors_with_var(T,Var,TFV,TRest).
	
get_factors_with_var([f(Table,Vars,Sz)|T],Var,TFV,[f(Table,Vars,Sz)|TRest]):-
	get_factors_with_var(T,Var,TFV,TRest).

multiply_tables([], [],_SorteAtoms) :- !.

multiply_tables([Table], Table,_SorteAtoms) :- !.
multiply_tables([TAB1, TAB2| Tables], Out,SorteAtoms) :-
	multiply_CPTs(TAB1, TAB2, TAB,SorteAtoms),
	multiply_tables([TAB| Tables], Out,SorteAtoms).

combine_CPTs(f(Tab1, Deps1, Sz1), f(Tab2, Deps2, Sz2), F, SortedAtoms) :-
	get_common_conv(Deps1,Deps2,CommConv),
	rename_convergent(1,CommConv,Deps1,Deps11,[],NewAt0),
	rename_convergent(2,CommConv,Deps2,Deps21,NewAt0,NewAt1),
	update_sorted(SortedAtoms,NewAt1,SortedAtoms1),
	expand_tabs(Deps11, Sz1, Deps21, Sz2, Map1, Map2, NDeps0,SortedAtoms1),
	matrix_expand(Tab1, Map1, NTab1),
	matrix_expand(Tab2, Map2, NTab2),
	matrix_op(NTab1,NTab2,*,OT0),
	matrix_dims(OT0,NSz0),
	sum_fact(CommConv,OT0,NDeps0,NSz0,OT,NDeps,NSz),
	sort_tables([f(OT, NDeps, NSz)],[F],SortedAtoms).

get_common_conv(Deps1,Deps2,CommConv):-
	get_conv(Deps1,C1),
	get_conv(Deps2,C2),
	intersection(C1,C2,CommConv).

get_conv([],[]):-!.

get_conv([d(H)|T],[H|T1]):-!,
	get_conv(T,T1).

get_conv([_H|T],T1):-!,
	get_conv(T,T1).

        
sum_fact([],T,D,S,T,D,S):-!.

%	remove_renamed_conv(D0,D1).

sum_fact([H|T],T0,D0,S0,T1,D1,S1):-
	simplify_evidence([\+d(H,1),\+d(H,2)],T0,D0,S0,Tff,D,S),
	simplify_evidence([\+d(H,1),d(H,2)],T0,D0,S0,Tft,D,S),
	simplify_evidence([d(H,1),\+d(H,2)],T0,D0,S0,Ttf,D,S),
	simplify_evidence([d(H,1),d(H,2)],T0,D0,S0,Ttt,D,S),
	matrix_op(Tft,Ttf,+,T2),
	matrix_op(T2,Ttt,+,T3),
	matrix_to_list(T3,Lt),
	matrix_to_list(Tff,Lf),
	append(Lf,Lt,L),
	matrix_new(floats, [2|S], L,T4),
	sum_fact(T,T4,[d(H)|D],[2|S],T1,D1,S1).

remove_renamed_conv([],[]):-!.

remove_renamed_conv([d(H,_N)|D0],[d(H)|D1]):-!,
	remove_renamed_conv(D0,D1).

remove_renamed_conv([H|D0],[H|D1]):-
	remove_renamed_conv(D0,D1).



update_sorted([],_NewAt,[]):-!.

update_sorted([d(H)|T],NewAt,[d(H,1)|T1]):-
	member(d(H,1),NewAt),!,
	update_sorted1(H,T,NewAt,T1).

update_sorted([d(H)|T],NewAt,[d(H,2)|T1]):-
	member(d(H,2),NewAt),!,
	update_sorted(T,NewAt,T1).

update_sorted([H|T],NewAt,[H|T1]):-
	update_sorted(T,NewAt,T1).

update_sorted1(H,T,NewAt,[d(H,2)|T1]):-
	member(d(H,2),NewAt),!,
	update_sorted(T,NewAt,T1).

update_sorted1(_H,T,NewAt,T1):-
	update_sorted(T,NewAt,T1).

rename_convergent(_N,_CommConv,[],[],NA,NA):-!.

rename_convergent(N,CommConv,[d(H)|T],[d(H,N)|T1],NA0,[d(H,N)|NA1]):-
	member(H,CommConv),!,
	rename_convergent(N,CommConv,T,T1,NA0,NA1).

rename_convergent(N,CommConv,[H|T],[H|T1],NA0,NA1):-
	rename_convergent(N,CommConv,T,T1,NA0,NA1).


multiply_CPTs(f(Tab1, Deps1, Sz1), f(Tab2, Deps2, Sz2), f(OT, NDeps, NSz), SortedAtoms) :-
	expand_tabs(Deps1, Sz1, Deps2, Sz2, Map1, Map2, NDeps,SortedAtoms),
	matrix_expand(Tab1, Map1, NTab1),
	matrix_expand(Tab2, Map2, NTab2),
	matrix_op(NTab1,NTab2,*,OT),
	matrix_dims(OT,NSz).

expand_tabs([], [], [], [], [], [], [],_SortedAtoms):-!.
expand_tabs([V1|Deps1], [S1|Sz1], [], [], [0|Map1], [S1|Map2], [V1|NDeps],SortedAtoms) :-!,
	expand_tabs(Deps1, Sz1, [], [], Map1, Map2, NDeps,SortedAtoms).
expand_tabs([], [], [V2|Deps2], [S2|Sz2], [S2|Map1], [0|Map2], [V2|NDeps],SortedAtoms) :-!,
	expand_tabs([], [], Deps2, Sz2, Map1, Map2, NDeps,SortedAtoms).
expand_tabs([V1|Deps1], [S1|Sz1], [V2|Deps2], [S2|Sz2], Map1, Map2, NDeps,SortedAtoms) :-
	compare_var(C,V1,V2,SortedAtoms),
	(C == = ->
	 NDeps = [V1|MDeps],
	 Map1 = [0|M1],
	 Map2 = [0|M2],
	 NDeps = [V1|MDeps],
	 expand_tabs(Deps1, Sz1, Deps2, Sz2, M1, M2, MDeps,SortedAtoms)
	;
	 C == < ->
	 NDeps = [V1|MDeps],
	 Map1 = [0|M1],
	 Map2 = [S1|M2],
	 NDeps = [V1|MDeps],
	 expand_tabs(Deps1, Sz1, [V2|Deps2], [S2|Sz2], M1, M2, MDeps,SortedAtoms)
	;
	 NDeps = [V2|MDeps],
	 Map1 = [S2|M1],
	 Map2 = [0|M2],
	 NDeps = [V2|MDeps],
	 expand_tabs([V1|Deps1], [S1|Sz1], Deps2, Sz2, M1, M2, MDeps,SortedAtoms)
	).
	
compare_var(C,V1,V2,SortedAtoms):-
	nth(N1,SortedAtoms,V1),
	nth(N2,SortedAtoms,V2),!,
	compare(C,N1,N2).

deputy_atoms([],[]):-!.

deputy_atoms([H|T],[d(H)|T1]):-
	deputy_atoms(T,T1).

identity_facotrs([],[],[],Graph,Graph):-!.

identity_facotrs([H|T],[d(H)|TD],[f(Mat,[d(H),H],[2,2])|TF],Graph0,Graph1):-
	dgraph_add_edges(Graph0,[d(H)-H],Graph2),
	matrix_new(floats, [2,2], [1.0,0.0,0.0,1.0],Mat),
	identity_facotrs(T,TD,TF,Graph2,Graph1).


find_rules_with_atom(_A,[],[]).

find_rules_with_atom(A,[(N,Head,_Body)|T],[(N,Head)|R]):-
	member(A:_P,Head),!,
	find_rules_with_atom(A,T,R).

find_rules_with_atom(A,[_H|T],R):-
	find_rules_with_atom(A,T,R).

rule_factors([],HetF,HetF,[],Graph,Graph):-!.

rule_factors([(N,Head,Body)|T],HetF0,HetF1,[f(Mat,Deps,Sizes)|HomF],Graph0,Graph1):-
	find_atoms_head(Head,Atoms,Probs),
	length(Body,LB),
	list2(0,LB,Sizes0),
	length(Head,LH),
	LH1 is LH-1,
	list0(0,LH1,FalseCol0),
	append(FalseCol0,[1.0],FalseCol),
	build_table(Probs,FalseCol,Body,Table),
	append(Sizes0,[LH],Sizes),
	matrix_new(floats,Sizes,Table,Mat),
	find_atoms_body(Body,BodyAtoms),
	append(BodyAtoms,[ch(N)],Deps),
	gen_het_factors(Atoms,N,LH,0,HetF0,HetF2),
	add_hom_edges_to_graph(BodyAtoms,N,Graph0,Graph2),
	add_het_edges_to_graph(Atoms,N,Graph2,Graph3),
	rule_factors(T,HetF2,HetF1,HomF,Graph3,Graph1).


build_table(Probs,FalseCol,Body,T):-!,
	build_col(Body,t,Probs,FalseCol,[],T).

build_col([],t,Probs,_FalseCol,T0,T1):-!,
	append(T0,Probs,T1).

build_col([],f,_Probs,FalseCol,T0,T1):-!,
	append(T0,FalseCol,T1).

build_col([\+ _H|T],Truth,Probs,FalseCol,T0,T1):-!,
	build_col(T,Truth,Probs,FalseCol,T0,T2),
	build_col(T,f,Probs,FalseCol,T2,T1).

build_col([_H|T],Truth,Probs,FalseCol,T0,T1):-
	build_col(T,f,Probs,FalseCol,T0,T2),
	build_col(T,Truth,Probs,FalseCol,T2,T1).

add_hom_edges_to_graph([],_N,Graph,Graph):-!.

add_hom_edges_to_graph([H|T],N,Graph0,Graph1):-
	dgraph_add_edges(Graph0,[H-ch(N)],Graph2),
	add_hom_edges_to_graph(T,N,Graph2,Graph1).

add_het_edges_to_graph([''],_N,Graph,Graph):-!.

add_het_edges_to_graph([H|T],N,Graph0,Graph1):-
	dgraph_add_edges(Graph0,[ch(N)-d(H)],Graph2),
	add_het_edges_to_graph(T,N,Graph2,Graph1).

add_edges_to_graph([],_Atoms,Graph,Graph):-!.

add_edges_to_graph([H|T],Atoms,Graph0,Graph1):-
	add_edges_from_atom(Atoms,H,Graph0,Graph2),
	add_edges_to_graph(T,Atoms,Graph2,Graph1).

add_edges_from_atom([''],_At,Graph,Graph):-!.

add_edges_from_atom([H|T],At,Graph0,Graph1):-
	dgraph_add_edges(Graph0,[At-d(H)],Graph2),
	add_edges_from_atom(T,At,Graph2,Graph1).

gen_het_factors([''],_N,_LH,_Pos,HetF,HetF):-!.

gen_het_factors([H|Atoms],N,LH,Pos,HetF0,[f(Mat,[ch(N),d(H)],[LH,2])|HetF1]):-
	gen_het_table(0,LH,Pos,Table),
	matrix_new(floats, [LH,2], Table, Mat),
	Pos1 is Pos+1,
	gen_het_factors(Atoms,N,LH,Pos1,HetF0,HetF1).

gen_het_table(N,N,_Pos,[]):-!.

gen_het_table(N0,N,N0,[0.0,1.0|T]):-!,
	N1 is N0+1,
	gen_het_table(N1,N,N0,T).

gen_het_table(N0,N,Pos,[1.0,0.0|T]):-
	N1 is N0+1,
	gen_het_table(N1,N,Pos,T).

	
	
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

list2(N,N,[]):-!.

list2(NIn,N,[2|T]):-
	N1 is NIn+1,
	list2(N1,N,T).

list0(N,N,[]):-!.

list0(NIn,N,[0.0|T]):-
	N1 is NIn+1,
	list0(N1,N,T).

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
