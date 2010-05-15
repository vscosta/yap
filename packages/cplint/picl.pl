/*
	LPAD and CP-Logic reasoning suite
	File lpadsld.pl
	Goal oriented interpreter for LPADs based on SLDNF
	Copyright (c) 2007, Fabrizio Riguzzi
*/

:-dynamic rule/4,def_rule/2,setting/2.
%:-yap_flag(gc_trace,very_verbose).
%:-source.
:-use_module(library(lists)).
:-use_module(library(ugraphs)).

:-load_foreign_files(['cplint'],[],init_my_predicates).

:- op(1150, xfx, <- ).
:- op(950,xfy, &).
:- op(900,fy,~).
:- op(1170,fx,prob).

/* start of list of parameters that can be set by the user with
set(Parameter,Value) */
setting(epsilon_parsing,0.00001).
setting(save_dot,false).
setting(ground_body,false). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */
setting(min_error,0.01).
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
	findall(Deriv,find_deriv(GoalsList,Deriv),L),
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
	findall(Deriv,find_deriv(GoalsList,Deriv),L)->
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
	WallTime2 is WT2/1000,
	format(user_error,"~nMemory after inference~n",[]).
	%print_mem.

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
	solve(GoalsList,[],Deriv).
%	remove_duplicates(DerivDup,Deriv). 


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
	findall(DerivE,find_deriv(Evidence,DerivE),LE),
	findall(DerivGE,find_deriv_GE(LE,Goals,DerivGE),LGE),
	%print_mem,
	build_formula(LE,FormulaE,[],VarE),
	var2numbers(VarE,0,NewVarE),
	build_formula(LGE,FormulaGE,[],VarGE),
	var2numbers(VarGE,0,NewVarGE),
	compute_prob(NewVarE,FormulaE,ProbE,0),
	call_compute_prob(NewVarGE,FormulaGE,ProbGE),
	(ProbE>0.0->
		Prob is ProbGE/ProbE
	;
		%print_mem,
		Prob=undefined
	),
	format(user_error,"~nMemory after inference~n",[]).
	%print_mem.

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
	findall(DerivE,find_deriv(Evidence,DerivE),LE),
	findall(DerivGE,find_deriv_GE(LE,Goals,DerivGE),LGE),
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
	(ProbE>0.0->
		Prob is ProbGE/ProbE,
		statistics(cputime,[_,CT2]),
		CPUTime2 is CT2/1000,
		statistics(walltime,[_,WT2]),
		WallTime2 is WT2/1000
	;
		Prob=undefined,
		statistics(cputime,[_,CT2]),
		CPUTime2 is CT2/1000,
		statistics(walltime,[_,WT2]),
		WallTime2 is WT2/1000
	).

solve_cond_goals(Goals,LE,0,Time1,0):-
	statistics(runtime,[_,_]),
	\+ find_deriv_GE(LE,Goals,_DerivGE),
	statistics(runtime,[_,T1]),
	Time1 is T1/1000.

call_compute_prob(NewVarGE,FormulaGE,ProbGE):-
	(setting(save_dot,true)->
		format("Variables: ~p~n",[NewVarGE]),
		compute_prob(NewVarGE,FormulaGE,ProbGE,1)
	;
		compute_prob(NewVarGE,FormulaGE,ProbGE,0)
	).

find_deriv_GE(LD,GoalsList,Deriv):-
	member(D,LD),
	solve(GoalsList,D,DerivDup),
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

solve([\+ H|T],CIn,COut):-
	builtin(H),!,
	call((\+ H)),
	solve(T,CIn,COut).

solve([\+ H |T],CIn,COut):-!,
	list2and(HL,H),
	findall(D,solve(HL,CIn,D),L),
		choose_clauses(CIn,L,C1),	
		solve(T,C1,COut)
	.	
solve([H|T],CIn,COut):-
	builtin(H),!,
	call(H),
	solve(T,CIn,COut).

solve([H|T],CIn,COut):-
	def_rule(H,B),
	append(B,T,NG),
	solve(NG,CIn,COut).
	
solve([H|T],CIn,COut):-
	find_rule(H,(R,S,N),CIn),
	solve_pres(R,S,N,T,CIn,COut).

solve_pres(R,S,N,T,CIn,COut):-
	member_eq((N,R,S),CIn),!,
	solve(T,CIn,COut).

solve_pres(R,S,N,T,CIn,COut):-
%	format("a",[]),
	solve(T,[(N,R,S)|CIn],COut).

/*
solve_pres(R,S,N,T,CIn,COut):-
	append(CIn,[(N,R,S)],C1),
	solve(T,C1,COut).
*/

	
	
/* find_rule(G,(R,S,N),Body,C) takes a goal G and the current C set and
returns the index R of a disjunctive rule resolving with G together with
the index N of the resolving head, the substitution S and the Body of the 
rule */
find_rule(H,(R,S,N),C):-
	rule(H,_P,N,R,S,_NH,_Head),
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

not_different(N,N,S,S).



/* choose_clauses(CIn,LC,COut) takes as input the current C set and 
the set of C sets for a negative goal and returns a new C set that 
excludes all the derivations for the negative goals */
choose_clauses(C,[],C).

choose_clauses(CIn,[D|T],COut):-
	set_diff(D,Cin,D1),
	member((N,R,S),D1),
	add_inc(N,R,S,CIn,T,COut).


add_inc(N,R,S,CIn,T,COut):-
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_clauses(CIn,T,COut).

add_inc(N,R,S,CIn,T,COut):-
	\+ member((N,R,S),CIn),
        new_head(N,R,S,N1),
	choose_clauses([(N1,R,S)|CIn],T,COut).

set_diff([H|_T],[H|_S],[]):-!.

set_diff([H|T],S,[H|R]):-
	set_diff(T,S,R).

set_diff(T,_S,T).

/*
set_diff([H|T],[H|S],R):-!,
	set_diff(T,S,R).

set_diff(T,_S,T).
*/
/*
set_diff([],_S,[]).

set_diff([H|T],S,R):-
	member(H,S),!,
	set_diff(T,S,R).

set_diff([H|T],S,[H|R]):-
	set_diff(T,S,R).
*/
/* select a head different from N for rule R with
substitution S, return it in N1 */
new_head(N,R,S,N1):-
	rule_by_num(R,S,Numbers,Head,_Body),
	Head\=uniform(_,_,_),!,
	nth0(N, Numbers, _Elem, Rest),
	member(N1,Rest).

already_present_with_a_different_head(N,R,S,[(NH,R,SH)|_T]):-
	S=SH,NH \= N,!.

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
build_formula([],[],Var,Var,C,C).

build_formula([D|TD],[F|TF],VarIn,VarOut,C0,C1):-
	length(D,NC),
	C2 is C0+NC,
	%reverse(D,D1),
	D1=D,
	build_term(D1,F,VarIn,Var1),
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
	atom_concat(File,'.cil',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
	retractall(rule_by_num(_,_,_,_,_)),
	retractall(rule(_,_,_,_,_,_,_)),
	retractall(def_rule(_,_)),
	process_clauses(C,1).

process_clauses([(end_of_file,[])],_N).
	
process_clauses([((H <- B),_V)|T],N):-!,
	convert_body(B,BL),
	assert(def_rule(H,BL)),
	process_clauses(T,N).


process_clauses([((prob H),V)|T],N):-!,
	list2and(HL1,H),
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
	assertz(rule(H,P,Pos,N,V1,NH,HL)),
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
	
convert_body((~ A & B),[\+ A|B1]):-!,
	convert_body(B,B1).

convert_body((A & B),[A|B1]):-!,
	convert_body(B,B1).

convert_body(~ A,[\+ A]):-!.

convert_body(A,[A]).

/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
	retract(setting(Parameter,_)),
	assert(setting(Parameter,Value)).

/* end of utility predicates */
