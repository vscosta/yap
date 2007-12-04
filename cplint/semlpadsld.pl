/*
	LPAD and CP-Logic reasoning suite
	File semlpadsld.pl
	Program for building the semantics of an LPAD
	Queries are answered by using Prolog in every instance
	Copyright (c) 2007, Fabrizio Riguzzi
*/

:-module(semlpadsld,[p/1,s/2,sc/3,set/2]).
:-use_module(library(lists)).
:-dynamic setting/2.
:-set_prolog_flag(unknown,fail).


setting(epsilon,0.00001).
setting(ground_body,false). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */

setting(grounding,modes).
/* available values: variables, modes
if set to variables, the universe facts from the .uni file are used
if set to modes, the mode and type declaration from the .uni file are used
*/

setting(verbose,false).


sc(Goals,Evidence,Prob):-
	s(Evidence,ProbE),
	append(Goals,Evidence,GE),
	s(GE,ProbGE),
	Prob is ProbGE/ProbE.

s(GoalsList,Prob):-
	program_names(L),
	list2and(GoalsList,Goals),
	run_query(L,Goals,0,Prob).

run_query([],_G,P,P).

run_query([Prog|T],Goal,PIn,POut):-
	elab_conj(Prog,Goal,Goal1),
	call(Goal1),
	prob(Prog,P),
	P1 is PIn+P,
	run_query(T,Goal,P1,POut).
	
run_query([Prog|T],Goal,PIn,POut):-
	elab_conj(Prog,Goal,Goal1),
	\+ call(Goal1),
	run_query(T,Goal,PIn,POut).

/* predicate for parsing the program file */	
p(File):-
	clean_db,
	atom_concat(File,'.cpl',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
	atom_concat(File,'.uni',FileUni),
	reconsult(FileUni),
	process_clauses(C,ClausesVar),
	instantiate(ClausesVar,[],Clauses),
	assert(program(1)),
	assert(program_names([])),
	create_programs(Clauses).

clean_db:-
	findall((P/A),(mode(Atom),functor(Atom,P,A0),A is A0+1),L),
	abolish_all(L),
	abolish(program/1),
	abolish(program_names/1),
	abolish(prob/2).

abolish_all([]).

abolish_all([(P/A)|T]):-
	abolish(P/A),
	abolish_all(T).

/* create_programs(Clauses)
	create the instances of the ground LPAD composed by Clauses
	Each instance is identified by an atom of the form P<Number> where <Number> is an
	increasing number. An extra argument is added to each atom in the clauses to represent
	the identifier of the instance.
*/
create_programs(Clauses):-
	create_single_program(Clauses,1,Program),
	retract(program(N)),
	number_codes(N,NC),
	atom_codes(NA,NC),
	atom_concat(p,NA,Name),
	N1 is N+1,
	assert(program(N1)),
	(setting(verbose,true)->
		format("Writing instance ~d~n",[N])
	;
		true
	),
	write_program(Name,Program),
	retract(program_names(L)),
	append(L,[Name],L1),
	assert(program_names(L1)),
	fail.

create_programs(_).


write_program(_Name,[]).

write_program(Name,[(H:-B)|T]):-
	elab_conj(Name,H,H1),
	elab_conj(Name,B,B1),
	assertz((H1:-B1)),
	write_program(Name,T).

/* elab_conj(Name,Conj0,Conj)
	adds the extra argument Name to the conjunction Conj0 resulting in Conj
*/
elab_conj(_Name,true,true):-!.

elab_conj(Name,\+(B),\+(B1)):-!,
	elab_conj(Name,B,B1).
		
elab_conj(Name,(BL,Rest),(BL1,Rest1)):-!,
	elab_conj(Name,BL,BL1),
	elab_conj(Name,Rest,Rest1).

elab_conj(Name,bagof(V,EV^G,L),bagof(V,EV^GL,L)):-!,
	elab_conj(Name,G,GL).

elab_conj(Name,bagof(V,G,L),bagof(V,GL,L)):-!,
	elab_conj(Name,G,GL).

elab_conj(Name,setof(V,EV^G,L),setof(V,EV^GL,L)):-!,
	elab_conj(Name,G,GL).

elab_conj(Name,setof(V,G,L),setof(V,GL,L)):-!,
	elab_conj(Name,G,GL).

elab_conj(Name,findall(V,G,L),findall(V,GL,L)):-!,
	elab_conj(Name,G,GL).

elab_conj(_Name,A,A):-
	bg(A),!.

elab_conj(_Name,A,A):-
	builtin(A),!.

elab_conj(Name,Lit,Lit1):-
	Lit\=(_,_),
	Lit=..[Pred|Args],
	Lit1=..[Pred,Name|Args].


create_single_program([],P,[(prob(P):-true)]).
	
create_single_program([r(H,B)|T],PIn,[(HA:-B)|T1]):-
	member((HA:P),H),
	P1 is PIn*P,
	create_single_program(T,P1,T1).

/* predicates for producing the ground instances of program clauses */

/* instantiate(Clauses,C0,C)
	returns in C the set of clauses obtained by grounding Clauses
*/
instantiate([],C,C).

instantiate([r(_V,[H:1],B)|T],CIn,COut):-!,
	append(CIn,[r([H:1],B)],C1),
	instantiate(T,C1,COut).

instantiate([r(V,H,B)|T],CIn,COut):-
	(setting(grounding,variables)->
		findall(r(H,BOut),instantiate_clause_variables(V,H,B,BOut),L)
	;
		findall(r(H,BOut),instantiate_clause_modes(H,B,BOut),L)
	),
	append(CIn,L,C1),
	instantiate(T,C1,COut).
		

instantiate_clause_modes(H,B,BOut):-
	instantiate_head_modes(H),
	list2and(BL,B),
	instantiate_body_modes(BL,BLOut),
	list2and(BLOut,BOut).


instantiate_head_modes([]):-!.

instantiate_head_modes([H:_P|T]):-
	instantiate_atom_modes(H),
	instantiate_head_modes(T).


instantiate_body_modes(BL,BL):-
	setting(ground_body,false),!.
	
instantiate_body_modes(BL0,BL):-
	instantiate_list_modes(BL0,BL).


instantiate_list_modes([],[]).

instantiate_list_modes([H|T0],T):-
	builtin(H),!,
	call(H),
	instantiate_list_modes(T0,T).

instantiate_list_modes([\+ H|T0],T):-
	builtin(H),!,
	\+ call(H),
	instantiate_list_modes(T0,T).

instantiate_list_modes([\+ H|T0],[\+ H|T]):-!,
	instantiate_atom_modes(H),
	instantiate_list_modes(T0,T).

instantiate_list_modes([H|T0],[H|T]):-
	instantiate_atom_modes(H),
	instantiate_list_modes(T0,T).


instantiate_atom_modes(''):-!.

instantiate_atom_modes(A):-
	functor(A,F,NArgs),
	functor(TA,F,NArgs),
	A=..[F|Args],
	mode(TA),
	TA=..[F|Types],
	instantiate_args_modes(Args,Types).


instantiate_args_modes([],[]):-!.

instantiate_args_modes([H|T],[TH|TT]):-
	type(TH,Constants),
	member(H,Constants),
	instantiate_args_modes(T,TT).


instantiate_clause_variables([],_H,B,BOut):-
	list2and(BL,B),
	(setting(ground_body,true)->
		check_body(BL,BLOut)
	;
		BLOut=BL
	),
	list2and(BLOut,BOut).

instantiate_clause_variables([VarName=Var|T],H,BIn,BOut):-
	universe(VarNames,U),
	member(VarName,VarNames),
	member(Var,U),
	instantiate_clause_variables(T,H,BIn,BOut).	

instantiate_clause_variables([VarName=_Var|T],H,BIn,BOut):-
	\+ varName_present_variables(VarName),!,
	instantiate_clause_variables(T,H,BIn,BOut).	


varName_present_variables(VarName):-
	universe(VarNames,_U), member(VarName,VarNames).

/* check_body(Body0,Body)
	removes the true builtin literals from Body0. Fails if there is a false builtin literal.
*/
check_body([],[]).

check_body([H|T],TOut):-
	builtin(H),!,
	call(H),
	check_body(T,TOut).

check_body([H|T],[H|TOut]):-
	check_body(T,TOut).
	

/* predicates for processing the clauses read from the file */
/* process_clauses(Terms,Clauses)
	processes Terms to produce Clauses
	Terms is a list contatining elements of the form
	((H:-B),V)
	Clauses is a list containing elements of the form
	r(V,HL,BL)
	where HL is the list of disjuncts in H and BL is the list
	of literals in B
*/
process_clauses([(end_of_file,[])],[]).

process_clauses([((H:-B),V)|T],[r(V,HL,B)|T1]):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).

process_clauses([((H:-B),V)|T],[r(V,HL,B)|T1]):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).
	
process_clauses([((H:-B),V)|T],[r(V,[H:1],B)|T1]):-!,
	process_clauses(T,T1).

process_clauses([(H,V)|T],[r(V,HL,true)|T1]):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).

process_clauses([(H,V)|T],[r(V,HL,true)|T1]):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).
	
process_clauses([(H,V)|T],[r(V,[H:1],true)|T1]):-
	process_clauses(T,T1).

process_head([H:PH],P,[H:PH1|Null]):-
	PH1 is PH,
	PNull is 1-P-PH1,
	setting(epsilon,Eps),
	EpsNeg is - Eps,
	PNull > EpsNeg,
	(PNull>Eps->
		Null=['':PNull]
	;
		Null=[]
	).

process_head([H:PH|T],P,[H:PH1|NT]):-
	PH1 is PH,
	P1 is P+PH1,
	process_head(T,P1,NT).
	

/* predicates for reading in the program clauses */
/* read_clauses(S,Clauses)
	read Clauses from stream S
*/
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


/* extract_vars_cl(Clause,VariableNames,Couples)
	extract from Clause couples of the form VariableName=Variable
*/
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

member_eq(A,[H|_T]):-
	A==H,!.
	
member_eq(A,[_H|T]):-
	member_eq(A,T).

/* auxiliary predicates */
list2or([X],X):-
		X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
		list2or(T,Ta).


list2and([],true):-!.

list2and([X],X):-
		X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
		list2and(T,Ta).


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


bg(member(_El,_L)).
bg(average(_L,_Av)).
bg(max_list(_L,_Max)).
bg(min_list(_L,_Max)).


average(L,Av):-
	sum_list(L,Sum),
	length(L,N),
	Av is Sum/N.

/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
	retract(setting(Parameter,_)),
	assert(setting(Parameter,Value)).
