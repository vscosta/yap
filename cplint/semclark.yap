/*

Program for computing the probability of a query directly according to the 
semantics

Copyright (c) 2007, Fabrizio Riguzzi


*/
:-use_module(library(lists)).
:-dynamic setting/2.
:-set_prolog_flag(unknown,fail).


setting(epsilon,0.00001).
setting(test_builtins,false).

solve(GoalsList,Prob):-
	s(GoalsList,Prob).


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
	
p(File):-
	atom_concat(File,'.uni',FileUni),
	consult(FileUni),
	atom_concat(File,'.cpl',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
	process_clauses(C,ClausesVar),
	instantiate(ClausesVar,[],Clauses),
	assert(program(1)),
	assert(program_names([])),
	create_programs(Clauses).



create_programs(Clauses):-
	create_single_program(Clauses,1,Program),
	retract(program(N)),
	number_codes(N,NC),
	atom_codes(NA,NC),
	atom_concat(p,NA,Name),
	N1 is N+1,
	assert(program(N1)),
	format("Writing program ~d~n",[N]),
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

instantiate([],C,C).

instantiate([r(V,H,B)|T],CIn,COut):-
	findall(r(H,BOut),instantiate_clause(V,H,B,BOut),L),
	append(CIn,L,C1),
	instantiate(T,C1,COut).

check_body([],[]).

check_body([H|T],TOut):-
	builtin(H),setting(test_builtins,true),!,
	call(H),
	check_body(T,TOut).

check_body([H|T],[H|TOut]):-
	check_body(T,TOut).


instantiate_clause([],_H,B,BOut):-
	list2and(BL,B),
	check_body(BL,BLOut),
	list2and(BLOut,BOut).


instantiate_clause([VarName=Var|T],H,BIn,BOut):-
	universe(VarNames,U),
	member(VarName,VarNames),
	member(Var,U),
	instantiate_clause(T,H,BIn,BOut).	

instantiate_clause([VarName=_Var|T],H,BIn,BOut):-
	\+ varName_present(VarName),!,
	instantiate_clause(T,H,BIn,BOut).	

varName_present(VarName):-
	universe(VarNames,_U), member(VarName,VarNames).

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

read_clauses(S,[(Cl,V)|Out]):-
	read_term(S,Cl,[variable_names(V)]),
	(Cl=end_of_file->
		Out=[]
	;
		read_clauses(S,Out)
	).



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

