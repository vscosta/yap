/*

Program for computing the probability of a query directly according to the 
semantics

Copyright (c) 2007, Fabrizio Riguzzi


*/

:-module(semcpl,[p/1,s/2,sc/3,build/0,print/0,set/2]).

:-use_module(library(lists)).
:-dynamic setting/2.
%:-set_prolog_flag(unknown,fail).

setting(epsilon_parsing,0.00001).
setting(ground_body,false). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */

/* sc(Goal,Evidence,Probability)
	computes a conditional probability
*/
sc(G,E,P):-
	append(G,E,GE),
	s(GE,PGE),
	s(E,PE),
	(PE\==0->
		P is PGE/PE
	;
		P=undefined
	).
/* s(GoalsList,Prob)
	computes the probability of a query in the form of a list of literals
*/	
s(GoalsList,Prob):-
	findall((State,Prob),node(empty,_,_,State,Prob),LL),
	sum_prob(LL,GoalsList,0,Prob).

/* sum_prob(List,GoalsList,P0,P)
	List is a list of couples (State,Prob) where State is an interpretation and
Prob is the associated probability.
	GoalsList is a list of goals.
	P0/P is an accumulator for the probability
	sum_prob computes the probability of GoalsList by summing the probability of every
	state where GoalsList is true
*/
sum_prob([],_GL,P,P):-!.

sum_prob([(State,Prob)|T],GL,P0,P):-
	copy_term(GL,GLC),
	(body_true(GLC,State)->
		P1 is P0+Prob
	;
		P1 is P0
	),
	sum_prob(T,GL,P1,P).
	
/* predicates for build the probabilistic process tree */	
/* build
	builds the probabilistic process tree with an empty context
*/
build:-
	build([]).

/* build(Context)
	builds the probabilistic process tree with context Context
*/
build(Context):-
	clauses(Cl),
	herbrand_base(HB),
	deleteall(HB,Context,HB1),
	get_new_atom(A),
	assert(root(A)),
	build(A,Context,1,Cl,HB1).

/* build(Parent,State,Prob,Clauses,HB):-
	Given a parent node, its State, its probability, the list of ground
	clauses Clauses and the Herbrand Base HB, it builds the tree below Parent.
	The tree is stored in the database in the form of facts of the form
	node(Node,Parent,State,r(Head,Body),Prob).
	Parent is the parent of Node, State is the interpretation associated to
	Node, r(Head,Body) is the clause associated to Node and Prob is its Probability.
	The real root of the tree has a dummy parent P for which the fact root(P) is 
	true that is used to access the tree
*/
build(Parent,State,Prob,Clauses,HB):-
	get_new_atom(Node),
	compute_three_valued(State,Clauses,HB,[],Unknowns),
	choose_clause(Clauses,State,Unknowns,RemainingClauses,Clause),
	(Clause=empty->
		(RemainingClauses=[]->
			assert(node(empty,Node,Parent,State,Prob))
		;
			format("Invalid program.~nInterpretation=~p.~nUnknowns atoms=~p.~nClauses=~p~n",[State,Unknowns,RemainingClauses])
		)
	;
		Clause=r(Head,Body),
		assert(node(r(Head,Body),Node,Parent,State,Prob)),
		new_states(Node,Head,State,Prob,RemainingClauses,Unknowns)
	).
/* choose_clause(Clauses,Trues,Unknowns,RemainingClauses,Clause)
	selects a clause whose body is true in the three valued interpretation 
	represented by Trues and Unknowns. The selected clause is returned in Clause
	and the remaining clauses in RemainingClauses. If no clause has the body
	true, it returns empty in Clause.
*/
choose_clause([],_True,_Unk,[],empty):-!.

choose_clause([r(Head,Body)|T],True,Unk,RemCl,Clause):-
	copy_term(Body,BodyC),
	(body_true(BodyC,True,[],Unk)->
		Clause=r(Head,Body),
		remove_false(T,True,Unk,RemCl)
	;
		(body_false(BodyC,True,[],Unk)->
			RemCl=RemCl0
		;
			RemCl=[r(Head,Body)|RemCl0]
		),
		choose_clause(T,True,Unk,RemCl0,Clause)
	).

remove_false([],_True,_Unk,[]):-!.

remove_false([r(Head,Body)|T],True,Unk,RemCl):-
	copy_term(Body,BodyC),
	(body_false(BodyC,True,[],Unk)->
		RemCl=RemCl0
	;
		RemCl=[r(Head,Body)|RemCl0]
	),
	remove_false(T,True,Unk,RemCl0).
	

body_true([],_,_,_):-!.

body_true([\+ H|T],True,False,Unk):-
	(\+ ground(H)->
		format("Floundering ~n",[])
	;
		true
	),
	\+ member(H,True),\+ member(H,Unk),!,
	body_true(T,True,False,Unk).

body_true([H|T],True,False,Unk):-
	H \= (\+ _A),
	member(H,True),
	body_true(T,True,False,Unk).

body_true([H|T],True,False,Unk):-
	builtin(H),
	call(H),
	body_true(T,True,False,Unk).

body_true([],_):-!.

body_true([\+ H|T],True):-
	\+ member(H,True),!,
	body_true(T,True).

body_true([H|T],True):-
	member(H,True),!,
	body_true(T,True).


body_undef([\+ H|T],True,False,Unk):-
	member(H,False),!,
	body_undef(T,True,False,Unk).

body_undef([\+ H|T],True,False,Unk):-
	\+ member(H,True),\+ member(H,Unk),!,
	body_undef(T,True,False,Unk).

body_undef([\+ H|T],True,False,Unk):-
	member(H,Unk),!,
	\+ body_false(T,True,False,Unk).

body_undef([H|T],True,False,Unk):-
	member(H,True),!,
	body_undef(T,True,False,Unk).

body_undef([H|T],True,False,Unk):-
	member(H,Unk),!,
	\+ body_false(T,True,False,Unk).
	
/* compute_three_valued(State,Clauses,False,Unknowns0,Unknowns)
	computes the three valued interpretation associated with State
*/
compute_three_valued(State,Clauses,False,Unknowns0,Unknowns):-
	choose_clause_three_val(Clauses,State,False,Unknowns0,RemainingClauses,Clause),
	(Clause=empty->
		Unknowns=Unknowns0
	;
		Clause=r(Head,_Body),
		new_int(Head,False,False1,Unknowns0,Unknowns1),
		compute_three_valued(State,RemainingClauses,False1,Unknowns1,Unknowns)
	).
/* choose_clause_three_val(Clauses,Trues,False,Unknowns,RemainingClauses,Clause)
	selects a clause whose body is not false in the three valued interpretation 
	represented by Trues, False and Unknowns. The selected clause is returned in Clause
	and the remaining clauses in RemainingClauses. If no clause has the body
	true, it returns empty in Clause.
*/
choose_clause_three_val([],_True,_False,_Unk,_,empty):-!.

choose_clause_three_val([r(Head,Body)|T],True,False,Unk,RemCl,Clause):-
	(\+ body_false(Body,True,False,Unk)->
		Clause=r(Head,Body),
		RemCl=T
	;
		RemCl=[r(Head,Body)|RemCl0],
		choose_clause_three_val(T,True,False,Unk,RemCl0,Clause)
	).

body_false([\+ H|_T],True,_False,_Unk):-
	(\+ ground(H)->
		format("Floundering ~n",[])
	;
		true
	),
	member(H,True),!.

body_false([\+ H|T],True,False,Unk):-
	builtin(H),
	(call(H)->
		true
	;
		body_false(T,True,False,Unk)
	).

body_false([\+ _H|T],True,False,Unk):-!,
	body_false(T,True,False,Unk).

body_false([H|T],True,False,Unk):-
	builtin(H),
	call(H),!,
	body_false(T,True,False,Unk).

body_false([H|_T],_True,_False,_Unk):-
	builtin(H),
	\+ call(H),!.

body_false([H|T],True,False,Unk):-
	findall(H,(member(H,True);member(H,Unk)),LH),!,
	body_false_list(LH,H,T,True,False,Unk).


body_false_list([],_,_,_,_,_):-!.

body_false_list([H|T],A,Body,True,False,Unk):-
	copy_term((A,Body),(AC,BodyC)),
	AC=H,
	body_false(BodyC,True,False,Unk),
	body_false_list(T,A,Body,True,False,Unk).


/* new_int(Head,False0,False,Unk0,Unk)
	computes a new three valued interpretation from False0/Unk0 by moving the atoms
	in the head from False to Unk
*/		
new_int([],False,False,Unk,Unk):-!.

new_int([H:_P|T],False0,False,Unk0,Unk):-
	(member(H,False0)->
		delete(False0,H,False1),
		append(Unk0,[H],Unk1)
	;
		False1=False0,
		Unk1=Unk0
	),
	new_int(T,False1,False,Unk1,Unk).



/* new_states(Node,Head,State,Prob,Clauses,HB)
	computest the tree below Node, where Head is the head of the clause
	associated to Node, Prob is the probability of Node, Clauses is the list
	of ground clauses yet to be associated to a node and HB is the Herbrand Base.
*/	
new_states(_,[],_,_,_,_):-!.
	

new_states(Node,[H:P|T],State,Prob,Clauses,HB):-
	Prob1 is P*Prob,
	(member(H,State)->
		NewState=State
	;
		append(State,[H],NewState)
	),
	build(Node,NewState,Prob1,Clauses,HB),
	new_states(Node,T,State,Prob,Clauses,HB).	

/* get_new_atom(Atom)
	returns a new Atom of the form nNumber
*/
get_new_atom(Atom):-
	retract(new_number(N)),
	N1 is N+1,
	assert(new_number(N1)),
	number_atom(N,NA),
	atom_concat('n',NA,Atom).

/* predicates for printing the probabilistic process tree */
print:-
	root(Root),
	print_children(Root,"").
	
print_children(Parent,Tab):-
	findall((Node,State,Clause,Prob),node(Clause,Node,Parent,State,Prob),LC),
	print_list(LC,Tab).
	
print_list([],_Tab):-!.

print_list([(Node,State0,Clause,Prob)|T],Tab):-
	delete(State0,'',State),
	(Clause=empty-> % leaf
		format("~s~p  ~f~n",[Tab,State,Prob])
	;
		format("~s~p ~p ~f~n",[Tab,State,Clause,Prob]),
		append(Tab,"|  ",Tab1),
		print_children(Node,Tab1)
	),
	print_list(T,Tab).
	



/* predicate for parsing the program file */		
p(File):-
	parse(File),
	build.
		
parse(File):-
	retractall(root(_)),
	retractall(clauses(_)),
	retractall(herbrand_base(_)),
	retractall(node(_,_,_,_,_)),
	retractall(new_number(_)),
	assert(new_number(0)),
	atom_concat(File,'.cpl',FilePl),
	open(FilePl,read,S),
	read_clauses(S,C),
	close(S),
	atom_concat(File,'.uni',FileUni),
	reconsult(FileUni),
	process_clauses(C,ClausesVar),
	instantiate(ClausesVar,[],Clauses),
	assert(clauses(Clauses)),
	build_herbrand_base(HB),
	assert(herbrand_base(HB)).


build_herbrand_base(HB):-
	findall(A,mode(A),LA),
	inst_list(LA,[],HB).

/* inst_list(Atoms,HB0,HB)
	enlarges the Herbrand Base by instantiating the atoms in Atoms
*/
inst_list([],HB,HB):-!.

inst_list([H|T],HB0,HB):-
	functor(H,F,Args),
	functor(A,F,Args),
	findall(A,instantiate_atom_modes(A),LA),
	append(HB0,LA,HB1),
	inst_list(T,HB1,HB).

/* instantiate(Clauses,C0,C)
	returns in C the set of clauses obtained by grounding Clauses
*/
instantiate([],C,C).

instantiate([r(_V,H,B)|T],CIn,COut):-
	findall(r(H,BOut),instantiate_clause_modes(H,B,BOut),L),
	append(CIn,L,C1),
	instantiate(T,C1,COut).


instantiate_clause_modes(H,B,BOut):-
	instantiate_head_modes(H),
	instantiate_body_modes(B,BOut).


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

process_clauses([((H:-B),V)|T],[r(V,HL,BL)|T1]):-
	H=(_;_),!,
	list2or(HL1,H),
	list2and(BL,B),
	process_head(HL1,0,HL),
	process_clauses(T,T1).

process_clauses([((H:-B),V)|T],[r(V,HL,BL)|T1]):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	list2and(BL,B),
	process_clauses(T,T1).
	
process_clauses([((H:-B),V)|T],[r(V,[H:1],BL)|T1]):-!,
	list2and(BL,B),
	process_clauses(T,T1).

process_clauses([(H,V)|T],[r(V,HL,[])|T1]):-
	H=(_;_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).

process_clauses([(H,V)|T],[r(V,HL,[])|T1]):-
	H=(_:_),!,
	list2or(HL1,H),
	process_head(HL1,0,HL),
	process_clauses(T,T1).
	
process_clauses([(H,V)|T],[r(V,[H:1],[])|T1]):-
	process_clauses(T,T1).

process_head([H:PH],P,[H:PH1|Null]):-
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

process_head([H:PH|T],P,[H:PH1|NT]):-
	PH1 is PH,
	P1 is P+PH1,
	process_head(T,P1,NT).

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
	
pair(_VN,[],[]).

pair([VN= _V|TVN],[V|TV],[VN=V|T]):-
	pair(TVN,TV,T).	
	
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

assert_all([]):-!.

assert_all([(:- G)|T]):-!,
	call(G),
	assert_all(T).
	
assert_all([H|T]):-!,
	assertz((H)),
	assert_all(T).
	
assert_all(C):-
	assertz((C)).

deleteall(L,[],L).

deleteall(L,[H|T],LOut):-
  delete(L,H,L1),
  deleteall(L1,T,LOut).

member_eq(A,[H|_T]):-
	A==H.
	
member_eq(A,[_H|T]):-
	member_eq(A,T).

/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
	retract(setting(Parameter,_)),
	assert(setting(Parameter,Value)).
