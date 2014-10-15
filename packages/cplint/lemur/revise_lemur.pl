/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi, Nicola di Mauro and Elena Bellodi

*/
:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).

:- [dv_lemur].


theory_revisions_op(Theory,TheoryRevs):-
  setof(RevOp, Theory^revise_theory(Theory,RevOp), TheoryRevs),!.
theory_revisions_op(_Theory,[]).

filter_add_rule([],[]).
filter_add_rule([add(Rule)|R],R1):-
	!,
	filter_add_rule(R,R1).
filter_add_rule([A|R],[A|R1]):-
	!,
	filter_add_rule(R,R1).


theory_revisions_r(Theory,TheoryRevs):-
	theory_revisions_op(Theory,TheoryRevs1),
%	filter_add_rule(TheoryRevs11,TheoryRevs1),

	
	( TheoryRevs1 == [] ->
		TheoryRevs = []
	;
		length(TheoryRevs1,L),
		random(0,L,K),
		nth0(K, TheoryRevs1,Revision),
		apply_operators([Revision],Theory,TheoryRevs)
	).


theory_revisions(Theory,TheoryRevs):-
  theory_revisions_op(Theory,TheoryRevs1),
  apply_operators(TheoryRevs1,Theory,TheoryRevs).


apply_operators([],_Theory,[]).

apply_operators([add(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  append(Theory, [Rule], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule,NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).


revise_theory(Theory,Ref):-
  specialize_theory(Theory,Ref).

revise_theory(Theory,Ref):-
  generalize_theory(Theory,Ref).

/*
generalize_theory(Theory,Ref):-
  Theory \== [],
  choose_rule(Theory,Rule),
  generalize_rule(Rule,Ref).
*/
generalize_theory(Theory,Ref):-
  length(Theory,LT),
  setting(max_rules,MR),
  LT<MR,
  add_rule(Ref).


generalize_rule(Rule,Ref):-
  generalize_head(Rule,Ref).

generalize_rule(Rule,Ref):-
  generalize_body(Rule,Ref).


add_rule(add(rule(ID,Head,[],Lits))):-
  setting(specialization,bottom),!,
  database(DB),
  sample(1,DB,[M]),
  get_head_atoms(O),
  member(A,O),
  functor(A,F,N),    
  functor(F1,F,N),   
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Pred1),L),
  sample(1,L,LH),
  generate_body(LH,[rule(ID,Head,[],Lits)]).

add_rule(add(SpecRule)):-
  findall(HL , modeh(_,HL), HLS),
  length(HLS,L),
  L1 is L+1,
  P is 1/L1,
  generate_head(HLS,P,Head),
	get_next_rule_number(ID),
	Rule0 = rule(ID,Head,[],true),
	specialize_rule(Rule0,SpecRule,Lit).



generate_head([H|_T],_P,[H1:0.5,'':0.5]):-
  H=..[Pred|Args],
  length(Args,LA),
  length(Args1,LA),
  H1=..[Pred|Args1],
	check_for_constants(Args,Args1).

check_for_constants([],[]).
check_for_constants([+X|R],[V|R1]):-
	!,
	check_for_constants(R,R1).
check_for_constants([-X|R],[V|R1]):-
	!,
	check_for_constants(R,R1).
check_for_constants([X|R],[X|R1]):-
	check_for_constants(R,R1).



generate_head([_H|T],P,Head):-
  generate_head(T,P,Head).


generalize_head(Rule,Ref):-
  Rule = rule(ID,LH,BL,L),
  generalize_head1(LH,LH1,NewAt),
  Ref = add_head(Rule,rule(ID,LH1,BL,L),NewAt).


generalize_head1(LH,LH1,NH):-
  findall(HL , modeh(_,HL), HLS),
  generalize_head2(HLS,LH,LH1,NH).


generalize_head2([X|_R],LH,LH1,PH) :-
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  \+ member(PH:_, LH),
  (setting(new_head_atoms_zero_prob,true)->
    delete_matching(LH,'':PNull,LH0),
    append(LH0,[PH:0.0,'':PNull],LH1)
  ;
    length(LH,NH),
    add_to_head(LH,NH,PH,LH1)
  ).

generalize_head2([_X|R],LH,LH1) :-
  generalize_head2(R,LH,LH1).


add_to_head(['':PN],NH,At,[At:PA,'':PN1]):-!,
  PN1 is PN*NH/(NH+1),
  PA is 1/(NH+1).

add_to_head([H:PH|T],NH,At,[H:PH1|T1]):-
  PH1 is PH*NH/(NH+1),
  add_to_head(T,NH,At,T1).
  

get_module_var(LH,Module):-
  member(H:_,LH),!,
  H=..[_F,Module|_].


generalize_body(Rule,Ref):-
  Rule = rule(ID,LH,BL),
  delete_one(BL,BL1,A),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  linked_clause(BL1,LH2),
  Ref = remove_body(Rule,rule(ID,LH,BL1),A).
  

specialize_theory(Theory,Ref):-
  Theory \== [],
  choose_rule(Theory,Rule),
  specialize_rule(Rule,SpecRule,Lit),
  Ref = add_body(Rule,SpecRule,Lit),
	SpecRule = rule(_,_,B,_).

/*,
	\+ (member(b_rel11(X1,Y1),B), member(b_rel11(Z1,Y1),B), Y1 \== Z1),
	\+ (member(b_rel12(X2,Y2),B), member(b_rel12(Z2,Y2),B), Y2 \== Z2),
	\+ (member(b_rel13(X3,Y3),B), member(b_rel13(Z3,Y3),B), Y3 \== Z3).*/

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ lookahead_cons(Lit,_),
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
%  check_ref(LH1,BL1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits).

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  append(BL,[Lit],BL0),
  (lookahead(Lit,LLit1);lookahead_cons(Lit,LLit1)),  % lookahead_cons serve a dire che rating(_A,_B,_C) e' aggiunto solo  insieme ai letterali indicati nella lista, mai da solo.
  copy_term(LLit1,LLit2),
  specialize_rule_la_bot(LLit2,RLits,RLits1,BL0,BL1),
  remove_prob(LH,LH1),
%  check_ref(LH1,BL1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1).

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,mode),!,
%  findall(BL , modeb(_,BL), BLS),
	mcts_modeb(BSL0),
	Rule = rule(ID,LH,BL,_),
	( BL \= [] ->
		%last(BL,LastLit),
		%LastLit =.. [Pred|_],
		%filter_modeb(BSL0,LastLit,BSL)
		BSL = BSL0
	;
		BSL = BSL0
	),
  specialize_rule(BSL,Rule,SpecRule,Lit).

filter_modeb([],_Pred,[]).
filter_modeb([Modeb|RestModeb],Pred,[Modeb|RestBSL]):-
	Modeb =.. [PredMode|_],
	Modeb @>= Pred,
	!,
	filter_modeb(RestModeb,Pred,RestBSL).
filter_modeb([_|RestModeb],Pred,RestBSL):-
	filter_modeb(RestModeb,Pred,RestBSL).


skolemize(Theory,Theory1):-
	copy_term(Theory,Theory1),
	term_variables(Theory1,Vars),
	skolemize1(Vars,1).

skolemize1([],_).
skolemize1([Var|R],K):-
	atomic_list_concat([s,K],Skolem),
	Var = Skolem,
	K1 is K + 1,
	skolemize1(R,K1).


banned_clause(H,B):-
	skolemize([H,B],[H1,B1]),
  banned(H2,B2),
  mysublist(H2,H1),
  mysublist(B2,B1).


mysublist([],_).

mysublist([A\==B|T],L):-
	!,
	A\==B,
  mysublist(T,L).
mysublist([H|T],L):-
	nth(_,L,H,R),
  mysublist(T,R).


check_ref(H,B):-
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  (ref(H1,B1)->
    fail
  ;
    assert(ref(H1,B1))
  ).


specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  (lookahead(SLit,LLit1);lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  \+ banned_clause(LH1,BL2),		
  SpecRul = rule(ID,LH,BL2,true).

specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),

%	\+ member(SLit,LH1), %%%%

  \+ lookahead_cons(SLit,_),

	
  append(BL,[SLit],BL1),

  append(LH1,BL1,ALL1),

%  dv(LH1,BL1,DList),						%var,depth list DList in output
	
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  setting(maxdepth_var,MD),	
%	exceed_depth(DList,MD),				%fallisce se una sottolista eccede MD
  \+ banned_clause(LH1,BL1),	
  SpecRul = rule(ID,LH,BL1,true).

specialize_rule([_|RLit],Rule,SpecRul,Lit):-
  specialize_rule(RLit,Rule,SpecRul,Lit).


specialize_rule_la([],_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,LH1,BL2,BL3).


specialize_rule_la_bot([],Bot,Bot,BL,BL).

specialize_rule_la_bot([Lit|T],Bot0,Bot,BL1,BL3):-
  delete_one(Bot0,Bot1,Lit),
  append(BL1,[Lit],BL2),
  specialize_rule_la_bot(T,Bot1,Bot,BL2,BL3).


remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


specialize_rule1(Lit,Lits,SpecLit):-
  Lit =.. [Pred|Args],
  exctract_type_vars(Lits,TypeVars0),  
  remove_duplicates(TypeVars0,TypeVars),
  take_var_args(Args,TypeVars,Args1),
  SpecLit =.. [Pred|Args1],
  \+ member_eq(SpecLit,Lits).


convert_to_input_vars([],[]):-!.

convert_to_input_vars([+T|RT],[+T|RT1]):-
  !,
  convert_to_input_vars(RT,RT1).

convert_to_input_vars([-T|RT],[+T|RT1]):-
  convert_to_input_vars(RT,RT1).


member_eq(X,[Y|_List]) :-
  X == Y.

member_eq(X,[_|List]) :-
  member_eq(X,List).


remove_eq(X,[Y|R],R):-
  X == Y,
  !.

remove_eq(X,[_|R],R1):-
  remove_eq(X,R,R1).


linked_clause(X):-
  linked_clause(X,[]).

linked_clause([],_).

linked_clause([L|R],PrevLits):-
  term_variables(PrevLits,PrevVars),
  input_variables(L,InputVars),
  linked(InputVars,PrevVars),!,
  linked_clause(R,[L|PrevLits]).


linked([],_).

linked([X|R],L) :-
  member_eq(X,L),
  !,
  linked(R,L).
  

input_variables(\+ LitM,InputVars):-
  !,
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  copy_term(LitM,Lit0),
  modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(Lit0,Lit2,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  modeh(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_vars(Lit,Lit1,InputVars):-
  Lit =.. [_|Vars],
  Lit1 =.. [_|Types],
  input_vars1(Vars,Types,InputVars).


input_vars1([],_,[]).

input_vars1([V|RV],[+_T|RT],[V|RV1]):-
  !,
  input_vars1(RV,RT,RV1).

input_vars1([_V|RV],[_|RT],RV1):-
  input_vars1(RV,RT,RV1).


exctract_type_vars([],[]).

exctract_type_vars([Lit|RestLit],TypeVars):-
  Lit =.. [Pred|Args],
  length(Args,L),
  length(Args1,L),
  Lit1 =.. [Pred|Args1],
  take_mode(Lit1),
  type_vars(Args,Args1,Types),
  exctract_type_vars(RestLit,TypeVars0),
  !,
  append(Types,TypeVars0,TypeVars).


take_mode(Lit):-
  modeh(_,Lit),!.

take_mode(Lit):-
  modeb(_,Lit),!.

take_mode(Lit):-
  mode(_,Lit),!.


type_vars([],[],[]).

type_vars([V|RV],[+T|RT],[V=T|RTV]):-
  !,
  type_vars(RV,RT,RTV).

type_vars([V|RV],[-T|RT],[V=T|RTV]):-atom(T),!,
  type_vars(RV,RT,RTV).

type_vars([_V|RV],[_T|RT],RTV):-
  type_vars(RV,RT,RTV).


take_var_args([],_,[]).

take_var_args([+T|RT],TypeVars,[V|RV]):-
  !,
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[_V|RV]):-
  atom(T),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[V|RV]):-
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([T|RT],TypeVars,[T|RV]):-
  T\= + _,(T\= - _; T= - A,number(A)),  
  take_var_args(RT,TypeVars,RV).


choose_rule(Theory,Rule):-
	( setting(mcts_covering,true)	->
		mcts_restart(Restart),
		nth(K,Theory,Rule),
		K >= Restart
	;
		member(Rule,Theory)
	).
	%last(Theory,Rule).

add_rule(Theory,add(rule(ID,H,[],true))):-
  new_id(ID),
  findall(HL , modeh(_,HL), HLS),
  length(HLS,NH),
  P is 1/(NH+1),
  add_probs(HLS,H,P),
  \+ member(rule(_,H,[],true),Theory).

add_rule(Theory,TheoryGen):-
  findall(HL , modeh(_,HL), HLS),
  add_rule(HLS,Theory,TheoryGen).

add_rule([X|_R],Theory,TheoryGen) :-
  new_id(ID),
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  TheoryGen = add(rule(ID,[PH:0.5,'':0.5],[],true)),
  \+ member(rule(_,[PH:_,'':_],[],true),Theory).

add_rule([_X|R],Theory,TheoryGen) :-
  add_rule(R,Theory,TheoryGen).


add_probs([],['':P],P):-!.

add_probs([H|T],[H:P|T1],P):-
  add_probs(T,T1,P).


extract_fancy_vars(List,Vars):-
  term_variables(List,Vars0),
  fancy_vars(Vars0,1,Vars).


fancy_vars([],_,[]).

fancy_vars([X|R],N,[NN2=X|R1]):-
  name(N,NN),
  append([86],NN,NN1),
  name(NN2,NN1),
  N1 is N + 1,
  fancy_vars(R,N1,R1).


delete_one([X|R],R,X).

delete_one([X|R],[X|R1],D):-
  delete_one(R,R1,D).


remove_last([_X],[]) :-
  !.

remove_last([X|R],[X|R1]):-
  remove_last(R,R1).


delete_matching([],_El,[]).

delete_matching([El|T],El,T1):-!,
  delete_matching(T,El,T1).

delete_matching([H|T],El,[H|T1]):-
  delete_matching(T,El,T1).
  
