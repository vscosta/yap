/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzii, Nicola di Mauro and Elena Bellodi

*/
:- use_module(library(terms)).
:- use_module(library(lists)).

:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).



theory_revisions_op(Theory,TheoryRevs):-
  setof(RevOp, Theory^revise_theory(Theory,RevOp), TheoryRevs),!.

theory_revisions_op(_Theory,[]).

theory_revisions(Theory,TheoryRevs):-
  theory_revisions_op(Theory,TheoryRevs1),
  apply_operators(TheoryRevs1,Theory,TheoryRevs).


apply_operators([],_Theory,[]).

apply_operators([add(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  append(Theory, [Rule], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule,NewTheory),
  apply_operators(RestOps,Theory,RestTheory).


revise_theory(Theory,Ref):-
  specialize_theory(Theory,Ref).

revise_theory(Theory,Ref):-
  generalize_theory(Theory,Ref).


generalize_theory(Theory,Ref):-
  Theory \== [],
  choose_rule(Theory,Rule),
  generalize_rule(Rule,Ref).

generalize_theory(Theory,Ref):-
  length(Theory,LT),
  setting(max_rules,MR),
  LT<MR,
  add_rule(Ref).


generalize_rule(Rule,Ref):-
  generalize_head(Rule,Ref).

generalize_rule(Rule,Ref):-
  generalize_body(Rule,Ref).


add_rule(add(rule(ID,Head,[]))):-
  findall(HL , modeh(_,HL), HLS),
  length(HLS,L),
  L1 is L+1,
  P is 1/L1,
  generate_head(HLS,P,Head),
  get_next_rule_number(ID).


generate_head([H|_T],_P,[H1:0.5,'':0.5]):-
  H=..[Pred|Args],
  take_const(Args,Args1),
  H1=..[Pred|Args1].

generate_head([_H|T],P,Head):-
  generate_head(T,P,Head).


take_const([],[]).

take_const([+A|T],[_V|T1]):-
  atom(A),!,
  take_const(T,T1).

take_const([-A|T],[_V|T1]):-
  atom(A),!,
  take_const(T,T1).

take_const([A|T],[A|T1]):-
  take_const(T,T1).


generalize_head(Rule,Ref):-
  Rule = rule(ID,LH,BL),
  generalize_head1(LH,LH1,NewAt),
  Ref = add_head(Rule,rule(ID,LH1,BL),NewAt).


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
  Ref = add_body(Rule,SpecRule,Lit).


specialize_rule(Rule,SpecRule,Lit):-
  findall(BL , modeb(_,BL), BLS),
  specialize_rule(BLS,Rule,SpecRule,Lit).


specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  lookahead(SLit,LLit1),
  specailize_rule_la(LLit1,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL2).

specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL1).

specialize_rule([_|RLit],Rule,SpecRul,Lit):-
  specialize_rule(RLit,Rule,SpecRul,Lit).


specailize_rule_la([],_LH1,BL1,BL1).

specailize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specailize_rule_la(T,LH1,BL2,BL3).


remove_prob(['':_P],[]):-!.

remove_prob([X:_],[X]):-!.

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
  member(Rule,Theory).


add_rule(Theory,add(rule(ID,H,[]))):-
  new_id(ID),
  findall(HL , modeh(_,HL), HLS),
  length(HLS,NH),
  P is 1/(NH+1),
  add_probs(HLS,H,P),
  \+ member(rule(_,H,[]),Theory).

add_rule(Theory,TheoryGen):-
  findall(HL , modeh(_,HL), HLS),
  add_rule(HLS,Theory,TheoryGen).

add_rule([X|_R],Theory,TheoryGen) :-
  new_id(ID),
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  TheoryGen = add(rule(ID,[PH:0.5,'':0.5],[])),
  \+ member(rule(_,[PH:_,'':_],[]),Theory).

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
  
