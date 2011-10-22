/*

RIB

Copyright (c) 2011, Fabrizio Riguzzi and Nicola di Mauro

*/

:- module(inference_ib,
[build_network/5,
get_prob/3,get_CL/3,remove_head/2,build_ground_lpad/2,find_ground_atoms/3,get_atoms/2,
find_atoms_head/3,find_deriv_inf1/2]).

:-load_foreign_files(['cplint'],[],init_my_predicates).

:-multifile setting/2.
:-dynamic rule/4,def_rule/4,setting/2.


:-use_module(library(lists)).

/* start of list of parameters that can be set by the user with
set(Parameter,Value) */
setting(epsilon_parsing,0.00001).
setting(save_dot,false).
setting(ground_body,true). 
/* available values: true, false
if true, both the head and the body of each clause will be grounded, otherwise
only the head is grounded. In the case in which the body contains variables 
not appearing in the head, the body represents an existential event */

/* end of list of parameters */

/* s(GoalsLIst,Prob) compute the probability of a list of goals 
GoalsLis can have variables, s returns in backtracking all the solutions with their
corresponding probability */




get_atoms([],[]):-!.

get_atoms([\+ H|T],T1):-
  builtin(H),!,
  get_atoms(T,T1).

get_atoms([H|T],T1):-
  builtin(H),!,
  get_atoms(T,T1).


get_atoms([\+ H|T],[H1|T1]):-!,
  H=..[P,_|Rest],
  H1=..[P|Rest],
  get_atoms(T,T1).

get_atoms([H|T],[H1|T1]):-
  H=..[P,_|Rest],
  H1=..[P|Rest],
  get_atoms(T,T1).

lookup_gvars([],_AV,[]):-!.  

lookup_gvars([\+ H|T],AV,[HV|T1]):-  !,
  avl_lookup(H,HV,AV),
  lookup_gvars(T,AV,T1).
  
lookup_gvars([H|T],AV,[HV|T1]):-  
  avl_lookup(H,HV,AV),
  lookup_gvars(T,AV,T1).  
  
get_prob(ChoiceVar,Vars,Distr):-  
  clpbn:call_solver([ChoiceVar], Vars),
  clpbn_display:get_atts(ChoiceVar, [posterior(_Vs,_Vals,Distr,_AllDiffs)]).

get_CL(Goal,Vars,CL):-  
  clpbn:call_solver([Goal], Vars),
  clpbn_display:get_atts(Goal, [posterior(_Vs,_Vals,[_P,CL],_AllDiffs)]).


remove_head([],[]).

remove_head([d(R,S)|T],[d(R,S)|T1]):-
  %write(n),
  remove_head(T,T1).

remove_head([(_N,R,S)|T],[(R,S)|T1]):-
  %write(n),
  remove_head(T,T1).

append_all([],L,L):-!.

append_all([LIntH|IntT],IntIn,IntOut):-
    append(IntIn,LIntH,Int1),
    append_all(IntT,Int1,IntOut).

process_goals([],[],[]):-!.

process_goals([H|T],[HG|TG],[HV|TV]):-
  H=..[F,HV|Rest],
  HG=..[F|Rest],
  process_goals(T,TG,TV).

build_ground_lpad([],[]):-!.

build_ground_lpad([(R,S)|T],[(R,S,Head,Body)|T1]):-
  user:rule_by_num(R,S,_,Head,Body),
  build_ground_lpad(T,T1).


add_ev([],_AV):-!.

add_ev([\+ H|T],AV):-!,
  H=..[F,_|R],
  H1=..[F|R],
  avl_lookup(H1,V,AV),
  clpbn:put_atts(V,evidence(0)),
  add_ev(T,AV).

add_ev([H|T],AV):-
  H=..[F,_|R],
  H1=..[F|R],
  avl_lookup(H1,V,AV),
  clpbn:put_atts(V,evidence(1)),
  add_ev(T,AV).


  
lookup_gvars([],_AV,[],S,S):-!.  

lookup_gvars([\+ H|T],AV,[HV|T1],Sign0,Sign2):-  !,
  avl_lookup(H,HV,AV),
  clpbn:get_atts(HV, [key(K)]), 
  avl_insert(K,f,Sign0,Sign1),
  lookup_gvars(T,AV,T1,Sign1,Sign2).
  
lookup_gvars([H|T],AV,[HV|T1],Sign0,Sign2):-  
  avl_lookup(H,HV,AV),
  clpbn:get_atts(HV, [key(K)]), 
  avl_insert(K,t,Sign0,Sign1),
  lookup_gvars(T,AV,T1,Sign1,Sign2).
  

build_table_conj(R,Table):-
  build_col_conj(R,t,f,[],Row1),
  build_col_conj(R,t,t,Row1,Table).

build_col_conj([],Tr,Final,Row0,Row1):-!,
  (Tr=Final->
    append(Row0,[0.999],Row1)
  ;
    append(Row0,[0.001],Row1)
  ).

build_col_conj([\+_H|RP],Tr,Final,Row0,Row2):-!,
  build_col_conj(RP,Tr,Final,Row0,Row1),
  build_col_conj(RP,f,Final,Row1,Row2).

build_col_conj([_H|RP],Tr,Final,Row0,Row2):-
  build_col_conj(RP,f,Final,Row0,Row1),
  build_col_conj(RP,Tr,Final,Row1,Row2).

build_table_atoms(H,R,Table):-
  build_col(H,R,f,f,[],Row1),
  build_col(H,R,t,f,Row1,Table).

build_col(_A,[],Tr,Found,Row0,Row1):-!,
  (Tr=Found->
    append(Row0,[0.999],Row1)
  ;
    append(Row0,[0.001],Row1)
  ).

build_col(A,[(_N,_S,H)|RP],Tr,Found,Row0,Row1):-
  build_col_cycle(A,H,RP,Tr,Found,Row0,Row1).

build_col_cycle(_A,[],_RP,_Tr,_Found,Row,Row).
  
build_col_cycle(A,[A:_P|T],RP,Tr,Found,Row0,Row2):-!,
  build_col(A,RP,Tr,t,Row0,Row1),
  build_col_cycle(A,T,RP,Tr,Found,Row1,Row2).
  
build_col_cycle(A,[_|T],RP,Tr,Found,Row0,Row2):-
  build_col(A,RP,Tr,Found,Row0,Row1),
  build_col_cycle(A,T,RP,Tr,Found,Row1,Row2).

parents([],_CV,[]):-!.

parents([(N,S,_H)|T],CV,[V|T1]):-
  avl_lookup(ch(N,S),V,CV),
  parents(T,CV,T1).

find_rules_with_atom(_A,[],[]):-!.

find_rules_with_atom(A,[(N,S,Head,_Body)|T],[(N,S,Head)|R]):-
  member(A:_P,Head),!,
  find_rules_with_atom(A,T,R).

find_rules_with_atom(A,[_H|T],R):-
  find_rules_with_atom(A,T,R).


build_table([P],L,Row):-!,
  build_col(L,t,P,0.999,Row).

build_table([HP|TP],L,Tab):-
  build_col(L,t,HP,0.001,Row),
  append(Row,Row1,Tab),
  build_table(TP,L,Row1).

build_col([],t,HP,_PNull,[HP]):-!.

build_col([],f,_HP,PNull,[PNull]):-!.


build_col([\+ _H|T],Truth,P,PNull,Row):-!,
  build_col(T,Truth,P,PNull,Row1),
  append(Row1,Row2,Row),
  build_col(T,f,P,PNull,Row2).

build_col([_H|T],Truth,P,PNull,Row):-
  build_col(T,f,P,PNull,Row1),
  append(Row1,Row2,Row),
  build_col(T,Truth,P,PNull,Row2).  
  
get_parents([],_AV,[]):-!.
    
get_parents([\+ H|T],AV,[V|T1]):-!,
  avl_lookup(H,V,AV),
  get_parents(T,AV,T1).

get_parents([H|T],AV,[V|T1]):-!,
  avl_lookup(H,V,AV),
  get_parents(T,AV,T1).

choice_vars([],Tr,Tr,[]):-!.

choice_vars([(N,S,_H,_B)|T],Tr0,Tr1,[NV|T1]):-
  avl_insert(ch(N,S),NV,Tr0,Tr2),
  choice_vars(T,Tr2,Tr1,T1).

atom_vars([],Tr,Tr,[]):-!.

atom_vars([H|T],Tr0,Tr1,[VH|VT]):-
  avl_insert(H,VH,Tr0,Tr2),
  atom_vars(T,Tr2,Tr1,VT).

find_ground_atoms([],GA,GA):-!.

find_ground_atoms([(_N,_S,Head,_Body)|T],GA0,GA1):-
  find_atoms_head(Head,AtH,_P),
  append(GA0,AtH,GA3),
  find_ground_atoms(T,GA3,GA1).

find_atoms_body([],[]):-!.

find_atoms_body([\+H|T],[H|T1]):-!,
  find_atoms_body(T,T1).

find_atoms_body([H|T],[H|T1]):-
  find_atoms_body(T,T1).

find_atoms_head([],[],[]).

find_atoms_head(['':P],[''],[P]):-!.

find_atoms_head([H:P|T],[H1|TA],[P|TP]):-
  H=..[F,_|R],
  H1=..[F|R],
  find_atoms_head(T,TA,TP).

find_deriv_inf1(GoalsList,DB,Deriv):-
  solve1(GoalsList,DB,[],Deriv).

find_deriv_inf(GoalsList,Deriv):-
  solve(GoalsList,[],Deriv).

find_deriv_inf(GoalsList,DB,Deriv):-
  solve(GoalsList,DB,[],Deriv).
/* duplicate can appear in the C set because two different unistantiated clauses may become the 
same clause when instantiated */

solve1([],_DB,C,C):-!.

solve1([\+ H |T],DB,CIn,COut):-!,
  DB > 0,
  DB1 is DB-1,
  list2and(HL,H),
  (setof(D,find_deriv_inf1(HL,DB1,D),L)->
    choose_clauses(CIn,L,C1),  
    solve(T,DB1,C1,COut)
  ;
    solve(T,DB1,CIn,COut)
  ).
  

solve1([H|T],DB,CIn,COut):-
  DB>0,
  DB1 is DB-1,
  user:def_rule(H,B,N,S),
  append(B,T,NG),
  append(CIn,[d(N,S)],CIn1),
  solve(NG,DB1,CIn1,COut).

solve1([H|T],DB,CIn,COut):-
  DB>0,
  DB1 is DB-1,
  find_rule(H,(R,S,N),B,CIn),
  solve_pres(R,S,N,B,T,DB1,CIn,COut).


solve([],_DB,C,C):-!.

solve([\+ H|T],DB,CIn,COut):-
  builtin(H),!,
  call(\+ H),
  solve(T,DB,CIn,COut).

solve([\+ H|T],DB,CIn,COut):-
  functor(H,F,Args),
  Args1 is Args-1,
  user:input_cw(F/Args1),!,
  call(user:neg(H)),
  solve(T,DB,CIn,COut).
  
solve([\+ H|T],DB,CIn,COut):-
  functor(H,F,Args),
  Args1 is Args-1,
  user:input(F/Args1),
  call(user:neg(H)),
  solve(T,DB,CIn,COut).


solve([\+ H |T],DB,CIn,COut):-!,
  DB > 0,
  DB1 is DB-1,
  list2and(HL,H),
  functor(H,F,Args),
  Args1 is Args-1,
  (user:input(F/Args1)->
    call(user:neg(H))
  ;
    true
  ),
  (setof(D,find_deriv_inf(HL,DB1,D),L)->
    choose_clauses(CIn,L,C1),  
    solve(T,DB1,C1,COut)
  ;
    solve(T,DB1,CIn,COut)
  ).
  
solve([H|T],DB,CIn,COut):-
  builtin(H),!,
  call(H),
  solve(T,DB,CIn,COut).

solve([H|T],DB,CIn,COut):-
  user:db(H),!,
  call(user:H),
  solve(T,DB,CIn,COut).

solve([H|T],DB,CIn,COut):-
  DB>0,
  DB1 is DB-1,
  user:def_rule(H,B,N,S),
  append(B,T,NG),
  append(CIn,[d(N,S)],CIn1),
  solve(NG,DB1,CIn1,COut).

solve([H|T],DB,CIn,COut):-
  functor(H,F,Args),
  Args1 is Args-1,
  user:input_cw(F/Args1),!,
  call(user:H),
  solve(T,DB,CIn,COut).
  
solve([H|T],DB,CIn,COut):-
  functor(H,F,Args),
  Args1 is Args-1,
  user:input(F/Args1),
  call(user:H),
  solve(T,DB,CIn,COut).

solve([H|T],DB,CIn,COut):-
  DB>0,
  DB1 is DB-1,
  find_rule(H,(R,S,N),B,CIn),
  solve_pres(R,S,N,B,T,DB1,CIn,COut).

solve_pres(R,S,N,B,T,DB,CIn,COut):-
  member_eq((N,R,S),CIn),!,
  append(B,T,NG),
  solve(NG,DB,CIn,COut).
  
solve_pres(R,S,N,B,T,DB,CIn,COut):-
  append(CIn,[(N,R,S)],C1),
  append(B,T,NG),
  solve(NG,DB,C1,COut).


not_present_with_a_different_head(_N,_R,_S,[]).

not_present_with_a_different_head(N,R,S,[(N,R,S)|T]):-!,
  not_present_with_a_different_head(N,R,S,T).

not_present_with_a_different_head(N,R,S,[(_N1,R,S1)|T]):-
  S\=S1,!,
  not_present_with_a_different_head(N,R,S,T).

not_present_with_a_different_head(N,R,S,[(_N1,R1,_S1)|T]):-
  R\=R1,
  not_present_with_a_different_head(N,R,S,T).
    

  
/* find_rule(G,(R,S,N),Body,C) takes a goal G and the current C set and
returns the index R of a disjunctive rule resolving with G together with
the index N of the resolving head, the substitution S and the Body of the 
rule */
find_rule(H,(R,S,N),Body,C):-
  user:rule(H,_P,N,R,S,_,_Head,Body),
  not_already_present_with_a_different_head(N,R,S,C).

not_already_present_with_a_different_head(_N,_R,_S,[]).

not_already_present_with_a_different_head(N,R,S,[d(_R,_S1)|T]):-!,
  not_already_present_with_a_different_head(N,R,S,T).

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
  user:rule_by_num(R,S,Numbers,Head,_Body),
  Head\=uniform(_,_,_),!,
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

list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).

list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).

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
%  reverse(D,D1),
  D=D1,
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
  user:rule_by_num(R,S,_N,Head,_Body),
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

/* end of predicates for building the formula to be converted into a BDD */

   
