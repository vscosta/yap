/*

CEM

Copyright (c) 2011, Fabrizio Riguzzi

*/

:- module(inference,[find_deriv_inf1/3]).


:-multifile setting/2.
:-dynamic setting/2.

%:-load_foreign_files(['cplint'],[],init_my_predicates).


:-use_module(library(lists)).
:-use_module(library(avl)).

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


find_deriv_inf1(GoalsList,DB,Deriv):-
        solve1(GoalsList,DB,[],Deriv).

find_deriv_inf(GoalsList,D,Deriv):-
  solve(GoalsList,D,[],Deriv).
/* duplicate can appear in the C set because two different unistantiated clauses may become the 
same clause when instantiated */

solve([],_D,C,C):-!.

solve([\+ H|T],DB,CIn,COut):-
        functor(H,F,Args),
  Args1 is Args-1,
  user:input_cw(F/Args1),!,
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
         \+ H 
        ),
        (setof(D,find_deriv_inf(HL,DB1,D),L)->
                choose_clauses(CIn,L,C1),
                solve(T,DB1,C1,COut)
        ;
                solve(T,DB1,CIn,COut)
        ).

  
solve([H|T],D,CIn,COut):-
  builtin(H),!,
  call(H),
  solve(T,D,CIn,COut).

solve([H|T],DB,CIn,COut):-
        user:db(H),!,
        call(user:H),
        solve(T,DB,CIn,COut).

solve([H|T],DB,CIn,COut):-
        functor(H,F,Args),
        Args1 is Args-1,
        user:input_cw(F/Args1),!,
        call(user:H),
        solve(T,DB,CIn,COut).

solve([H|T],D,CIn,COut):-
  functor(H,F,Args),
  Args1 is Args-1,
  user:input(F/Args1),
  call(user:H),
  solve(T,D,CIn,COut).

solve([H|T],D,CIn,COut):-
  user:def_rule(H,B),
  append(B,T,NG),
  solve(NG,D,CIn,COut).

solve([H|T],D,CIn,COut):-
  D>=1,
  find_rule(H,(R,S,N),B,CIn),
  solve_pres(R,S,N,B,T,D,CIn,COut).

solve_pres(R,S,N,B,T,D,CIn,COut):-
  member_eq((N,R,S),CIn),!,
  append(B,T,NG),
  D1 is D-1,
  solve(NG,D1,CIn,COut).
  
solve_pres(R,S,N,B,T,D,CIn,COut):-
  append(CIn,[(N,R,S)],C1),
  append(B,T,NG),
  D1 is D-1,
  solve(NG,D1,C1,COut).


solve1([],_D,C,C):-!.

solve1([\+ H |T],DB,CIn,COut):-!,
  DB>=1,
  list2and(HL,H),
  findall(D,find_deriv_inf1(HL,DB,D),L),
    choose_clauses(CIn,L,C1),  
    solve(T,C1,COut).
  
solve1([H|T],D,CIn,COut):-
  user:def_rule(H,B),
  append(B,T,NG),
  solve(NG,D,CIn,COut).

solve1([H|T],D,CIn,COut):-
  D>=1,
  find_rule(H,(R,S,N),B,CIn),
  solve_pres(R,S,N,B,T,D,CIn,COut).


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
    
