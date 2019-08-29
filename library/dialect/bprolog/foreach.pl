%   File   : foreach.pl
%   Author : Neng-Fa Zhou
%   Updated: June 2009, updated Dec. 2009, updated Sep. 2010
%   Purpose: an interpreter of foreach/2-10 and list comprehension
/************************************************************************/

:- yap_flag(unknown,error).
:- ensure_loaded(actionrules).
:- op(560,xfy,[..,to,downto]).
:- op(700,xfx,[subset,notin,in,@=]).

:- use_module(library(lists)).

/*
test:-
    L=[1,2,3],foreach(I in L, writeln(I)),fail.
test:-
    foreach(I in 1..10,format("~d ",I)),fail.
test:-
    foreach(I in 1..2..10,format("~d ",I)),fail.   % step = 2
test:-
    foreach(I in 10.. -1.. 1,format("~d ",I)),fail. % step = -1
test:-
    foreach((A,N) in ([a,b],1..2),writeln(A=N)),fail.
test:-
    L=[1,2,3],foreach(I in L, ac(S,0), S^1 is S^0+I),writeln(S),fail.
test:-
    T=f(1,2,3),functor(T,_,N),foreach(I in 1..N,ac(S,0),(S^1 is S^0+T[I])),writeln(S),fail.
test:-
    L=[1,2,3],foreach(I in L, ac1(C,[]), C^0=[I|C^1]),writeln(C),fail.
test:-
    foreach(I in [1,2], J in [a,b], ac(L,[]),L^1=[(I,J)|L^0]),writeln(L),fail.
test:-
    foreach(I in [1,2], J in [a,b], ac1(L,[]),L^0=[(I,J)|L^1]),writeln(L),fail.
test:-
    foreach(T in ([a,b],1..2),writeln(T)),fail.
test:-
    foreach(F in 1.0..0.2..1.5,format("~1f ",F)),fail.
test:-
    L @= [I : I in 1..10],writeln(L),fail.
test:-
    L @= [I : I in 1..2..10],writeln(L),fail.
test:-
    L @= [I : I in 10..-1..1],writeln(L),fail.
test:-
    L @=[X : X in 1..5],writeln(L),fail.
test:-
    L @= [1 : X in 1..5],writeln(L),fail.
test:-
    L @= [Y : X in 1..5],writeln(L),fail.
test:-
    L @= [Y : X in 1..5,[Y]],writeln(L),fail.
test:-
    L @=[(A,I): (A,I) in ([a,b],1..2)],writeln(L),fail.
test:-
    L @= [Y : X in [1,2,3], [Y], Y is -X],writeln(L),fail.
test:-
    L @=[(A,I): A in [a,b], I in 1..2],writeln(L),fail.
test:-
    L @=[(A,I): (A,I) in ([a,b],1..2)],writeln(L),fail.
test.
*/

Lhs @= Rhs,
    Rhs=[(T:I)|Is],
    I=(_ in _) =>  % list comprehension
    '$change_list_comprehension_to_foreach'(T,I,Is,CallForeach,L),
    call(CallForeach),
    L @= Lhs.
Lhs @= Rhs,
    Lhs=[(T:I)|Is],
    I=(_ in _) =>  % list comprehension
    '$change_list_comprehension_to_foreach'(T,I,Is,CallForeach,L),
    call(CallForeach),
    L @= Rhs.
A^Indexes @= Exp =>       % array access
    '$aget'(A,Indexes,T),   
    Exp @= T.
Exp @= A^Indexes =>       % array access
    '$aget'(A,Indexes,T),
    Exp @= T.
Lhs @= Rhs  => Lhs=Rhs.

'$change_list_comprehension_to_foreach'(T,I,Is,CallForeach,L):-
    '$retrieve_list_comp_lvars_goal'(Is,LocalVars1,Goal1,Is1),
    (nonvar(T),T=_^_->  % array access
        LocalVars=[TempVar|LocalVars1],
        (Goal1==true->
           Goal=(TempVar@=T,L^0=[TempVar|L^1])
         ;
           Goal=(Goal1->(TempVar@=T,L^0=[TempVar|L^1]);L^0=L^1)
        )
     ;
        LocalVars=LocalVars1,
        (Goal1==true->
            Goal=(L^0=[T|L^1])
         ;
            Goal=(Goal1->L^0=[T|L^1];L^0=L^1)
        )
    ),
    append(Is1,[LocalVars,ac1(L,[]),Goal],Is2),
    CallForeach=..[foreach,I|Is2].

'$retrieve_list_comp_lvars_goal'([],LocalVars,Goal,Is) =>
     LocalVars=[],Goal=true,Is=[].
'$retrieve_list_comp_lvars_goal'([E|Es],LocalVars,Goal,Is),E = (_ in _) =>
    Is=[E|IsR],
    '$retrieve_list_comp_lvars_goal'(Es,LocalVars,Goal,IsR).
'$retrieve_list_comp_lvars_goal'([LVars,G],LocalVars,Goal,Is),LVars=[] =>
    Is=[],LocalVars=LVars,G=Goal.
'$retrieve_list_comp_lvars_goal'([LVars,G],LocalVars,Goal,Is),LVars=[_|_] =>
    Is=[],LocalVars=LVars,G=Goal.
'$retrieve_list_comp_lvars_goal'([LVars],LocalVars,Goal,Is),LVars=[_|_] =>
    Is=[],LocalVars=LVars,Goal=true.
'$retrieve_list_comp_lvars_goal'([LVars],LocalVars,Goal,Is),LVars=[] =>
    Is=[],LocalVars=LVars,Goal=true.
'$retrieve_list_comp_lvars_goal'([G],LocalVars,Goal,Is) =>
    Is=[],LocalVars=[],G=Goal.
'$retrieve_list_comp_lvars_goal'(Args,_LocalVars,_Goal,_Is) =>
    throw(illegal_arguments(list_comprehension(Args))).

foreach(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10):-
    foreach_aux((A1,A2,A3,A4,A5,A6,A7),A8,A9,A10).

foreach(A1,A2,A3,A4,A5,A6,A7,A8,A9):-
    foreach_aux((A1,A2,A3,A4,A5,A6),A7,A8,A9).

foreach(A1,A2,A3,A4,A5,A6,A7,A8):-
    foreach_aux((A1,A2,A3,A4,A5),A6,A7,A8).

foreach(A1,A2,A3,A4,A5,A6,A7):-
    foreach_aux((A1,A2,A3,A4),A5,A6,A7).

foreach(A1,A2,A3,A4,A5,A6):-
    foreach_aux((A1,A2,A3),A4,A5,A6).

foreach(A1,A2,A3,A4,A5):-
    foreach_aux((A1,A2),A3,A4,A5).

/** @pred foreach( _Sequence_,  _Goal_,  _Acc0_,  _AccF_)

Deterministic iterator with accumulator style arguments.

 
*/
foreach(A1,A2,A3,A4):-
    foreach_aux(A1,A2,A3,A4).

foreach_aux(A1,A2,A3,A4):-
    (A2=(_ in _); A2=(_,_)),!, % iterator
    foreach_aux((A1,A2),A3,A4).
foreach_aux(A1,A2,A3,A4):-
    foreach_check_accumulators(A3),!, 
    interp_foreach_with_acs(A1,A2,A3,A4).
foreach_aux(A1,A2,A3,A4):-
    foreach_check_accumulators(A2),!, 
    interp_foreach_with_acs(A1,A3,A2,A4).
foreach_aux(A1,A2,A3,A4):-
    throw(illegal_arguments(foreach(A1,A2,A3,A4))).
    
foreach(A1,A2,A3):-
    foreach_aux(A1,A2,A3).

foreach_aux(A1,A2,A3):-
    (A2=(_ in _); A2=(_,_)),!,
    interp_foreach((A1,A2),true,[],A3,[],[],_).
foreach_aux(A1,A2,A3):-
    foreach_check_accumulators(A2),!, 
    interp_foreach_with_acs(A1,[],A2,A3).
foreach_aux(A1,A2,A3):-
    foreach_check_lvars(A2),!, 
    interp_foreach(A1,true,A2,A3,[],[],_).

/** @pred foreach( _Sequence_,  _Goal_) 


Deterministic iterator. The ranges are given by  _Sequence_ that is
either ` _I_ in  _M_.. _N_`, or of the form 
`[ _I_, _J_] ins  _M_.. _N_`, or a list of the above conditions. 

Variables in the goal are assumed to be global, ie, share a single value
in the execution. The exceptions are the iteration indices. Moreover, if
the goal is of the form ` _Locals_^ _G_` all variables
occurring in  _Locals_ are marked as local. As an example:

~~~~~
foreach([I,J] ins 1..N, A^(A <==M[I,J], N[I] <== N[I] + A*A) )
~~~~~
the variables  _I_,  _J_ and  _A_ are duplicated for every
call (local), whereas the matrices  _M_ and  _N_ are shared
throughout the execution (global).

 
*/
/** @pred foreach(:Generator, : _Goal_) 


True if the conjunction of instances of  _Goal_ using the
bindings from Generator is true. Unlike forall/2, which runs a
failure-driven loop that proves  _Goal_ for each solution of
Generator, foreach creates a conjunction. Each member of the
conjunction is a copy of  _Goal_, where the variables it shares
with Generator are filled with the values from the corresponding
solution.

The implementation executes forall/2 if  _Goal_ does not contain
any variables that are not shared with Generator.

Here is an example:

~~~~~
    ?- foreach( between(1,4,X), dif(X,Y)), Y = 5.
    Y = 5
    ?- foreach( between(1,4,X), dif(X,Y)), Y = 3.
    No
~~~~~

Notice that  _Goal_ is copied repeatedly, which may cause
problems if attributed variables are involved.

 
*/
foreach(Iterators,Goal):-
    interp_foreach(Iterators,true,[],Goal,[],[],_).

interp_foreach_with_acs(Iterators,LVars,Accumulators,Goal):-
    init_accumulators(Accumulators,ACs0),!,
    interp_foreach(Iterators,true,LVars,Goal,[],ACs0,ACs),
    fin_accumulators(Accumulators,ACs0,ACs).
interp_foreach_with_acs(Iterators,LVars,Accumulators,Goal):-
    throw(illegal_arguments(foreach(Iterators,LVars,Accumulators,Goal))).

interp_foreach((I,Is),IsRest,LVars,Goal,Map,ACs0,ACs):-!,
    (IsRest==true->IsRest1=Is;IsRest1=(Is,IsRest)),
     interp_foreach(I,IsRest1,LVars,Goal,Map,ACs0,ACs).
interp_foreach(Pattern in D,IsRest,LVars,Goal,Map,ACs0,ACs):-
    interp_foreach_term_instance(D,D1,Map),
    (var(D1)->handle_exception(instantiation_error,foreach);true),
    interp_foreach_in(Pattern,D1,IsRest,LVars,Goal,Map,ACs0,ACs).
interp_foreach(true,true,LVars,Goal,Map,ACs0,ACs):-!,
    foreach_copy_accumulators(ACs0,ACs),
    interp_foreach_term_instance(Goal,Goal1,LVars,Map,_,ACs0,ACs),
    call(Goal1).
interp_foreach(true,Is,LVars,Goal,Map,ACs0,ACs):-
    interp_foreach(Is,true,LVars,Goal,Map,ACs0,ACs).

interp_foreach_in(Var,(L..Step..U),IsRest,LVars,Goal,Map,ACs0,ACs) =>
    (var(Var)->true;throw(wrong_loop_variable(Var))),
    (foreach_lookup_map(Var,_,Map)->throw(duplicate_loop_variable(Var));true),
    L1 is L,
    U1 is U,
    Step1 is Step,
    foreach_range(Var,L1,U1,Step1,IsRest,LVars,Goal,Map,ACs0,ACs).
interp_foreach_in(Var,L..U,IsRest,LVars,Goal,Map,ACs0,ACs) =>
    (var(Var)->true;throw(wrong_loop_variable(Var))),
    (foreach_lookup_map(Var,_,Map)->throw(duplicate_loop_variable(Var));true),
    L1 is L,
    U1 is U,
    foreach_range(Var,L1,U1,1,IsRest,LVars,Goal,Map,ACs0,ACs).
interp_foreach_in(_,[],IsRest,LVars,Goal,Map,ACs0,ACs) => 
    ACs=ACs0.
interp_foreach_in(E,D,IsRest,LVars,Goal,Map,ACs0,ACs):-true :::
    term_variables(E,EVars),
    (member(Var,EVars),foreach_lookup_map(Var,_,Map),!,throw(duplicate_loop_variable(Var));true),
    foreach_pattern_in(E,D,IsRest,LVars,Goal,Map,ACs0,ACs).

foreach_range(_Var,L,U,Step,_IsRest,_LVars,_Goal,_Map,ACs0,ACs),Step>0,L>U =>
    ACs0=ACs.
foreach_range(_Var,L,U,Step,_IsRest,_LVars,_Goal,_Map,ACs0,ACs),Step<0,L<U =>
    ACs0=ACs.
foreach_range(Var,L,U,Step,IsRest,LVars,Goal,Map,ACs0,ACs) =>
    interp_foreach(IsRest,true,LVars,Goal,[(Var,L)|Map],ACs0,ACs1),
    L1 is L+Step,
    foreach_range(Var,L1,U,Step,IsRest,LVars,Goal,Map,ACs1,ACs).
    
foreach_pattern_in(_Pattern,D,_IsRest,_LVars,_Goal,_Map,_ACs0,_ACs),var(D)  =>
    handle_exception(instantiation_error,foreach).
foreach_pattern_in(Pattern,D,IsRest,LVars,Goal,Map,ACs0,ACs),D=[_|_]  =>
    foreach_pattern_in_list(Pattern,D,IsRest,LVars,Goal,Map,ACs0,ACs).
foreach_pattern_in(Pattern,D,IsRest,LVars,Goal,Map,ACs0,ACs) =>
    foreach_simu_collection_to_tuples(D,Tuples),
    foreach_pattern_in_list(Pattern,Tuples,IsRest,LVars,Goal,Map,ACs0,ACs).

foreach_pattern_in_list(_Pattern,Lst,_IsRest,_LVars,_Goal,_Map,_ACs0,_ACs),var(Lst) =>
    handle_exception(instantiation_error,foreach).
foreach_pattern_in_list(_Pattern,[],_IsRest,_LVars,_Goal,_Map,ACs0,ACs) =>
    ACs0=ACs.
foreach_pattern_in_list(Pattern,[E|Es],IsRest,LVars,Goal,Map,ACs0,ACs) =>
    (foreach_update_map(Pattern,E,Map,Map1)->
     interp_foreach(IsRest,true,LVars,Goal,Map1,ACs0,ACs1)
     ;
     ACs0=ACs1),
    foreach_pattern_in_list(Pattern,Es,IsRest,LVars,Goal,Map,ACs1,ACs).
foreach_pattern_in_list(_Pattern,Lst,_IsRest,_LVars,_Goal,_Map,_ACs0,_ACs):-true :::
    handle_exception(type_error(list,Lst),foreach).

foreach_update_map(Var,E,Map0,Map):-var(Var),!,Map=[(Var,E)|Map0].
foreach_update_map(Pattern,E,Map0,Map):-atomic(Pattern),!,E==Pattern,Map=Map0.
foreach_update_map(Pattern,E,Map0,Map):-nonvar(E),
    functor(Pattern,F,N),
    functor(E,F,N),
    foreach_update_map(Pattern,E,Map0,Map,1,N).

foreach_update_map(_Pattern,_E,Map0,Map,I,N):-I>N,!,Map=Map0.
foreach_update_map(Pattern,E,Map0,Map,I,N):-
    arg(I,Pattern,Ti),
    arg(I,E,Ei),
    foreach_update_map(Ti,Ei,Map0,Map1),
    I1 is I+1,
    foreach_update_map(Pattern,E,Map1,Map,I1,N).

interp_foreach_term_instance(Term,Term1,Map):-
   interp_foreach_term_instance(Term,Term1,[],Map,_,[],[]).

% Replace loop variables with their values; rename local variables;
% replace inputs and outputs in recurrences: A^0 is input and A^1 is output.
interp_foreach_term_instance(Term,NTerm,LVars,Map,NMap,_ACs0,_ACs):-var(Term),!,
   (foreach_lookup_map(Term,NTerm,Map)->NMap=Map;
    membchk(Term,LVars)->NMap=[(Term,NTerm)|Map];
    NTerm=Term,NMap=Map).
interp_foreach_term_instance(Term,NTerm,_LVars,Map,NMap,_ACs0,_ACs):-atomic(Term),!,
    NTerm=Term,NMap=Map.
interp_foreach_term_instance(Term^Tail,NTerm,_LVars,Map,NMap,ACs0,_ACs):-
    var(Term),Tail==0,
    foreach_lookup_map(Term,NTerm,ACs0),!,
    NMap=Map.
interp_foreach_term_instance(Term^Tail,NTerm,_LVars,Map,NMap,_ACs0,ACs):-
    var(Term),Tail==1,
    foreach_lookup_map(Term,NTerm,ACs),!,
    NMap=Map.
interp_foreach_term_instance([E|Es],Lst,LVars,Map,NMap,ACs0,ACs):-!,
    Lst=[E1|Es1],
    interp_foreach_term_instance(E,E1,LVars,Map,Map1,ACs0,ACs),
    interp_foreach_term_instance(Es,Es1,LVars,Map1,NMap,ACs0,ACs).
interp_foreach_term_instance(Term,NTerm,_LVars,Map,NMap,_ACs0,_ACs):-
    is_array(Term),!,
    NTerm=Term,NMap=Map.
interp_foreach_term_instance(Term,NTerm,_LVars,Map,NMap,_ACs0,_ACs):-
    is_hashtable(Term),!,
    NTerm=Term,NMap=Map.
interp_foreach_term_instance(Term,NTerm,LVars,Map,NMap,ACs0,ACs):-
    functor(Term,F,N),
    functor(NTerm,F,N),
    interp_foreach_term_instance(Term,NTerm,LVars,Map,NMap,1,N,ACs0,ACs).

interp_foreach_term_instance(_Term,_NTerm,_LVars,Map,NMap,I,N,_,_):-I>N,!,
    NMap=Map.
interp_foreach_term_instance(Term,NTerm,LVars,Map,NMap,I,N,ACs0,ACs):-
    arg(I,Term,A),
    arg(I,NTerm,NA),
    interp_foreach_term_instance(A,NA,LVars,Map,Map1,ACs0,ACs),
    I1 is I+1,
    interp_foreach_term_instance(Term,NTerm,LVars,Map1,NMap,I1,N,ACs0,ACs).
     
init_accumulators(ac1(Name,_),ACs):-!, ACs=[(Name,_)].
init_accumulators(ac(Name,Init),ACs):-!, ACs=[(Name,Init)].
init_accumulators(Accumulators,ACs):-Accumulators=[_|_],
    init_accumulator_lst(Accumulators,ACs).

init_accumulator_lst([],ACs):-!,ACs=[].
init_accumulator_lst([ac1(Name,_)|Accumulators],ACs):-!,
    ACs=[(Name,_)|ACsR],
    init_accumulator_lst(Accumulators,ACsR).
init_accumulator_lst([ac(Name,Init)|Accumulators],ACs):-
    ACs=[(Name,Init)|ACsR],
    init_accumulator_lst(Accumulators,ACsR).

fin_accumulators(ac1(Name,Fin),[(_,Init)],[(_,Val)]):-!,
    Name=Init,Fin=Val.
fin_accumulators(ac(Name,_),_,[(_,Val)]):-!, Name=Val.
fin_accumulators(Accumulators,ACs0,ACs):-Accumulators=[_|_],
    fin_accumulator_lst(Accumulators,ACs0,ACs).

fin_accumulator_lst([],_,_).
fin_accumulator_lst([ac1(Name,Fin)|Accumulators],[(_,Init)|ACs0],[(_,Val)|ACs]):-!,
    Fin=Val,
    Name=Init,
    fin_accumulator_lst(Accumulators,ACs0,ACs).
fin_accumulator_lst([ac(Name,_)|Accumulators],[_|ACs0],[(_,Val)|ACs]):-
    Name=Val,
    fin_accumulator_lst(Accumulators,ACs0,ACs).

foreach_copy_accumulators([],ACs):-!, ACs=[].
foreach_copy_accumulators([(Name,_)|ACs0],ACs):-
    ACs=[(Name,_)|ACs1],
    foreach_copy_accumulators(ACs0,ACs1).

foreach_check_lvars([]):-true ::: true.
foreach_check_lvars([X|Xs]):- var(X) ::: foreach_check_lvars(Xs).
foreach_check_lvars(Xs):-true :::
    throw(illegal_local_variables(Xs)).

foreach_check_accumulators(ac1(_,_)):-!.
foreach_check_accumulators(ac(_,_)):-!.
foreach_check_accumulators(Accumulators):-Accumulators=[_|_],
    foreach_check_accumulator_lst(Accumulators).

foreach_check_accumulator_lst([]).
foreach_check_accumulator_lst([X|_]):-var(X),!,fail.
foreach_check_accumulator_lst([ac(_,_)|L]):-!,
    foreach_check_accumulator_lst(L).
foreach_check_accumulator_lst([ac1(_,_)|L]):-
    foreach_check_accumulator_lst(L).

foreach_lookup_map(Term,NTerm,[(Term1,NTerm1)|_]):-Term==Term1,!,
    NTerm=NTerm1.
foreach_lookup_map(Term,NTerm,[_|Map]):-
    foreach_lookup_map(Term,NTerm,Map).
	   
foreach_simu_collection_to_tuples((C1,C2,C3),Tuples) ?=>
    foreach_collection_to_lst(C1,L1),
    foreach_collection_to_lst(C2,L2),
    foreach_collection_to_lst(C3,L3),!,
    (foreach_simu_collection_to_tuples3(L1,L2,L3,Tuples)->true;
     handle_exception(wrong_collection_in_foreach,(C1,C2,C3))).
foreach_simu_collection_to_tuples((C1,C2),Tuples) ?=>
    foreach_collection_to_lst(C1,L1),
    foreach_collection_to_lst(C2,L2),!,
    (foreach_simu_collection_to_tuples2(L1,L2,Tuples)->true;
     handle_exception(wrong_collection_in_foreach,(C1,C2))).
foreach_simu_collection_to_tuples(CTuple,_) =>
    handle_exception(wrong_collection_in_foreach,CTuple).

foreach_collection_to_lst([],L) => L=[].
foreach_collection_to_lst(C,L),C=[_|_] => L=C.
foreach_collection_to_lst((B1..Step..B2),L) =>
    NB1 is B1,
    NB2 is B2,
    NStep is Step,
    foreach_eval_range(NB1,NB2,NStep,L).
foreach_collection_to_lst((B1..B2),L) =>    
    NB1 is B1,
    NB2 is B2,
    foreach_eval_range(NB1,NB2,1,L).
foreach_collection_to_lst(CTuple,L),CTuple=(_,_) =>    
    foreach_simu_collection_to_tuples(CTuple,L).
foreach_collection_to_lst(Collection,_L) => 
    handle_exception(wrong_collection_in_foreach,Collection).    

foreach_eval_range(B1,B2,Step,L),Step>0,B1>B2 => L=[].
foreach_eval_range(B1,B2,Step,L),Step<0,B1<B2 => L=[].
foreach_eval_range(B1,B2,Step,L) => L=[B1|LR],
    NB1 is B1+Step,
    foreach_eval_range(NB1,B2,Step,LR).

foreach_simu_collection_to_tuples3([],[],[],Tuples) => Tuples=[].
foreach_simu_collection_to_tuples3([X1|L1],[X2|L2],[X3|L3],Tuples) => 
    Tuples=[(X1,X2,X3)|TuplesR],
    foreach_simu_collection_to_tuples3(L1,L2,L3,TuplesR).

foreach_simu_collection_to_tuples2([],[],Tuples) => Tuples=[].
foreach_simu_collection_to_tuples2([X1|L1],[X2|L2],Tuples) => 
    Tuples=[(X1,X2)|TuplesR],
    foreach_simu_collection_to_tuples2(L1,L2,TuplesR).
