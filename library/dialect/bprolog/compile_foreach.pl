%   File   : compile_foreach.pl
%   Author : Neng-Fa Zhou
%   Updated: June 2009, updated Dec. 2009, updated Sep. 2010
%   Purpose: compile away foreach
/*  compile_foreach(Cls,NCls): NCls is a list of clauses obtained by
    compiling away foreach calls in Cls. The new predicate introduced
    for a foreach is named p_#_i where p is the name of the predicate
    in which the foreach occurs and i is a unique integer.
*/

:- yap_flag(unknown,error).
:- ensure_loaded(actionrules).
:- use_module(library(lists)).
:- op(1200,fy,[delay]).
:- op(1150,xfy,[?]).
:- op(560,xfy,[..,to,downto]).
:- op(700,xfx,[subset,notin,is,in,\==,\=,@>=,@>,@=<,@=,@<,@:=,?=,>=,>,
				  =\=,==,=<,=:=,=..,=,<=,<,:=,$>=,$=<,$=,#\=,#>=,#>,#=<,
		 #=,#<\-,#<>,#<-,#<,#:=,##]).
/*
test:-
    Cl1=(test1(L):-foreach(I in L, write(I))),
    Cl2=(test2(L):-foreach(I in L, ac(S,0), S^1 is S^0+I)),
    Cl3=(test3(T):-functor(T,_,N),foreach(I in 1..N, [Ti],ac(S,0), (arg(I,T,Ti),S^1 is S^0+Ti))),
    Cl4=(test4(L):-foreach(I in L, ac1(C,[]), C^0=[I|C^1])),
    Cl5=(test5:-foreach(I in [1,2], J in [a,b], ac(L,[]),L^1=[(I,J)|L^0]),writeln(L),fail),
    Cl6=(test6:-foreach(I in [1,2], J in [a,b], ac1(L,[]),L^0=[(I,J)|L^1]),writeln(L),fail),
    Cl7=(test7(L1,L2):-foreach(X in L1, (write(X),foreach(Y in L2, writeln((X,Y)))))),
    Cl8=(p(D1,D3,IN,OUT):-
	 foreach(E in D3,
		 [INi,OUTi],
		 (asp_lib_clone_rel(IN,OUT,INi,OUTi),
		  (foreach(X in D1, Y in D1,(not diagY(X,Y,E)->asp_lib_add_tuples(OUTi,X,Y);true)),
		   asp_lib_card_unique(2,INi,OUTi))))),
    compile_foreach([Cl1,Cl2,Cl3,Cl4,Cl5,Cl6,Cl7,Cl8],NCls),   	 
    (member(NCl,NCls), portray_clause(NCl),fail;true).
*/
compile_foreach(File):-
    '$getclauses_read_file'(File,'$t.t.t$',0,_Singleton,_Redef,Cls,[]),
    compile_foreach(Cls,NCls),
    foreach(NCl in NCls, portray_clause(NCl)).

compile_foreach(Cls,NCls):-
    new_hashtable(ProgTab),
    compile_foreach(Cls,NCls,NCls1,ProgTab,0),
    hashtable_values_to_list(ProgTab,Prog),
    retrieve_new_cls(Prog,NCls1).

retrieve_new_cls([],[]).
retrieve_new_cls([pred(_,_,_,_,_,Cls)|Preds],NCls):-
    append_diff(Cls,NCls,NCls1),
    retrieve_new_cls(Preds,NCls1).

compile_foreach([],NCls,NClsR,_ProgTab,_DumNo) => NCls=NClsR.
compile_foreach([Cl|Cls],NCls,NClsR,ProgTab,DumNo) =>
    NCls=[NCl|NCls1],
    expand_constr(Cl,NCl,ProgTab,DumNo,DumNo1),
    compile_foreach(Cls,NCls1,NClsR,ProgTab,DumNo1).

cl_contains_foreach((delay (_H:-(_G : B)))) =>
    goal_contains_foreach(B,Flag),nonvar(Flag).
cl_contains_foreach((_H:-_G : B)) =>
    goal_contains_foreach(B,Flag),nonvar(Flag).
cl_contains_foreach((_H:-_G ? B)) => 
    goal_contains_foreach(B,Flag),nonvar(Flag).    
cl_contains_foreach((_H:-B)) => 
    goal_contains_foreach(B,Flag),nonvar(Flag).    

goal_contains_foreach(G):-
    goal_contains_foreach(G,Flag),
    nonvar(Flag).

goal_contains_foreach(_G,Flag), nonvar(Flag) => true.
goal_contains_foreach(G,_Flag), var(G) => true.
goal_contains_foreach((_G : B),Flag) =>
    goal_contains_foreach(B,Flag).
goal_contains_foreach((_G ? B),Flag) =>
    goal_contains_foreach(B,Flag).
goal_contains_foreach((A,B),Flag) =>
    goal_contains_foreach(A,Flag),
    goal_contains_foreach(B,Flag).
goal_contains_foreach((A -> B ; C),Flag) =>
    goal_contains_foreach(A,Flag),
    goal_contains_foreach(B,Flag),
    goal_contains_foreach(C,Flag).
goal_contains_foreach((A;B),Flag) =>
    goal_contains_foreach(A,Flag),
    goal_contains_foreach(B,Flag).
goal_contains_foreach(not(A),Flag) =>
    goal_contains_foreach(A,Flag).
goal_contains_foreach(\+(A),Flag) =>
    goal_contains_foreach(A,Flag).
goal_contains_foreach(Lhs @= Rhs,Flag) =>
    exp_contains_list_comp(Lhs,Flag),
    exp_contains_list_comp(Rhs,Flag).
goal_contains_foreach(E1#=E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(E1#\=E2,Flag) => 
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(E1#<E2,Flag) => 
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(E1#=<E2,Flag) => 
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(E1#>E2,Flag) => 
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(E1#>=E2,Flag) => 
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
goal_contains_foreach(G,Flag), functor(G,foreach,_) => Flag=1.
goal_contains_foreach(_G,_Flag) => true.

exp_contains_list_comp(_,Flag), nonvar(Flag) => true.
exp_contains_list_comp([(_ : _)|_],Flag) => Flag=1.
exp_contains_list_comp(E1+E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
exp_contains_list_comp(E1-E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
exp_contains_list_comp(E1*E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
exp_contains_list_comp(E1/E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
exp_contains_list_comp(E1//E2,Flag) =>
    exp_contains_list_comp(E1,Flag),
    exp_contains_list_comp(E2,Flag).
exp_contains_list_comp(-E,Flag) =>
    exp_contains_list_comp(E,Flag).
exp_contains_list_comp(abs(E),Flag) =>
    exp_contains_list_comp(E,Flag).
exp_contains_list_comp(sum([(_ : _)|_]),Flag) => Flag=1.
exp_contains_list_comp(min([(_ : _)|_]),Flag) => Flag=1.
exp_contains_list_comp(max([(_ : _)|_]),Flag) => Flag=1.
exp_contains_list_comp(_,_) => true.

%%
'$change_list_comprehension_to_foreach_cmptime'(T,I,Is,CallForeach,L):-
    '$retrieve_list_comp_lvars_goal_cmptime'(Is,LocalVars1,Goal1,Is1),
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

'$retrieve_list_comp_lvars_goal_cmptime'([],LocalVars,Goal,Is) =>
     LocalVars=[],Goal=true,Is=[].
'$retrieve_list_comp_lvars_goal_cmptime'([E|Es],LocalVars,Goal,Is),E = (_ in _) =>
    Is=[E|IsR],
    '$retrieve_list_comp_lvars_goal_cmptime'(Es,LocalVars,Goal,IsR).
'$retrieve_list_comp_lvars_goal_cmptime'([LVars,G],LocalVars,Goal,Is),LVars=[] =>
    Is=[],LocalVars=LVars,G=Goal.
'$retrieve_list_comp_lvars_goal_cmptime'([LVars,G],LocalVars,Goal,Is),LVars=[_|_] =>
    Is=[],LocalVars=LVars,G=Goal.
'$retrieve_list_comp_lvars_goal_cmptime'([LVars],LocalVars,Goal,Is),LVars=[_|_] =>
    Is=[],LocalVars=LVars,Goal=true.
'$retrieve_list_comp_lvars_goal_cmptime'([LVars],LocalVars,Goal,Is),LVars=[] =>
    Is=[],LocalVars=LVars,Goal=true.
'$retrieve_list_comp_lvars_goal_cmptime'([G],LocalVars,Goal,Is),nonvar(G) =>
    Is=[],LocalVars=[],G=Goal.

%%
extract_list_comprehension_array_notation(T,NT,TempCalls,TempCallsR), var(T) =>
    NT=T,TempCalls=TempCallsR.
extract_list_comprehension_array_notation(T,NT,TempCalls,TempCallsR), T=(_^_) =>
    TempCalls=[NT @= T|TempCallsR].
extract_list_comprehension_array_notation(sum(T),NT,TempCalls,TempCallsR), T=[(_ : _)|_] =>
    NT=sum(L),
    TempCalls=[L @= T|TempCallsR].
extract_list_comprehension_array_notation(min(T),NT,TempCalls,TempCallsR), T=[(_ : _)|_] => 
    NT=min(L),
    TempCalls=[L @= T|TempCallsR].
extract_list_comprehension_array_notation(max(T),NT,TempCalls,TempCallsR), T=[(_ : _)|_] =>
    NT=max(L),
    TempCalls=[L @= T|TempCallsR].
extract_list_comprehension_array_notation(X+Y,NT,TempCalls,TempCallsR) =>
    NT=(NX+NY),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCalls1),
    extract_list_comprehension_array_notation(Y,NY,TempCalls1,TempCallsR).
extract_list_comprehension_array_notation(X-Y,NT,TempCalls,TempCallsR) =>
    NT=(NX-NY),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCalls1),
    extract_list_comprehension_array_notation(Y,NY,TempCalls1,TempCallsR).
extract_list_comprehension_array_notation(X*Y,NT,TempCalls,TempCallsR) =>
    NT=(NX*NY),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCalls1),
    extract_list_comprehension_array_notation(Y,NY,TempCalls1,TempCallsR).
extract_list_comprehension_array_notation(X//Y,NT,TempCalls,TempCallsR) =>
    NT=(NX//NY),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCalls1),
    extract_list_comprehension_array_notation(Y,NY,TempCalls1,TempCallsR).
extract_list_comprehension_array_notation(X/Y,NT,TempCalls,TempCallsR) =>
    NT=(NX/NY),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCalls1),
    extract_list_comprehension_array_notation(Y,NY,TempCalls1,TempCallsR).
extract_list_comprehension_array_notation(abs(X),NT,TempCalls,TempCallsR) =>
    NT=abs(NX),
    extract_list_comprehension_array_notation(X,NX,TempCalls,TempCallsR).
extract_list_comprehension_array_notation(T,NT,TempCalls,TempCallsR) =>
    NT=T,TempCalls=TempCallsR.

compile_foreach_goal(G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    functor(G,_,Arity),
    (compile_foreach_retrieve_iterators(G,1,Arity,Is,ACs,LocalVars,Goal)->
       compile_foreach(Is,LocalVars,ACs,Goal,NG,PrefixName,ProgTab,DumNo,DumNoR)
     ;
       NG=G,DumNo=DumNoR  % interpreted
     ).

compile_foreach(Iterators,LocalVars,ACs,G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    initial_acs_map(ACs,ACMap,Init,Fin),
    NG=(Init,G1,Fin),
    compile_foreach_iterators(Iterators,LocalVars,ACMap,G,G1,PrefixName,ProgTab,DumNo,DumNoR).

compile_foreach_iterators([],_LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR) =>
    substitute_accumulators(G,G1,ACMap),
    expand_constr(G1,NG,PrefixName,ProgTab,DumNo,DumNoR).
compile_foreach_iterators([I in B1..Step..B2|Iterators],LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR) =>
    (var(I)->true; cmp_error(["wrong loop variable: ", I])),
    (Step== -1 ->
     compile_foreach_range_downto_1(I,B1,B2,Iterators,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR);
     compile_foreach_range_step(I,B1,B2,Step,Iterators,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR)).
compile_foreach_iterators([I in L..U|Iterators],LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR) =>
    (var(I)->true; cmp_error(["wrong loop variable: ", I])),
    compile_foreach_range_upto_1(I,L,U,Iterators,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR).
compile_foreach_iterators([I in Lst|Iterators],LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR) =>
    compile_foreach_lst(I,Lst,Iterators,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR).

compile_foreach_range_upto_1(I,LExp,UExp,IteratorsR,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    new_pred_name_foreach(PrefixName,DumNo,NewPredName),
    DumNo1 is DumNo+1,
    term_variables((IteratorsR,G),AllVars),
    extract_arg_vars(AllVars,I,IteratorsR,LocalVars,ACMap,GVars,[]),
    foreach_accumulator_args(ACMap,ACHeadArgs,[]),
    split_acs_map(ACMap,ACMap1,ACMap2),
    append(GVars,ACHeadArgs,Args),
    foreach_accumulator_args(ACMap2,ACTailArgs,[]),
    append(GVars,ACTailArgs,TailArgs),
    foreach_end_accumulator_args(ACMap,BodyR1),
    CallNewPred=..[NewPredName,Lower,Upper|Args],
    NG=(Lower is LExp, Upper is UExp, CallNewPred),
    Head=..[NewPredName,Elm,Upper|Args],
    Body1=(Elm>Upper : BodyR1),
    Tail2=..[NewPredName,Elm1,Upper|TailArgs],
    Body2=(G1,Elm1 is Elm+1,Tail2),
    Cl1=(Head:-Body1),
    copy_term(Cl1,Cl1CP),
    Cl2=(Head:-true : Body2),
    I=Elm,
    copy_term(t(IteratorsR,LocalVars,ACMap1,G,G1,Cl2),TCopy),
    TCopy=t(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,Cl2CP),
    %
    compile_foreach_iterators(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,PrefixName,ProgTab,DumNo1,DumNo2),
    %
    '$eliminate_disjunctions'(Cl1CP,NCl1CP,ProgTab,DumNo2,DumNo3),
    '$eliminate_disjunctions'(Cl2CP,NCl2CP,ProgTab,DumNo3,DumNoR),
    functor(Head,_,Arity),
    PredDef=pred(NewPredName,Arity,_Mode,_Delay,_Tabled,[NCl1CP,NCl2CP]),
    hashtable_put(ProgTab,NewPredName/Arity,PredDef).

compile_foreach_range_downto_1(I,UExp,LExp,IteratorsR,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    new_pred_name_foreach(PrefixName,DumNo,NewPredName),
    DumNo1 is DumNo+1,
    term_variables((IteratorsR,G),AllVars),
    extract_arg_vars(AllVars,I,IteratorsR,LocalVars,ACMap,GVars,[]),
    foreach_accumulator_args(ACMap,ACHeadArgs,[]),
    split_acs_map(ACMap,ACMap1,ACMap2), 
    append(GVars,ACHeadArgs,Args),
    foreach_accumulator_args(ACMap2,ACTailArgs,[]),
    append(GVars,ACTailArgs,TailArgs),
    foreach_end_accumulator_args(ACMap,BodyR1),
    CallNewPred=..[NewPredName,Upper,Lower|Args],
    NG=(Lower is LExp, Upper is UExp, CallNewPred),
    Head=..[NewPredName,Elm,Lower|Args],
    Body1=(Elm<Lower : BodyR1),
    Tail2=..[NewPredName,Elm1,Lower|TailArgs],
    Body2=(G1,Elm1 is Elm-1,Tail2),
    Cl1=(Head:-Body1),
    copy_term(Cl1,Cl1CP),
    Cl2=(Head:-true : Body2),
    I=Elm,
    copy_term(t(IteratorsR,LocalVars,ACMap1,G,G1,Cl2),TCopy),
    TCopy=t(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,Cl2CP),
    %
    compile_foreach_iterators(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,PrefixName,ProgTab,DumNo1,DumNo2),
    %
    '$eliminate_disjunctions'(Cl1CP,NCl1CP,ProgTab,DumNo2,DumNo3),
    '$eliminate_disjunctions'(Cl2CP,NCl2CP,ProgTab,DumNo3,DumNoR),
    functor(Head,_,Arity),
    PredDef=pred(NewPredName,Arity,_Mode,_Delay,_Tabled,[NCl1CP,NCl2CP]),
    hashtable_put(ProgTab,NewPredName/Arity,PredDef).

compile_foreach_range_step(I,B1,B2,Step,IteratorsR,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    new_pred_name_foreach(PrefixName,DumNo,NewPredName),
    DumNo1 is DumNo+1,
    term_variables((IteratorsR,G),AllVars),
    extract_arg_vars(AllVars,I,IteratorsR,LocalVars,ACMap,GVars,[]),
    foreach_accumulator_args(ACMap,ACHeadArgs,[]),
    split_acs_map(ACMap,ACMap1,ACMap2),
    append(GVars,ACHeadArgs,Args),
    foreach_accumulator_args(ACMap2,ACTailArgs,[]),
    append(GVars,ACTailArgs,TailArgs),
    foreach_end_accumulator_args(ACMap,BodyR1),
    CallNewPred=..[NewPredName,B1Val,B2Val,StepVal|Args],
    NG=(B1Val is B1, B2Val is B2, StepVal is Step, CallNewPred),
    Head=..[NewPredName,Elm,B2Arg,StepArg|Args],
    Body1=(StepArg>0,Elm>B2Arg : BodyR1),
    Cl1=(Head:-Body1),
    copy_term(Cl1,Cl1CP),
    Body2=(StepArg<0,Elm<B2Arg : BodyR1),
    Cl2=(Head:-Body2),
    copy_term(Cl2,Cl2CP),

    Tail3=..[NewPredName,Elm1,B2Arg,StepArg|TailArgs],
    Body3=(G1,Elm1 is Elm+StepArg,Tail3),
    Cl3=(Head:-true : Body3),
    I=Elm,
    copy_term(t(IteratorsR,LocalVars,ACMap1,G,G1,Cl3),TCopy),
    TCopy=t(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,Cl3CP),
    %
    compile_foreach_iterators(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,PrefixName,ProgTab,DumNo1,DumNo2),
    %
    '$eliminate_disjunctions'(Cl1CP,NCl1CP,ProgTab,DumNo2,DumNo3),
    '$eliminate_disjunctions'(Cl2CP,NCl2CP,ProgTab,DumNo3,DumNo4),
    '$eliminate_disjunctions'(Cl3CP,NCl3CP,ProgTab,DumNo4,DumNoR),
    functor(Head,_,Arity),
    PredDef=pred(NewPredName,Arity,_Mode,_Delay,_Tabled,[NCl1CP,NCl2CP,NCl3CP]),
    hashtable_put(ProgTab,NewPredName/Arity,PredDef).

		      

compile_foreach_lst(I,Lst,IteratorsR,LocalVars,ACMap,G,NG,PrefixName,ProgTab,DumNo,DumNoR):-
    new_pred_name_foreach(PrefixName,DumNo,NewPredName),
    DumNo1 is DumNo+1,
    term_variables((IteratorsR,G),AllVars),
    extract_arg_vars(AllVars,I,IteratorsR,LocalVars,ACMap,GVars,[]),
    foreach_accumulator_args(ACMap,ACHeadArgs,[]),
    split_acs_map(ACMap,ACMap1,ACMap2),
    append(GVars,ACHeadArgs,Args),
    foreach_accumulator_args(ACMap2,ACTailArgs,[]),
    append(GVars,ACTailArgs,TailArgs),
    foreach_end_accumulator_args(ACMap,BodyR1),
    NG=..[NewPredName,Lst|Args],
    Head1=..[NewPredName,[]|Args],
    Body1=BodyR1,
    Head2=..[NewPredName,[Elm|Elms]|Args],
    Tail2=..[NewPredName,Elms|TailArgs],
    Head3=..[NewPredName,[_|Elms]|Args],
    Tail3=..[NewPredName,Elms|Args],
    Body2=(G1,Tail2),
    Cl1=(Head1:-true : Body1),
    copy_term(Cl1,Cl1CP),
    Cl2=(Head2:-true : Body2),
    I=Elm,
    copy_term(t(IteratorsR,LocalVars,ACMap1,G,G1,Cl2),TCopy2),
    TCopy2=t(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,Cl2CP),
    Cl3=(Head3:-true : Tail3),
    copy_term(Cl3,Cl3CP),
    compile_foreach_iterators(IteratorsRCP,LocalVarsCP,ACMap1CP,GCP,G1CP,PrefixName,ProgTab,DumNo1,DumNo2),
    '$eliminate_disjunctions'(Cl1CP,NCl1CP,ProgTab,DumNo2,DumNo3),
    '$eliminate_disjunctions'(Cl2CP,NCl2CP,ProgTab,DumNo3,DumNoR),
    functor(Head1,_,Arity),
    Head4=..[NewPredName,Collection|Args],
    Tail4=..[NewPredName,CollectionLst|Args],
    Cl4=(Head4:-true : (foreach_collection_to_lst(Collection,CollectionLst),Tail4)),
    copy_term(Cl4,Cl4CP),
    PredDef=pred(NewPredName,Arity,_Mode,_Delay,_Tabled,[NCl1CP,NCl2CP,Cl3CP,Cl4CP]),
    hashtable_put(ProgTab,NewPredName/Arity,PredDef).

foreach_accumulator_args([],Args,ArgsR) => Args=ArgsR.
foreach_accumulator_args([ac_inout(_Name,In,Out)|ACMap],Args,ArgsR) =>
    Args=[In,Out|Args1],
    foreach_accumulator_args(ACMap,Args1,ArgsR).

foreach_end_accumulator_args([],Body) => Body=true.
foreach_end_accumulator_args([ac_inout(_Name,In,Out)|ACMap],Body) =>
    Body=(In=Out,BodyR),
    foreach_end_accumulator_args(ACMap,BodyR).
    
split_acs_map([],ACMap1,ACMap2) => ACMap1=[],ACMap2=[].
split_acs_map([ac_inout(Name,In,Out)|ACMap],ACMap1,ACMap2) =>
    ACMap1=[ac_inout(Name,In,Mid)|ACMap1R],
    ACMap2=[ac_inout(Name,Mid,Out)|ACMap2R],
    split_acs_map(ACMap,ACMap1R,ACMap2R).

/* utilities */
extract_arg_vars([],_I,_Iterators,_LocalVars,_ACMap,Args,ArgsR) => Args=ArgsR.
extract_arg_vars([Var|Vars],I,Iterators,LocalVars,ACMap,Args,ArgsR):-true ?
    ('$occur'(Var,I);
     is_a_loop_var(Var,Iterators);
     membchk(Var,LocalVars);
     foreach_lookup_acmap(Var,1,_,ACMap);
     foreach_lookup_acmap(Var,0,_,ACMap)),!,
    extract_arg_vars(Vars,I,Iterators,LocalVars,ACMap,Args,ArgsR).
extract_arg_vars([Var|Vars],I,Iterators,LocalVars,ACMap,Args,ArgsR) =>
    Args=[Var|Args1],
    extract_arg_vars(Vars,I,Iterators,LocalVars,ACMap,Args1,ArgsR).

is_a_loop_var(Var,(I in _)):-true ? '$occur'(Var,I),!.
is_a_loop_var(Var,(Iterators1,_)):-true ?
    is_a_loop_var(Var,Iterators1),!.
is_a_loop_var(Var,(_,Iterators2)) =>
    is_a_loop_var(Var,Iterators2).

initial_acs_map([],ACMap,InitCode,FinCode) => ACMap=[],InitCode=true,FinCode=true.
initial_acs_map([AC],ACMap,InitCode,FinCode) =>
    ACMap=[Triplet],
    initial_ac_map(AC,Triplet,InitCode,FinCode).
initial_acs_map([AC|ACs],[Triplet|ACMap],InitCode,FinCode):-
    InitCode=(InitCode1,InitCodeR),
    FinCode=(FinCode1,FinCodeR),
    initial_ac_map(AC,Triplet,InitCode1,FinCode1),
    initial_acs_map(ACs,ACMap,InitCodeR,FinCodeR).
initial_acs_map(AC,ACMap,InitCode,FinCode) =>
    ACMap=[Triplet],
    initial_ac_map(AC,Triplet,InitCode,FinCode).

initial_ac_map(ac(Name,InitVal),ac_inout(Name,NameIn,NameOut),(NameIn=InitVal),(Name=NameOut)).
initial_ac_map(ac1(Name,FinVal),ac_inout(Name,NameIn,NameOut),(Name=NameIn),(NameOut=FinVal)).

% Replace inputs and outputs in recurrences: A^0 is input and A^1 is output.
substitute_accumulators(Term,NTerm,_ACMap):-var(Term) :
    NTerm=Term.
substitute_accumulators(Term,NTerm,_ACMap):-atomic(Term) :
    NTerm=Term.
substitute_accumulators(Term,NTerm,ACMap):-Term=(Var^Tail) :
    (foreach_lookup_acmap(Var,Tail,NTerm,ACMap)->true;
     NTerm=Term).
substitute_accumulators([E|Es],Lst,ACMap) =>
    Lst=[E1|Es1],
    substitute_accumulators(E,E1,ACMap),
    substitute_accumulators(Es,Es1,ACMap).
substitute_accumulators(Term,NTerm,ACMap) =>
    functor(Term,F,N),
    functor(NTerm,F,N),
    substitute_accumulators(Term,NTerm,1,N,ACMap).

substitute_accumulators(_Term,_NTerm,I,N,_), I>N => true.
substitute_accumulators(Term,NTerm,I,N,ACMap) =>
    arg(I,Term,A),
    arg(I,NTerm,NA),
    substitute_accumulators(A,NA,ACMap),
    I1 is I+1,
    substitute_accumulators(Term,NTerm,I1,N,ACMap).
     
foreach_lookup_acmap(Term,Tail,NTerm,[ac_inout(Term1,In,Out)|_]), Term==Term1 => 
    (Tail==0->NTerm=In;
     Tail==1->NTerm=Out).
foreach_lookup_acmap(Term,Tail,NTerm,[_|ACMap]) =>
    foreach_lookup_acmap(Term,Tail,NTerm,ACMap).

new_pred_name_foreach(PrefixName,DumNo,NewPredName):-
    number_codes(DumNo,DumNoCodes),
    append(PrefixName,[0'_,0'#,0'_|DumNoCodes],NewPredNameCodes),
    atom_codes(NewPredName,NewPredNameCodes).
    
compile_foreach_retrieve_iterators(G,I,Arity,Iterators,ACs,LocalVars,Goal), I==Arity =>
    arg(I,G,Goal),
    Iterators=[],
    (var(ACs)->ACs=[];true),
    (var(LocalVars)->LocalVars=[];true).
compile_foreach_retrieve_iterators(G,I,Arity,Iterators,ACs,LocalVars,Goal) =>
    arg(I,G,A),
    (nonvar(A),A=(_ in _) ->
       Iterators=[A|Iterators1]
    ;I>=Arity-2 ->
       (cmp_foreach_check_accumulators(A) ->
          Iterators=Iterators1,
          (var(ACs)->ACs=A;cmp_error(["two accumulators given separately in foreach"]),fail)
	;cmp_foreach_check_lvars(A)->
          Iterators=Iterators1,
          (var(LocalVars)->LocalVars=A;cmp_error(["invalid local variables given in foreach"]),fail)
	;fail
	)
    ;fail
    ),
    I1 is I+1,
    compile_foreach_retrieve_iterators(G,I1,Arity,Iterators1,ACs,LocalVars,Goal).

cmp_foreach_check_lvars([]) => true.
cmp_foreach_check_lvars([X|Xs]) => var(X),cmp_foreach_check_lvars(Xs).

cmp_foreach_check_accumulators(ac1(_,_)) => true.
cmp_foreach_check_accumulators(ac(_,_)) => true.
cmp_foreach_check_accumulators(Accumulators), Accumulators=[_|_] =>
    cmp_foreach_check_accumulator_lst(Accumulators).

cmp_foreach_check_accumulator_lst([]) => true.
cmp_foreach_check_accumulator_lst([X|_]), var(X) => fail.
cmp_foreach_check_accumulator_lst([ac(_,_)|L]) =>
    cmp_foreach_check_accumulator_lst(L).
cmp_foreach_check_accumulator_lst([ac1(_,_)|L]) =>
    cmp_foreach_check_accumulator_lst(L).



    
    
    
    
