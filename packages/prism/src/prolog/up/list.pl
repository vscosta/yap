%%--------------------------------
%%  Temporary Clauses

:- dynamic $pd_temp_clause/2.
:- dynamic $pd_temp_clause/3.
:- dynamic $pd_temp_clause/4.

:- global_set($pg_temp_clause_num,0).

$pp_create_temp_clause_1(ID,X,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X) :- Body)), !.

$pp_create_temp_clause_2(ID,X,Y,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X,Y) :- Body)), !.

$pp_create_temp_clause_3(ID,X,Y,Z,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X,Y,Z) :- Body)), !.

$pp_delete_temp_clause_1(ID) :-
    retractall($pd_temp_clause(ID,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_delete_temp_clause_2(ID) :-
    retractall($pd_temp_clause(ID,_,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_delete_temp_clause_3(ID) :-
    retractall($pd_temp_clause(ID,_,_,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_create_temp_clause_num(N) :-
    global_get($pg_temp_clause_num,M),
    N is M + 1,
    global_set($pg_temp_clause_num,N), !.

$pp_delete_temp_clause_num(N) :-
    global_get($pg_temp_clause_num,N),
    M is N - 1,
    global_set($pg_temp_clause_num,M), !.
$pp_delete_temp_clause_num(_).
    

%%--------------------------------
%%  Base Predicates

$pp_length(Xs,N) :-
    $pp_length(Xs,0,N).

$pp_length(Xs0,N0,N), Xs0 = [] =>
    N0 = N.
$pp_length(Xs0,N0,N), Xs0 = [_|Xs1] =>
    N1 is N0 + 1,
    $pp_length(Xs1,N1,N).

$pp_match(Patt,X) :-
    \+ \+ ( number_vars(X,0,_), Patt ?= X ).

$pp_copy_term(X0,X) :-
    ground(X0) -> X0 = X ; copy_term(X0,X).

$pp_count(Table,Key,N) :-
    ( $pp_hashtable_get(Table,Key,N0) -> N is N0 + 1 ; N is 1 ),
    $pp_hashtable_put(Table,Key,N).

%%--------------------------------
%%  Stat: Means

avglist(List,Mean) :-
    $pp_meanlist(List,_,Mean,avglist/2).

meanlist(List,Mean) :-
    $pp_meanlist(List,_,Mean,meanlist/2).

gmeanlist(List,Mean) :-
    $pp_gmeanlist(List,_,Mean,gmeanlist/2).

hmeanlist(List,Mean) :-
    $pp_hmeanlist(List,_,Mean,hmeanlist/2).

$pp_meanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_meanlist(List,0,N0,0,M0) ->
      N0 = N,
      M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_meanlist(Xs,N0,N,M0,M), Xs = [] =>
    N0 = N,
    M0 = M.
$pp_meanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (X - M0) / N1,
    $pp_meanlist(Xs1,N1,N,M1,M).

$pp_gmeanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_gmeanlist(List,0,N0,0,M0) ->
      N0 = N,
      M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_gmeanlist(Xs,N0,N,M0,M), Xs = [] =>
    N = N0, M is exp(M0).
$pp_gmeanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (log(X) - M0) / N1,
    $pp_gmeanlist(Xs1,N1,N,M1,M).

$pp_hmeanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_hmeanlist(List,0,N0,0,M0) ->
      N0 = N, M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_hmeanlist(Xs,N0,N,M0,M), Xs = [] =>
    N = N0, M is 1 / M0.
$pp_hmeanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (1 / X  - M0) / N1,
    $pp_hmeanlist(Xs1,N1,N,M1,M).


%%--------------------------------
%%  Stat: Variance etc.

varlistp(List,Var) :-
    $pp_moment2(List,1,N,_,M2,varlistp/2),
    Var is M2 / N.

varlist(List,Var) :-
    $pp_moment2(List,2,N,_,M2,varlist/2),
    Var is M2 / (N - 1).

stdlistp(List,Std) :-
    $pp_moment2(List,1,N,_,M2,stdlistp/2),
    Std is sqrt(M2 / N).

stdlist(List,Std) :-
    $pp_moment2(List,2,N,_,M2,stdlist/2),
    Std is sqrt(M2 / (N - 1)).

semlistp(List,Sem) :-
    $pp_moment2(List,1,N,_,M2,semlistp/2),
    Sem is sqrt(M2) / N.

semlist(List,Sem) :-
    $pp_moment2(List,2,N,_,M2,semlist/2),
    Sem is sqrt(M2 / (N - 1) / N).

skewlistp(List,Skew) :-
    $pp_moment3(List,1,N,_,M2,M3,skewlistp/2),
    $pp_compute_skew0(Skew,N,M2,M3).

skewlist(List,Skew) :-
    $pp_moment3(List,3,N,_,M2,M3,skewlist/2),
    $pp_compute_skew1(Skew,N,M2,M3).

kurtlistp(List,Kurt) :-
    $pp_moment4(List,1,N,_,M2,_,M4,kurtlistp/2),
    $pp_compute_kurt0(Kurt,N,M2,M4).

kurtlist(List,Kurt) :-
    $pp_moment4(List,4,N,_,M2,_,M4,kurtlist/2),
    $pp_compute_kurt1(Kurt,N,M2,M4).

$pp_moment2(List,MinN,N,M,M2,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment2(List,0,N0,0,TmpM,0,TmpM2),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2.

$pp_moment3(List,MinN,N,M,M2,M3,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment3(List,0,N0,0,TmpM,0,TmpM2,0,TmpM3),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2, TmpM3 = M3.

$pp_moment4(List,MinN,N,M,M2,M3,M4,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment4(List,0,N0,0,TmpM,0,TmpM2,0,TmpM3,0,TmpM4),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2, TmpM3 = M3, TmpM4 = M4.

$pp_moment2(Xs,TmpN,N,TmpM,M,TmpM2,M2), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2.
$pp_moment2(Xs,OldN,N,OldM,M,OldM2,M2), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - NewM)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    $pp_moment2(Xs1,NewN,N,NewM,M,NewM2,M2).

$pp_moment3(Xs,TmpN,N,TmpM,M,TmpM2,M2,TmpM3,M3), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2,
    TmpM3 = M3.
$pp_moment3(Xs,OldN,N,OldM,M,OldM2,M2,OldM3,M3), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - OldN)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    NewM3 is OldM3 + E * (F * (NewN - 2) - 3 * OldM2),
    $pp_moment3(Xs1,NewN,N,NewM,M,NewM2,M2,NewM3,M3).

$pp_moment4(Xs,TmpN,N,TmpM,M,TmpM2,M2,TmpM3,M3,TmpM4,M4), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2,
    TmpM3 = M3,
    TmpM4 = M4.
$pp_moment4(Xs,OldN,N,OldM,M,OldM2,M2,OldM3,M3,OldM4,M4), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - OldN)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    NewM3 is OldM3 + E * (F * (NewN - 2) - 3 * OldM2),
    NewM4 is OldM4 + E * (E * F * (NewN ** 2 - (NewN + 1)) - 2 * (OldM3 + NewM3)),
    $pp_moment4(Xs1,NewN,N,NewM,M,NewM2,M2,NewM3,M3,NewM4,M4).

$pp_compute_skew0(Skew,N,M2,M3) :-
    Skew is M3 / M2 * sqrt(N / M2).

$pp_compute_skew1(Skew,N,M2,M3) :-
    Skew is M3 / M2 * sqrt((N - 1) / M2) * N / (N - 2).

$pp_compute_kurt0(Kurt,N,M2,M4) :-
    Kurt is M4 / (M2 * M2) * N - 3.

$pp_compute_kurt1(Kurt,N,M2,M4) :-
    F is M4 / (M2 * M2) * N * (N + 1),
    G is 3 * (N - 1),
    H is (N - 1) / (float(N - 2) * (N - 3)), % float(*) avoids overflow
    Kurt is (F - G) * H.


%%--------------------------------
%%  Stat: Mode

modelist(List,Mode) :-
    $pp_modelist(List,Mode,modelist/2).

amodelist(List,Modes) :-
    $pp_amodelist(List,Modes,amodelist/2).

rmodelist(List,Mode) :-
    $pp_amodelist(List,Modes,rmodelist/2),
    $pp_pmodelist(Modes,Mode).

pmodelist(List,Mode) :-
    $pp_pmodelist(List,Mode,pmodelist/2).

$pp_modelist(List,Mode,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    new_hashtable(Table),
    ( $pp_modelist(List,Table,_,0,Mode0) ->
      $pp_copy_term(Mode0,Mode)
    ; throw(error(type_error(list,List),Source))
    ).

$pp_modelist(Xs,_,Y,_,Mode), Xs = [] =>
    Y = Mode.
$pp_modelist(Xs,Table,Y0,N0,Mode), Xs = [X|Xs1] =>
    $pp_count(Table,X,N),
    ( $pp_modelist_cmp(N0,N,Y0,X) -> Y1 = X, N1 = N ; Y1 = Y0, N1 = N0 ),
    $pp_modelist(Xs1,Table,Y1,N1,Mode).

$pp_modelist_cmp(N0,N,_,_), N0 < N => true.
$pp_modelist_cmp(N0,N,_,_), N0 > N => fail.
$pp_modelist_cmp(_,_,X0,X) =>
    X0 @> X.

$pp_amodelist(List,Modes,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    new_hashtable(Table),
    ( $pp_amodelist(List,Table,_,0,Modes0) ->
      $pp_copy_term(Modes0,Modes1),
      sort(Modes1,Modes)
    ; throw(error(type_error(list,List),Source))
    ).

$pp_amodelist(Xs,_,Ys,_,Modes), Xs = [] =>
    Ys = Modes.
$pp_amodelist(Xs,Table,Ys0,N0,Modes), Xs = [X|Xs1] =>
    $pp_count(Table,X,N),
    ( N0 < N ->
      Ys1 = [X], N1 = N
    ; N0 > N ->
      Ys1 = Ys0, N1 = N0
    ; %% else
      Ys1 = [X|Ys0], N1 = N0
    ),
    $pp_amodelist(Xs1,Table,Ys1,N1,Modes).

$pp_pmodelist(List,Mode,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    ( $pp_pmodelist(List,Mode0) ->
      Mode0 = Mode
    ; throw(error(type_error(list,List),Source))
    ).

$pp_pmodelist(List,Mode) :-
    $pp_length(List,L), $pc_random_int(L,I), nth0(I,List,Mode).


%%--------------------------------
%%  Stat: Median

medianlist(List,Median) :-
    $pp_medianlist(List,Median,medianlist/2).

$pp_medianlist(List,Median,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_medianlist(List,Median0) ->
      Median0 = Median
    ; throw(error(type_error(list,List),Source))
    ).

$pp_medianlist(List,Median) :-
    $pp_length(List,L),
    N is L // 2,
    $pp_mergesort(0,L,List,_,Temp),
    ( L mod 2 is 0 ->
      nth1(N,Temp,A),
      nth0(N,Temp,B),
      Median is A + (B - A) / 2         % avoids overflow
    ; nth0(N,Temp,Median)
    ).


%%--------------------------------
%%  Stat: Min/Max

minlist(List,Min) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),minlist/2),
    $pp_require_numbers(List,$msg(2108),minlist/2),
    Min is min(List).

maxlist(List,Max) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),maxlist/2),
    $pp_require_numbers(List,$msg(2108),maxlist/2),
    Max is max(List).


%%--------------------------------
%%  Stat: agglist/2

agglist(List,Dest) :-
    $pp_require_list_not_shorter_than(Dest,1,$msg(2103),agglist/2),
    Flag = $aggop(0,0,0),
    $pp_agglist_1(Dest,Flag),
    $pp_agglist_2(List,Flag,N,M,M2,M3,M4,Modes),
    $pp_agglist_3(List,Dest,N,M,M2,M3,M4,Modes).

$pp_agglist_1(Dest,_), Dest = [] => true.
$pp_agglist_1(Dest,Flag), Dest = [Op=_|Dest1] =>
    $pp_require_agglist_operation(Op,$msg(2107),agglist/2),
    %%  X = none(0)/len(1)/mean(2)/var(3)/skew(4)/kurt(5)
    %%  Y = none(0)/mode(1)/amode(2)
    ( Op == sum    -> X = 0, Y = 0, N = 0
    ; Op == avg    -> X = 2, Y = 0, N = 1
    ; Op == mean   -> X = 2, Y = 0, N = 1
    ; Op == gmean  -> X = 0, Y = 0, N = 1
    ; Op == hmean  -> X = 0, Y = 0, N = 1
    ; Op == varp   -> X = 3, Y = 0, N = 1
    ; Op == var    -> X = 3, Y = 0, N = 2
    ; Op == stdp   -> X = 3, Y = 0, N = 1
    ; Op == std    -> X = 3, Y = 0, N = 2
    ; Op == semp   -> X = 3, Y = 0, N = 1
    ; Op == sem    -> X = 3, Y = 0, N = 2
    ; Op == skewp  -> X = 4, Y = 0, N = 1
    ; Op == skew   -> X = 4, Y = 0, N = 3
    ; Op == kurtp  -> X = 5, Y = 0, N = 1
    ; Op == kurt   -> X = 5, Y = 0, N = 4
    ; Op == mode   -> X = 0, Y = 1, N = 1
    ; Op == amode  -> X = 0, Y = 2, N = 1
    ; Op == rmode  -> X = 0, Y = 2, N = 1
    ; Op == pmode  -> X = 0, Y = 0, N = 1
    ; Op == median -> X = 0, Y = 0, N = 1
    ; Op == min    -> X = 0, Y = 0, N = 1
    ; Op == max    -> X = 0, Y = 0, N = 1
    ; Op == len    -> X = 1, Y = 0, N = 0
    ),
    Flag = $aggop(X0,Y0,N0),
    ( X0 < X -> setarg(1,Flag,X) ; true ),
    ( Y0 < Y -> setarg(2,Flag,Y) ; true ),
    ( N0 < N -> setarg(3,Flag,N) ; true ), !,
    $pp_agglist_1(Dest1,Flag).

$pp_agglist_2(List,Flag,N,M,M2,M3,M4,Modes) :-
    Flag = $aggop(X,Y,MinN),
    ( X == 0 ->
      true
    ; X == 1 -> $pp_length(List,N)
    ; X == 2 ->
      $pp_meanlist(List,N,M,agglist/2)
    ; X == 3 ->
      $pp_moment2(List,MinN,N,M,M2,agglist/2)
    ; X == 4 ->
      $pp_moment3(List,MinN,N,M,M2,M3,agglist/2)
    ; X == 5 ->
      $pp_moment4(List,MinN,N,M,M2,M3,M4,agglist/2)
    ; %% else
      $pp_unmatched_branches($pp_agglist_2/8,first_arg)
    ),
    ( Y == 0 ->
      true
    ; Y == 1 ->
      $pp_modelist(List,Mode,agglist/2), Modes = [Mode]
    ; Y == 2 ->
      $pp_amodelist(List,Modes,agglist/2)
    ; %% else
      $pp_unmatched_branches($pp_agglist_2/8,second_arg)
    ).

$pp_agglist_3(_,Dest,_,_,_,_,_,_), Dest = [] => true.
$pp_agglist_3(List,Dest,N,M,M2,M3,M4,Mode), Dest = [Op=Y|Dest1] =>
    ( Op == sum    -> Y is sum(List)
    ; Op == avg    -> Y = M
    ; Op == mean   -> Y = M
    ; Op == gmean  -> $pp_gmeanlist(List,_,Y,agglist/2)
    ; Op == hmean  -> $pp_hmeanlist(List,_,Y,agglist/2)
    ; Op == varp   -> Y is M2 / N
    ; Op == var    -> Y is M2 / (N - 1)
    ; Op == stdp   -> Y is sqrt(M2 / N)
    ; Op == std    -> Y is sqrt(M2 / (N - 1))
    ; Op == semp   -> Y is sqrt(M2) / N
    ; Op == sem    -> Y is sqrt(M2 / (N - 1) / N)
    ; Op == skewp  -> $pp_compute_skew0(Y,N,M2,M3)
    ; Op == skew   -> $pp_compute_skew1(Y,N,M2,M3)
    ; Op == kurtp  -> $pp_compute_kurt0(Y,N,M2,M4)
    ; Op == kurt   -> $pp_compute_kurt1(Y,N,M2,M4)
    ; Op == mode   -> [Y|_] = Mode
    ; Op == amode  -> Y = Mode
    ; Op == rmode  -> $pp_pmodelist(Mode,Y)
    ; Op == pmode  -> $pp_pmodelist(List,Y,agglist/2)
    ; Op == median -> $pp_medianlist(List,Y,agglist/2)
    ; Op == min    -> Y is min(List)
    ; Op == max    -> Y is max(List)
    ; Op == len    -> Y = N
    ; $pp_raise_unmatched_branches($pp_agglist_3/8,operation)
    ), !,
    $pp_agglist_3(List,Dest1,N,M,M2,M3,M4,Mode).
$pp_agglist_3(_,_,_,_,_,_) =>
    $pp_raise_unmatched_branches($pp_agglist_3/8,list).


%%--------------------------------
%%  Map

maplist(X,Clause,Xs) :-
    $pp_create_temp_clause_1(ID,X,Clause),
    ( $pp_maplist(ID,Xs) -> R = true ; R = fail ),
    $pp_delete_temp_clause_1(ID), R.

maplist(X,Y,Clause,Xs,Ys) :-
    $pp_create_temp_clause_2(ID,X,Y,Clause),
    ( $pp_maplist(ID,Xs,Ys) -> R = true ; R = fail ),
    $pp_delete_temp_clause_2(ID), R.

maplist(X,Y,Z,Clause,Xs,Ys,Zs) :-
    $pp_create_temp_clause_3(ID,X,Y,Z,Clause),
    ( $pp_maplist(ID,Xs,Ys,Zs) -> R = true ; R = fail ),
    $pp_delete_temp_clause_3(ID), R.

$pp_maplist(_,[]).
$pp_maplist(ID,[X|Xs]) :-
    $pd_temp_clause(ID,X), !, $pp_maplist(ID,Xs).

$pp_maplist(_,[],[]).
$pp_maplist(ID,[X|Xs],[Y|Ys]) :-
    $pd_temp_clause(ID,X,Y), !, $pp_maplist(ID,Xs,Ys).

$pp_maplist(_,[],[],[]).
$pp_maplist(ID,[X|Xs],[Y|Ys],[Z|Zs]) :-
    $pd_temp_clause(ID,X,Y,Z), !, $pp_maplist(ID,Xs,Ys,Zs).

maplist_func(F,Xs) :-
    $pp_require_atom(F,$msg(2100),maplist_func/2),
    $pp_maplist_func(F,Xs).

maplist_func(F,Xs,Ys) :-
    $pp_require_atom(F,$msg(2100),maplist_func/3),
    $pp_maplist_func(F,Xs,Ys).

maplist_func(F,Xs,Ys,Zs) :-
    $pp_require_atom(F,$msg(2100),maplist_func/4),
    $pp_maplist_func(F,Xs,Ys,Zs).

$pp_maplist_func(_,[]).
$pp_maplist_func(F,[X|Xs]) :-
    call(F,X), !, $pp_maplist_func(F,Xs).

$pp_maplist_func(_,[],[]).
$pp_maplist_func(F,[X|Xs],[Y|Ys]) :-
    call(F,X,Y), !, $pp_maplist_func(F,Xs,Ys).

$pp_maplist_func(_,[],[],[]).
$pp_maplist_func(F,[X|Xs],[Y|Ys],[Z|Zs]) :-
    call(F,X,Y,Z), !, $pp_maplist_func(F,Xs,Ys,Zs).

maplist_math(Op,Xs,Ys) :-
    $pp_require_atom(Op,$msg(2101),maplist_math/3),
    functor(Expr,Op,1),
    $pp_maplist_math(Expr,Xs,Ys).

maplist_math(Op,Xs,Ys,Zs) :-
    $pp_require_atom(Op,$msg(2102),maplist_math/4),
    functor(Expr,Op,2),
    $pp_maplist_math(Expr,Xs,Ys,Zs).

$pp_maplist_math(_,[],[]).
$pp_maplist_math(Expr,[X|Xs],[Y|Ys]) :-
    setarg(1,Expr,X),
    Y is Expr,
    $pp_maplist_math(Expr,Xs,Ys).

$pp_maplist_math(_,[],[],[]).
$pp_maplist_math(Expr,[X|Xs],[Y|Ys],[Z|Zs]) :-
    setarg(1,Expr,X),
    setarg(2,Expr,Y),
    Z is Expr,
    $pp_maplist_math(Expr,Xs,Ys,Zs).


%%--------------------------------
%%  Reduction

reducelist(A,B,C,Body,Xs,Y0,Y) :-
    $pp_create_temp_clause_3(ID,A,B,C,Body),
    ( $pp_reducelist(ID,Xs,Y0,Y) -> R = true ; R = fail ),
    $pp_delete_temp_clause_3(ID), R.

$pp_reducelist(_,[],Y,Y).
$pp_reducelist(ID,[X|Xs],Y0,Y) :-
    $pd_temp_clause(ID,Y0,X,Y1), !, $pp_reducelist(ID,Xs,Y1,Y).

reducelist_func(F,Xs,Y0,Y) :-
    $pp_require_atom(F,$msg(2100),reducelist_func/4),
    $pp_reducelist_func(F,Xs,Y0,Y).

$pp_reducelist_func(_,[],Y,Y).
$pp_reducelist_func(F,[X|Xs],Y0,Y) :-
    call(F,Y0,X,Y1), !, $pp_reducelist_func(F,Xs,Y1,Y).

reducelist_math(Op,Xs,Y0,Y) :-
    $pp_require_atom(Op,$msg(2102),reducelist_math/4),
    functor(Expr,Op,2),
    $pp_reducelist_math(Expr,Xs,Y0,Y).

$pp_reducelist_math(_,[],Y,Y).
$pp_reducelist_math(Expr,[X|Xs],Y0,Y) :-
    setarg(1,Expr,Y0),
    setarg(2,Expr, X),
    Y1 is Expr,
    $pp_reducelist_math(Expr,Xs,Y1,Y).

%%--------------------------------
%%  Sublists

/* vsc: not needed in YAP */
% sublist(Sub,Lst) :-
%    $pp_sublist1(I,_,Lst,Tmp),
%    $pp_sublist2(I,_,Tmp,Sub).

sublist(Sub,Lst,I,J) :-
    $pp_require_non_negative_integer(I,$msg(2105),sublist/4),
    $pp_require_non_negative_integer(J,$msg(2105),sublist/4),
    $pp_sublist1(I,J,Lst,Tmp),
    $pp_sublist2(I,J,Tmp,Sub).

$pp_sublist1(I,J,Xs,Ys) :- var(I), !,
    $pp_sublist1_var(0,I,J,Xs,Ys).
$pp_sublist1(I,J,Xs,Ys) :- var(J), !,
    $pp_sublist1_det(I,Xs,Ys).
$pp_sublist1(I,J,Xs,Ys) :- I =< J, !,
    $pp_sublist1_det(I,Xs,Ys).

%%  [03 Dec 2008, by yuizumi]
%%  This predicate would cause infinite loops without (I0 < J) for queries
%%  such as ( sublist(_,_,I,0), I > 0 ).

$pp_sublist1_var(I0,I,_,Xs,Ys) :-
    I0 = I,
    Xs = Ys.
$pp_sublist1_var(I0,I,J,Xs,Ys) :- var(J),!,
    I1 is I0 + 1,
    Xs = [_|Xs1],
    $pp_sublist1_var(I1,I,J,Xs1,Ys).
$pp_sublist1_var(I0,I,J,Xs,Ys) :- I0 < J, !,
    I1 is I0 + 1,
    Xs = [_|Xs1],
    $pp_sublist1_var(I1,I,J,Xs1,Ys).

$pp_sublist1_det(I,Xs,Ys) :- I =:= 0, !,
    Xs = Ys.
$pp_sublist1_det(I,Xs,Ys) :- I  >  0, !,
    I1 is I - 1,
    Xs = [_|Xs1],
    $pp_sublist1_det(I1,Xs1,Ys).

$pp_sublist2(I,J,Xs,Ys) :- var(J), !,
    $pp_sublist2_var(I,J,Xs,Ys).
$pp_sublist2(I,J,Xs,Ys) :- nonvar(J), !,
    N is J - I,
    $pp_sublist2_det(N,Xs,Ys).

$pp_sublist2_var(J0,J,_ ,Ys) :-
    J0 = J,
    Ys = [].
$pp_sublist2_var(J0,J,Xs,Ys) :-
    J1 is J0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_sublist2_var(J1,J,Xs1,Ys1).

$pp_sublist2_det(N,_ ,Ys) :- N =:= 0, !,
    Ys = [].
$pp_sublist2_det(N,Xs,Ys) :- N  >  0, !,
    N1 is N - 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_sublist2_det(N1,Xs1,Ys1).


%%--------------------------------
%%  Splitting

splitlist(Prefix,Suffix,List,N) :-
    $pp_splitlist(N,List,Prefix,Suffix,splitlist/4).

grouplist(List,N,Sizes,Dest) :-
    $pp_require_positive_integer(N,$msg(2106),grouplist/4),
    $pp_grouplist(N,Sizes,List,Dest).

egrouplist(List,N,Dest) :-
    ( $pp_length(List,L) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],egrouplist/4)
    ),
    $pp_require_positive_integer(N,$msg(2106),egrouplist/4),!,
    $pp_egrouplist(N,L,List,Dest).

$pp_splitlist(N,Xs,Ys,Zs,_), var(N) =>
    $pp_splitlist_var(0,N,Xs,Ys,Zs).
$pp_splitlist(N,Xs,Ys,Zs,Source) :-
    $pp_require_non_negative_integer(N,$msg(2105),Source),
    $pp_splitlist_det(0,N,Xs,Ys,Zs).

$pp_splitlist_var(N0,N,Xs,Ys,Zs) ?=>
    N0 = N,
    Xs = Zs,
    Ys = [].
$pp_splitlist_var(N0,N,Xs,Ys,Zs) =>
    N1 is N0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_splitlist_var(N1,N,Xs1,Ys1,Zs).

$pp_splitlist_det(N0,N,Xs,Ys,Zs), N0 =:= N =>
    Xs = Zs,
    Ys = [].
$pp_splitlist_det(N0,N,Xs,Ys,Zs), N0  <  N =>
    N1 is N0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_splitlist_det(N1,N,Xs1,Ys1,Zs).

$pp_grouplist(N,Ls,Xs,Ys), N =:= 0 =>
    Ls = [],
    Xs = [],
    Ys = [].
$pp_grouplist(N,Ls,Xs,Ys), N > 0 =>
    Ls = [L|Ls1],
    Ys = [Y|Ys1],
    $pp_splitlist(L,Xs,Y,Xs1,grouplist/4),
    N1 is N - 1,
    $pp_grouplist(N1,Ls1,Xs1,Ys1).

$pp_egrouplist(N,_,_ ,Ys), N =:= 0 =>
    Ys = [].
$pp_egrouplist(N,L,Xs,Ys), N > 0 =>
    M is (L + N - 1) // N,
    Ys = [Y|Ys1],
    $pp_splitlist_det(0,M,Xs,Y,Xs1),
    N1 is N - 1,
    L1 is L - M,
    $pp_egrouplist(N1,L1,Xs1,Ys1).


%%--------------------------------
%%  Filtering

filter(Patt,Xs,Ys) :-
    ( $pp_filter(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/3)
    ).

filter(Patt,Xs,Ys,Count) :-
    ( $pp_filter(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/4)
    ),
    length(Ys,Count).

$pp_filter(_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_filter(Patt,Xs,Ys), Xs = [X|Xs1] =>
    ( $pp_match(Patt,X) -> Ys = [X|Ys1] ; Ys = Ys1 ),
    $pp_filter(Patt,Xs1,Ys1).

filter_not(Patt,Xs,Ys) :-
    ( $pp_filter_not(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/4)
    ).

filter_not(Patt,Xs,Ys,Count) :-
    ( $pp_filter_not(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter_not/4)
    ),
    length(Ys,Count).

$pp_filter_not(_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_filter_not(Patt,Xs,Ys), Xs = [X|Xs1] =>
    ( $pp_match(Patt,X) -> Ys = Ys1 ; Ys = [X|Ys1] ),
    $pp_filter_not(Patt,Xs1,Ys1).


%%--------------------------------
%%  Counting

countlist(List,Counts) :-
    new_hashtable(Table),
    ( $pp_countlist(List,Table) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],countlist/2)
    ),
    hashtable_to_list(Table,Counts1),
    $pp_countlist_copy(Counts1,0,N),
    $pp_mergesort($pp_compare_eqpair(_,_),N,Counts1,_,Counts).

$pp_countlist(Xs,_), Xs = [] => true.
$pp_countlist(Xs,Table), Xs = [X|Xs1] =>
    $pp_count(Table,X,_), $pp_countlist(Xs1,Table).

countlist(Patt,List,Count) :-
    ( $pp_countlist(Patt,List,0,Count) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],countlist/3)
    ).

$pp_countlist(_,Xs,N0,N), Xs = [] => N0 = N.
$pp_countlist(Patt,Xs,N0,N), Xs = [X|Xs1] =>
    ( variant(X,Patt) -> N1 is N0 + 1 ; N1 is N0 ),
    $pp_countlist(Patt,Xs1,N1,N).

$pp_countlist_copy(KVs,N0,N), KVs = [] => N0 = N.
$pp_countlist_copy(KVs,N0,N), KVs = [KV|KVs1] =>
    KV = (Key=_),
    ( ground(Key) ->
      true
    ; copy_term(Key,KeyCp), setarg(1,KV,KeyCp) % overwrite
    ),
    N1 is N0 + 1,
    $pp_countlist_copy(KVs1,N1,N).

$pp_compare_eqpair((_=A2),(_=B2)), A2 > B2 => true.
$pp_compare_eqpair((A1=A2),(B1=B2)), A2 =:= B2 => A1 @< B1.


%%--------------------------------
%%  Sorting

number_sort(Xs,Ys) :-
    $pp_custom_sort(0,Xs,Ys,number_sort/2).

custom_sort(Op,Xs,Ys), Op == '<'  => $pp_custom_sort(0,Xs,Ys,custom_sort/3).
custom_sort(Op,Xs,Ys), Op == '@<' => $pp_custom_sort(1,Xs,Ys,custom_sort/3).
custom_sort(Op,Xs,Ys), atom(Op) =>
    functor(Term,Op,2),
    $pp_custom_sort(Term,Xs,Ys,custom_sort/3).
custom_sort(Op,_,_) =>
    $pp_require_atom(Op,$msg(2102),custom_sort/3).

custom_sort(A,B,Body,Xs,Ys) :-
    $pp_custom_sort($cmp(A,B,Body),Xs,Ys,custom_sort/5).

$pp_custom_sort(Cmp,Xs,Ys,Source) :-
    ( $pp_length(Xs,L) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],Source)
    ),
    $pp_mergesort(Cmp,L,Xs,_,Ys).

$pp_mergesort(_,N,Xs0,Xs1,Ys), N == 0 => Xs0 = Xs1, Ys = [].
$pp_mergesort(_,N,Xs0,Xs1,Ys), N == 1 => Xs0 = [X|Xs1], Ys = [X].
$pp_mergesort(Cmp,N,Xs0,Xs1,Ys) =>
    NL is N // 2,
    NR is N - NL,
    $pp_mergesort(Cmp,NL,Xs0,Xs2,Ys0),
    $pp_mergesort(Cmp,NR,Xs2,Xs1,Ys1),
    $pp_mergelist(Cmp,Ys0,Ys1,Ys).

$pp_mergelist(_,Xs,Ys,Zs), Xs == [] => Ys = Zs.
$pp_mergelist(_,Xs,Ys,Zs), Ys == [] => Xs = Zs.
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), Cmp == 0 =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( Y < X ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), Cmp == 1 =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( Y @< X ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), functor(Cmp,_,2) =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    setarg(1,Cmp,Y),
    setarg(2,Cmp,X),
    ( Cmp ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0) =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( \+ \+ ( Cmp = $cmp(Y,X,Body), Body ) ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).


%%--------------------------------

$pp_require_agglist_operation(Op,MsgID,Source) :-
    ( $pp_test_agglist_operation(Op) -> true
    ; $pp_raise_on_require([Op],MsgID,Source,$pp_error_agglist_operation)
    ).

$pp_test_agglist_operation(Op) :-
    atom(Op),
    membchk(Op,[sum,avg,mean,gmean,hmean,varp,var,
                stdp,std,semp,sem,skewp,skew,kurtp,kurt,
                mode,amode,rmode,pmode,median,min,max,len]).

$pp_error_agglist_operation(Op,instanciation_error) :-
    var(Op), !.
$pp_error_agglist_operation(Op,Error) :-
   \+ $pp_error_atom(Op,Error), !.
$pp_error_agglist_operation(Op,domain_error(agglist_operation,Op)) :-
   \+ $pp_test_agglist_operation(Op), !.
