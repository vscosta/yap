%% -*- Prolog -*-

%%======================================================================
%%
%% This module provides a quick validator for programs represented in the
%% B-Prolog internal form.
%%
%% $pp_valid_program(Prog) :-
%%     Succeeds if and only if <Prog> is a valid program.
%%
%%======================================================================

%%--------------------------------
%%  Entry Point

$pp_valid_program(Prog) :-
    new_hashtable(Done),
    $pp_valid_program_aux(Prog,Done).

$pp_valid_program_aux(Prog,_), Prog == [] =>
    true.
$pp_valid_program_aux(Prog,Done), Prog = [Pred|Prog1] =>
    ( $pp_valid_prog_elem(Pred,Done) ->
      true
    ; $pp_emit_message($msg(1100),[Pred]), fail
    ),
    arg(1,Pred,F),
    arg(2,Pred,N),
    hashtable_register(Done,F/N,1),
    $pp_valid_program_aux(Prog1,Done).


%%--------------------------------
%%  Predicate

$pp_illegal_pred(':-',2).

$pp_valid_prog_elem(Pred,Done) :-
    Pred = pred(F,N,_,_,_,_),
    atom(F), integer(N), N >= 0,
    \+ ( $pp_illegal_pred(F,N) ; hashtable_get(Done,F/N,_) ),
    $pp_valid_prog_pred(Pred).

$pp_valid_prog_pred(Pred),
      Pred = pred(F,N,M,D,T,Cls),
      F == $damon_load,
      N == 0 =>
    var(M),
    var(D),
    var(T),
    Cls = [Cl0,Cl1],
    Cl0 = ($damon_load :- Body),
    Cl1 = ($damon_load :- true),
    $pp_valid_damon(Body).
$pp_valid_prog_pred(Pred),
      Pred = pred(F,N,M,D,T,Cls) =>
    $pp_valid_mspec(N,M),
    $pp_valid_delay(D),
    $pp_valid_table(T),
    $pp_valid_clauses(F,N,D,Cls).


%%--------------------------------
%%  $damon_load/0

$pp_valid_damon(G) :- G = (A,B), !,
    $pp_valid_damon(A),
    $pp_valid_damon(B).
$pp_valid_damon(G) :- G == true, !,
    true.
$pp_valid_damon(G) :- G = $query(_), !,
    true.
$pp_valid_damon(G) :- callable(G), !,
    true.

%%--------------------------------
%%  Mode Spec

$pp_valid_mspec(_,M), var(M)    => true.
$pp_valid_mspec(N,M), nonvar(M) =>
    $pp_valid_mspec_loop(N,M).

$pp_valid_mspec_loop(N,ModeL), N == 0 => ModeL == [].
$pp_valid_mspec_loop(N,ModeL), N >= 1 =>
    ModeL = [Mode|ModeL1],
    $pp_valid_mode(Mode),
    N1 is N - 1,
    $pp_valid_mspec_loop(N1,ModeL1).

$pp_valid_mode(M), M == c  => true.
$pp_valid_mode(M), M == f  => true.
$pp_valid_mode(M), M == nv => true.
$pp_valid_mode(M), M == d  => true.

%%--------------------------------
%%  Delay

$pp_valid_delay(D), var(D) => true.
$pp_valid_delay(D), D == 1 => true.


%%--------------------------------
%%  Table

$pp_valid_table(T), var(T) => true.
$pp_valid_table(T),
      T = tabled(U1,U2,U3,U4),
      var(U1),
      var(U2),
      var(U3),
      var(U4) => true.


%%--------------------------------
%%  Clauses

$pp_valid_clauses(_,_,_,Cls), Cls == [] => true.
$pp_valid_clauses(F,N,D,Cls), Cls = [Cl|Cls1] =>
    $pp_valid_clause(F,N,D,Cl),
    $pp_valid_clauses(F,N,D,Cls1).

$pp_valid_clause(F,N,_,Cl), Cl = (H :- _) =>
    nonvar(H),
    functor(H,F,N).
$pp_valid_clause(F,N,D,Cl), Cl = delay(Cl1) =>
    D == 1,
    $pp_valid_clause(F,N,_,Cl1).
$pp_valid_clause(F,N,_,Cl) =>
    nonvar(Cl),
    functor(Cl,F,N).
