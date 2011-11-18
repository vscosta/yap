%% -*- Prolog -*-

%%======================================================================
%%
%% This module provides a pretty-printer for programs.  In the following
%% preidcates, <Prog> should be a valid program in the B-Prolog internal
%% form; otherwise they would behave in an unexpected way.
%% 
%% $pp_dump_program(Prog) :-
%%     Writes <Prog> into the current output stream.
%% 
%% $pp_dump_program(S,Prog) :-
%%     Writes <Prog> into the stream <S>.
%% 
%% $pp_save_program(Prog,File) :-
%%     Writes <Prog> into <File>.
%% 
%%======================================================================

%%--------------------------------
%%  Entry Point

$pp_dump_program(Prog) :-
    current_output(S), $pp_dump_program(S,Prog).

$pp_save_program(Prog,File) :-
    open(File,write,S), $pp_dump_program(S,Prog), close(S).

$pp_dump_program(S,Prog) :-
    $pp_dump_split(Prog,Damon,Preds),
    $pp_dump_damon(S,Damon),
    $pp_dump_decls(S,Preds),
    $pp_dump_preds(S,Preds).


%%--------------------------------
%%  Separator

$pp_dump_nl(S,L) :-
    var(L), !,
    nl(S),
    L = 1.
$pp_dump_nl(_,L) :-
    nonvar(L), !.


%%--------------------------------
%%  Split $damon_load/0

$pp_dump_split(Prog,Damon,Preds) :-
    Q = pred($damon_load,0,_,_,_,[($damon_load :- Damon)|_]),
    select(Q,Prog,Preds), !.


%%--------------------------------
%%  Start-up Queries

$pp_dump_damon(S,Damon) :-
    $pp_dump_damon(S,Damon,_).

$pp_dump_damon(S,Damon,L) :-
    Damon = (A,B), !,
    $pp_dump_damon(S,A,L),
    $pp_dump_damon(S,B,L).
$pp_dump_damon(_,Damon,_) :-
    Damon = true, !.
$pp_dump_damon(S,Damon,L) :-
    Damon = $query(Query), !,
    $pp_dump_nl(S,L),
    \+ \+ $pp_dump_query(S,Query).

$pp_dump_query(S,Query) :-
    prettyvars(Query),
    format(S,":- ~k.~n",[Query]).


%%--------------------------------
%%  Declarations

$pp_dump_decls(S,Preds) :-
    $pp_dump_m_decls(S,Preds,_),
    $pp_dump_t_decls(S,Preds,_).


%%--------------------------------
%%  Mode Declarations

$pp_dump_m_decls(_,Preds,_) :- Preds == [], !.
$pp_dump_m_decls(S,Preds,L) :- Preds = [Pred|Preds1], !,
    Pred = pred(F,N,M,_,_,_),
    $pp_dump_m_decl(S,F,N,M,L),
    $pp_dump_m_decls(S,Preds1,L).

$pp_dump_m_decl(_,_,_,M,_) :- var(M), !.
$pp_dump_m_decl(S,F,N,M,L) :- M = [_|_], !,
    $pp_dump_nl(S,L),
    format(S,":- mode ~q(",[F]),
    $pp_dump_m_spec(S,N,M),
    format(S,").~n",[]).

$pp_dump_m_spec(S,N,Mode) :- N == 1, !,
    Mode = [M],
    $pp_mode_symbol(M,Sym), !,   % M can be an unbound variable
    write(S,Sym).
$pp_dump_m_spec(S,N,Mode) :- N >= 2, !,
    Mode = [M|Mode1],
    $pp_mode_symbol(M,Sym), !,   % M can be an unbound variable
    write(S,Sym),
    write(S,','),
    N1 is N - 1,
    $pp_dump_m_spec(S,N1,Mode1).

$pp_mode_symbol(d ,? ).
$pp_mode_symbol(? ,? ).
$pp_mode_symbol(c ,+ ).
$pp_mode_symbol(+ ,+ ).
$pp_mode_symbol(f ,- ).
$pp_mode_symbol(- ,- ).
$pp_mode_symbol(nv,nv).


%%--------------------------------
%%  Table Decalrations

$pp_dump_t_decls(_,Preds,_) :- Preds == [], !.
$pp_dump_t_decls(S,Preds,L) :- Preds = [Pred|Preds1], !,
    Pred = pred(F,N,_,_,T,_),
    $pp_dump_t_decl(S,F,N,T,L),
    $pp_dump_t_decls(S,Preds1,L).

$pp_dump_t_decl(_,_,_,T,_) :- var(T), !.
$pp_dump_t_decl(S,F,N,T,L) :- nonvar(T), !,
    $pp_dump_nl(S,L),
    format(S,":- table ~q/~d.~n",[F,N]).


%%--------------------------------
%%  Clauses

$pp_dump_preds(_,Preds) :- Preds == [], !.
$pp_dump_preds(S,Preds) :- Preds = [Pred|Preds1], !,
    Pred = pred(_,_,_,_,_,Cls),
    $pp_dump_clauses(S,Cls,_),
    $pp_dump_preds(S,Preds1).

$pp_dump_clauses(_,Cls,_) :- Cls == [], !.
$pp_dump_clauses(S,Cls,L) :- Cls = [Cl|Cls1], !,
    $pp_dump_nl(S,L),
    portray_clause(S,Cl),
    $pp_dump_clauses(S,Cls1,L).
