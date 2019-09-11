11

%% @file yapi.yap
%% @brief support yap shell
%%

%% :- module(yapi, [
%% 		 python_ouput/0,
%% 		 show_answer/2,
%% 		 show_answer/3,
%% 		 yap_query/4,
%% 		 python_query/2,
%% 		 python_query/3,
%% 		 python_import/1,
%% 		 yapi_query/2
%% 		 ]).

%:- yap_flag(verbose, silent).

:- reexport(library(python)).

:- use_module( library(lists) ).
:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).
:- use_module( library(terms) ).


:- python_import(
			' yap4py.yapi' ).
:- python_import(
       json).
%:- python_import(gc).

/**
* @brief enable query output as text, in Prolog style.
*
*/
:- create_prolog_flag(yap4py_query_output, true, [access(read_write)]).
/**
* @brief enable query output as JSON.
*
*/
:- create_prolog_flag(yap4py_query_json, true, [access(read_write)]).

:- meta_predicate yapi_query(:,+), python_query(+,:), python_query(+,:,-) .

%:- start_low_level_trace.

%% @pred yapi_query( + VarList, - Dictionary)
%%
%% dictionary, Examples
%%
%%
yapi_query( VarNames, Caller ) :-
show_answer(VarNames, Dict),
Caller.bindings := Dict,
:= print(Dict).




%:- initialization set_preds.

set_preds :-
fail,
current_predicate(P, Q),
functor(Q,P,A),
atom_string(P,S),
catch(
:= yap4py.yapi.named( S, A),
_,
fail),
fail.
set_preds :-
fail,
system_predicate(P/A),
atom_string(P,S),
catch(
:= yap4py.yapi.named( S, A),
_,
fail),
fail.
set_preds.

argi(N,I,I1) :-
atomic_concat('A',I,N),
I1 is I+1.

:- meta_predicate python_query(:,-),
		  python_query(:,?, ?).

python_query( String		) :-
    python_query( String, _, _, _Bindings).

python_query( M:String,M:Goal,Status,FinalBindings ) :-
    atomic_to_term( String, Goal, VarNames ),
    query_to_answer( M:Goal, VarNames, Status, Bindings),
    rational_term_to_tree(Goal+Bindings,_NGoal+NBindings,ExtraBindings,[]),
    lists:append(NBindings,ExtraBindings,L),
    simplify(L,[],L2),
    lists2dict(L2, FinalBindings).

lists2dict([A=B], { A:NB}) :-
    !,
    listify(B,NB).
lists2dict([A=B|L], {A:NB,Dict}) :-
    lists2dict(L, {Dict}),
    listify(B,NB).

simplify([],_,[]).
simplify([X=V|Xs], I, NXs) :-
    var(V),
    !,
    X=V,
    simplify(Xs,I, NXs).
simplify([X=V|Xs], I, [X=V|NXs]) :-
    !,
    simplify(Xs,I,NXs).
simplify([G|Xs],I, [D=G|NXs]) :-
    I1 is I+1,
    atomic_concat(['__delay_',I,'__'],D),
    simplify(Xs,I1,NXs).


bv(V,I,I1) :-
atomic_concat(['__',I],V),
I1 is I+1.

listify(X,X) :-
    atomic(X),
    !.
listify('$VAR'(Bnd), V)  :-
    !,
    listify_var(Bnd, V).
listify([A|As], V)  :-
    !,
    maplist(listify,[A|As], V).
listify(A:As, A:Vs)  :-
    (atom(A);string(A)),
    !,
    maplist(listify,As, Vs).
listify({Xs}, {NXs}) :-
    !,
    listify(Xs,NXs).
listify(WellKnown, V)  :-
    WellKnown=..[N|As],
    length(As,Sz),
    well_known(N,Sz),
    !,
    maplist(listify,As, Vs),
    V =.. [N|Vs].
listify(T, [N,LAs])  :-
    T=..[N|As],
    listify(As, LAs).

listify_var(I, S) :-
    I >= 0,
    I =< 26,
    !,
    V is 0'A+I,
    string_codes(S, [V]).
listify_var(I, S) :-
    I < 0,
    I >= -26,
    !,
    V is 0'A+I,
    string_codes(S, [0'_+V]).
listify_var(S, S).

well_known(+,2).
well_known(-,2).
well_known(*,2).
well_known(/,2).
well_known((','),2).



