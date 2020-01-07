%% @file yapi.yap
%% @brief support yap shell
%%

 :- module(yapi, [
 		 python_ouput/0,
%% 		 show_answer/2,
%% 		 show_answer/3,
 		 python_query/2,
 		 python_query/3,
 		 python_query/4,
 		 python_import/1,
 		 yapi_query/2
 		 ]).

%:- yap_flag(verbose, silent).

:- reexport(library(python)).

:- use_module( library(lists) ).
:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).
:- use_module( library(terms) ).


:- python_import(
       yap4py.yapi ).
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

:- meta_predicate python_query(:,?.?), python_query(:.?,?,?) .

%:- start_low_level_trace.




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

:- meta_predicate python_query(+,:),
	python_query(:,-,-),
	python_query(:,-,-,-).

python_query( Self, MString) :-
 	strip_module(MString, M, String),
    python_query( M:String, _, Gate, Bindings),
    gate(Gate,Self,Bindings).

gate(Gate,Self,Bindings) :-
    Self.port := Gate,
	   Self.answer := Bindings.

python_query( String, Status, Bindings		) :-
    python_query( String, _, Status, Bindings).

python_query( MString, M:Goal, Status, FinalBindings  ) :-
	strip_module(MString, M, String),
	atomic_to_term( String, Goal, VarNames ),
	query(M:Goal, VarNames, Status, FinalBindings).

/*
    rational_term_to_forest(Goal+Bindings,NGoal+NBindings,ExtraBindings,{}),
    simplify(NBindings,0,L2,I2),
    non_singletons_in_term(Goal, [], NSVs),
    foldl(namev,NSVs,I2,_),
    term_variables(Goal,Vs),
    maplist('='('_'),Vs),
    lists:append(L2,ExtraBindings,L),
    lists2dict(L, FinalBindings).
*/

lists2dict([A=B], { A:NB}) :-
    !,
    listify(B,NB).
lists2dict([A=B|L], {A:NB,Dict}) :-
    lists2dict(L, {Dict}),
    listify(B,NB).

simplify([],I,[],I).
simplify([X=V|Xs], I, NXs, IF) :-
    var(V),
    !,
    X=V,
    simplify(Xs,I, NXs, IF).
simplify([X=V|Xs], I, [X=V|NXs], IF) :-
    var(X),
    !,
    namev(X,I,I1),
    simplify(Xs,I1,NXs, IF).
simplify([X=V|Xs], I, [X=V|NXs], IF) :-
    !,
    simplify(Xs,I,NXs, IF).

namev(V,I,I1) :-
    atomic_concat(['_',I],V),
    I1 is I+1.

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



