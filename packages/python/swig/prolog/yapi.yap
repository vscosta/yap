
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
 

:- python_import(yap4py.yapi).
:- python_import(json).
%:- python_import(gc).

:- create_prolog_flag(yap4py_query_output, true, [access(read_write)]).
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
		Caller.bindings := Dict.




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

python_query( Caller, String		) :-
    python_query( Caller, String, _Bindings).

python_query( Caller, String, Bindings ) :-
	atomic_to_term( String, Goal, VarNames ),
	query_to_answer( user:Goal, VarNames, Status, Bindings),
	Caller.q.port := Status,
		 rational_term_to_tree(Caller+Bindings,_NGoal+NBindings,ExtraBindings,[]),
		 lists:append(NBindings, ExtraBindings, TotalBindings),
		 copy_term_nat(TotalBindings,L),
		 term_variables(L,Vs),
numbervars(Vs,0,_),
		 output(Caller, L).



output( _, Bindings ) :-
    yap_flag(yap4py_query_output,true),
    once( write_query_answer( Bindings ) ),
    nl(user_error),
    nl(user_error),
    fail.
output( Caller, Bindings) :-
    yap_flag(yap4py_query_json,true),
    !,
    simplify(Bindings, 1, Bss),
    numbervars(Bss, 0, _),
    maplist(into_dict(Caller),Bss).
output( _Caller, _Bindings).

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

into_dict(D,V0=T) :-
    listify(T,L),
    D.q.answer[V0] := L.

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
listify({Xs}, I, NXs) :-
	!,
	simplify(Xs,I,NXs).
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

