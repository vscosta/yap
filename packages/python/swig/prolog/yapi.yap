
%% @file yapi.yap
%% @brief support yap shell
%%
%:- start_low_level_trace.
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

:- yap_flag(verbose, silent).

 :- use_module(library(python)).

:- use_module( library(lists) ).
:- use_module( library(maplist) ).
:- use_module( library(rbtrees) ).
:- use_module( library(terms) ).


:- python_import(yap4py.yapi).
%:- python_import(gc).

:- meta_predicate( yapi_query(:,+) ).

%:- start_low_level_trace.

	%% @pred yapi_query( + VarList, - Dictionary)
	%%
	%% dictionary, Examples
	%%
	%%
	yapi_query( VarNames, Self ) :-
		show_answer(VarNames, Dict),
		Self.bindings := Dict.


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
    atomic_concat(`A`,I,N),
	I1 is I+1.

python_query( Caller, String ) :-
	atomic_to_term( String, Goal, VarNames ),
	query_to_answer( Goal, VarNames, Status, Bindings),
	Caller.port := Status,
	write_query_answer( Bindings ),
		nl(user_error),
	maplist(in_dict(Caller.answer, Bindings), Bindings).

/**
 *
 */
in_dict(_Dict, _, var([_V0])) :- 
	!.
in_dict(Dict, Bindings, var([V0,V|Vs])) :- 
	!,
	atom_to_string(V0,S0),
	atom_to_string(V,S),
	Dict[S] := S0,
	in_dict( Dict, Bindings, var([V0|Vs])).
in_dict(Dict, Bindings, nonvar([V0|Vs], T)) :- 
	!,
	atom_to_string(V0,S0),
	term_to_string(T, S, _Bindings),
	Dict[S0] := S,
	in_dict( Dict, Bindings, var([V0|Vs])).
in_dict(_, _, _).

