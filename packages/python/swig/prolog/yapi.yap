
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
:- python_import(json).
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
	       answer := {},
	foldl(ground_dict(answer), Bindings, [], Ts),
	term_variables( Ts, Hidden),
	foldl(bv, Hidden , 0, _),
    maplist(into_dict(answer),Ts),
    Caller.answer := json.dumps(answer),
			  S := Caller.answer,
format(user_error, '~nor ~s~n~n',S).


bv(V,I,I1) :-
    atomic_concat(['__',I],V),
    I1 is I+1.

into_dict(D,V0=T) :-
    D[V0] := T.
    
/**
 *
 */
ground_dict(_Dict, var([V,V]), I, I) :- 
    !.
ground_dict(Dict, nonvar([V0|Vs], T),I0, [V0=T| I0]) :- 
    !,
    ground_dict( Dict, var([V0|Vs]), I0, I0).
ground_dict(Dict, var([V0,V|Vs]), I, I) :- 
    !,
    Dict[V]=V0,
    ground_dict( Dict, var([V0|Vs]), I, I).
ground_dict(_, _, _, _).


bound_dict(Dict, nonvar([V0|Vs], T)) :- 
    !,
    Dict[V0] := T,
    bound_dict( Dict, var([V0|Vs])).
bound_dict(Dict, var([V0,V|Vs])) :- 
    !,
    Dict[V] := V0,
    bound_dict( Dict, var([V0|Vs])).
bound_dict(_, _).

