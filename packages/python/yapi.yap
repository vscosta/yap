%% @file yapi.yap
%% @brief support yap shell
%%



 :- module(yapi, [
    python_ouput/0,
  		 yapi_query/2,
		 yapi_query/2 as python_query,
		 yapi_query/2 as python_show_query,
 		 python_import/1,
		 term_to_dict/4
 		 ]).

:- yap_flag(verbose_load, false).

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
:- create_prolog_flag(yap4py_query_json, false, [access(read_write)]).

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

:- meta_predicate yapi_query(+,:).

user:yapi_query( Self, MString		) :-
 	yapi_query( Self, MString	).

yapi_query( Self, MString) :-
	strip_module(MString, M, String),
	atomic_to_term( String, Goal, VarNames ),
	Answer := Self.q.answer,
	gated_call(
	    true,
	    run(M:Goal, VarNames, Vs, Gs),
	    Gate,
	    gate(Answer,Gate, VarNames, Vs,Gs)
	    ).

run(end_of_file, [], [], []) :-
    !.
run(G, Vs, NVs, Gs) :- 
    call(G),
    attributes:delayed_goals(G, Vs, NVs, Gs).

gate(Answer,Gate,VarNames, Vs,Gs) :-
    
    atom_string(Gate,SGate),
    Answer.gate := SGate,
    (current_prolog_flag(yap4py_query_json, true)
->
      term_to_dict(Vs, Gs,  Bindings,Delays),
	       Answer.bindings := json.dumps(Bindings),				     Answer.delays := json.dumps(Delays)				     ;
	    Answer.bindings := [],
	    Answer.delays := []
	   ),
	   (current_prolog_flag(yap4py_query_output, true)
	   ->
	       report(Gate,VarNames,Vs,Gs)
	   ;
	   true
	   ).

report(exit,VarNames,Vs,Gs) :- 
    print_message(help, answer(VarNames, Vs,Gs)).
report(answer,VarNames,Vs,Gs) :- 
     print_message(help, answer(VarNames, Vs,Gs)).
report(fail,_VarNames,_Vs,_Gs) :- 
    print_message(help,no).
				

term_to_dict(Vs,LGs,Dict,NGs) :-
    sort(Vs, NVs),
  append(NVs,LGs,LAnsws),
  term_factorized(LAnsws,B1,More),
  append(B1,More,VGs),
  foldl2(dddv,VGs, [], Bindings, [], NGs ),
  term_variables(Bindings+NGs, GGVs),
  foldl(set_v,GGVs,0,_),
  lists2dict(Bindings, Dict).

dddv(Name=_V, Bindings, Bindings, Gs, Gs ) :-
  sub_atom('_',0,1,0,Name),
  !.
dddv(Name=V, Bindings, Bindings, Gs, Gs ) :-
  sub_atom('_',0,1,_,Name),
  var(V),
  !,
  atom_string(Name,V).
dddv(Name=_V, Bindings, Bindings, Gs, Gs ) :-
  sub_atom('_',0,1,_,Name),
  !.
dddv(Name=V, Bindings, Bindings, Gs, Gs ) :-
  var(V),
  !,
  atom_string(Name,V).
dddv(Name=V, Bindings, [NV=V|Bindings], Gs, Gs ) :-
  !,
  atom_string(Name,NV).
dddv(G, Bindings, Bindings, Gs, [G|Gs] ).
  
  set_v(V,I,I1) :-
  I1 is I+1,
  format(string(V),`_~d`,I).

lists2dict([], { }).
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
    I < 26,
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



