%% @file yapi.yap
%% @brief support yap shell
%%



 :- module(yapi, [
	       python_ouput/0,
	       python_query/2 as python_show_query,
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
       yap4py.predicates ).
:- python_import(
       yap4py.queries ).
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

:- meta_predicate user:top_query(+,:),
		  yapi_query(+,:),
		  yapi_query(0,?,+).

user:top_query( Self, MString		) :-
    yapi_query( Self, MString	).

yapi_query( Engine, MString) :-
    strip_module(MString,M,String),
    atomic_to_term( String, Goal, Vs ),
    catch( yapi_query( M:Goal, Vs, Engine ), E, writeln(E)).

yapi_query( Goal, Vs, Engine ) :-
    Query = Engine.q,
    gated_call(
	current_source_module(O,user),
	user:Goal,
	    Gate,
	    gate(Query,Gate, O)
    ),
    %success
    !,
     all_attvars(AVs),
    attributes:delayed_goals(Goal+AVs, Vs, NVs,Delays),
    (current_prolog_flag(yap4py_query_json, true),
     Vs = [_|_]
    ->
	term_to_dict(Vs, Goal,  NVs,Delays),
	Query.bindings := json.dumps(NVs),				     Query.delays := json.dumps(Delays)				      ;
	    Query.bindings := [],
	    Query.delays := []
	   ),
	   (current_prolog_flag(yap4py_query_output, true)
	   ->
	       report(Gate,Vs,NVs,Delays)
	   ;
	   true
	   ).

gate(Query,Gate,M) :-
    atom_string(Gate,SGate),
    Query.gate := SGate,
	  current_source_module(M,_).

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



