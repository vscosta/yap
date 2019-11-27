/**
  * @file spy.yap
 * @brief debugger operation.
 */
  :- system_module( '$_debug', [debug/0,
        debugging/0,
        leash/1,
        nodebug/0,
        (nospy)/1,
        nospyall/0,
        notrace/0,
        (spy)/1,
        trace/0], [
        '$init_debugger'/0]).

:- use_system_module( '$_boot', ['$find_goal_definition'/4,
        '$system_catch'/4]).

:- use_system_module( '$_errors', ['$Error'/1,
        '$do_error'/2]).

:- use_system_module( '$_init', ['$system_module'/1]).

:- use_system_module( '$_modules', ['$meta_expansion'/6]).

:- use_system_module( '$_preds', ['$clause'/4]).

/*-----------------------------------------------------------------------------

			Debugging / creating spy points

-----------------------------------------------------------------------------*/

/**
 * @defgroup DebSet Debugger Control
 * @ingroup Deb_Interaction

@{
The
 following predicates are available to control the debugging of
programs:

+ debug

    Switches the debugger on.

+ debugging


    Outputs status information about the debugger which includes the leash
mode and the existing spy-points, when the debugger is on.

+ nodebug

    Switches the debugger off.


*/


:- op(900,fx,[spy,nospy]).

% First part : setting and reseting spy points

 % $suspy does most of the work
 '$suspy'(V,S,M) :- var(V) , !,
	 '$do_error'(instantiation_error,M:spy(V,S)).
 '$suspy'((M:S),P,_) :- !,
     '$suspy'(S,P,M).
 '$suspy'([],_,_) :- !.
 '$suspy'([F|L],S,M) :- !, ( '$suspy'(F,S,M) ; '$suspy'(L,S,M) ).
 '$suspy'(F/N,S,M) :- !,
	 functor(T,F,N),
	 '$do_suspy'(S, F, N, T, M).
 '$suspy'(A,S,M) :- atom(A), !,
	 '$suspy_predicates_by_name'(A,S,M).
 '$suspy'(P,spy,M) :- !,
	  '$do_error'(domain_error(predicate_spec,P),spy(M:P)).
 '$suspy'(P,nospy,M) :-
	  '$do_error'(domain_error(predicate_spec,P),nospy(M:P)).

 '$suspy_predicates_by_name'(A,S,M) :-
	 % just check one such predicate exists
	 current_predicate(A,M:_),
	 !,
	 '$do_suspy_predicates_by_name'(A,S,M).
'$suspy_predicates_by_name'(A,spy,M) :- !,
	 print_message(warning,no_match(spy(M:A))).
'$suspy_predicates_by_name'(A,nospy,M) :-
	 print_message(warning,no_match(nospy(M:A))).

'$do_suspy_predicates_by_name'(A,S,M) :-
	 current_predicate(A,M:T),
	 functor(T,A,N),
	 '$do_suspy'(S, A, N, T, M),
	 fail.
'$do_suspy_predicates_by_name'(_A, _S, _M).


 %
 % protect against evil arguments.
 %
 '$do_suspy'(S, F, N, T, M) :-
	 recorded('$import','$import'(EM,M,T0,_,F,N),_), !,
	 functor(T0, F0, N0),
	 '$do_suspy'(S, F0, N0, T, EM).
 '$do_suspy'(S, F, N, T, M) :-
	  '$undefined'(T,M), !,
	  ( S = spy ->
	      print_message(warning,no_match(spy(M:F/N)))
	  ;
	      print_message(warning,no_match(nospy(M:F/N)))
	  ).
 '$do_suspy'(S, F, N, T, M) :-
	  '$is_system_predicate'(T,M),
	  '$predicate_flags'(T,M,F,F),
	  F /\ 0x118dd080 =\= 0,
	  ( S = spy ->
	      '$do_error'(permission_error(access,private_procedure,T),spy(M:F/N))
	  ;
	      '$do_error'(permission_error(access,private_procedure,T),nospy(M:F/N))
	  ).
 '$do_suspy'(S, F, N, T, M) :-
     '$undefined'(T,M), !,
	  ( S = spy ->
	      print_message(warning,no_match(spy(M:F/N)))
	  ;
	      print_message(warning,no_match(nospy(M:F/N)))
	  ).
 '$do_suspy'(S,F,N,T,M) :-
	 '$suspy2'(S,F,N,T,M).

 '$suspy2'(spy,F,N,T,M) :-
	 recorded('$spy','$spy'(T,M),_), !,
	 print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,already)).
 '$suspy2'(spy,F,N,T,M) :- !,
	 recorda('$spy','$spy'(T,M),_),
	 '$set_spy'(T,M),
	 print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),add,ok)).
 '$suspy2'(nospy,F,N,T,M) :-
	 recorded('$spy','$spy'(T,M),R), !,
	 erase(R),
	 '$rm_spy'(T,M),
	 print_message(informational,breakp(bp(debugger,plain,M:T,M:F/N,N),remove,last)).
 '$suspy2'(nospy,F,N,_,M) :-
	 print_message(informational,breakp(no,breakpoint_for,M:F/N)).

 '$pred_being_spied'(G, M) :-
	 recorded('$spy','$spy'(G,M),_), !.

/**
@pred spy( + _P_ ).

Sets spy-points on all the predicates represented by
 _P_.  _P_ can either be a single specification or a list of
specifications. Each one must be of the form  _Name/Arity_
or  _Name_. In the last case all predicates with the name
 _Name_ will be spied. As in C-Prolog, system predicates and
predicates written in C, cannot be spied.


*/
 spy Spec :-
	 '$init_debugger'(zip),
	 prolog:debug_action_hook(spy(Spec)), !.
 spy L :-
	 '$current_module'(M),
	 '$suspy'(L, spy, M), fail.
 spy _ :- debug.

/** @pred nospy( + _P_ )


Removes spy-points from all predicates specified by  _P_.
The possible forms for  _P_ are the same as in `spy P`.


*/
 nospy Spec :-
	 '$init_debugger'(zip),
	 prolog:debug_action_hook(nospy(Spec)), !.
 nospy L :-
	 '$current_module'(M),
	 '$suspy'(L, nospy, M), fail.
nospy _.

/** @pred nospyall

Removes all existing spy-points.
*/
nospyall :-
	 '$init_debugger'(zip),
	 prolog:debug_action_hook(nospyall), !.
nospyall :-
	 recorded('$spy','$spy'(T,M),_), functor(T,F,N), '$suspy'(F/N,nospy,M), fail.
nospyall.

 % debug mode -> debug flag = 1
/** @pred debug

Enables the Prolof debugging. Notice that tracing is disabled, even if it was active.
*/
debug :-
	 ( '__NB_getval__'('$spy_gn',_, fail) -> true ; '__NB_setval__'('$spy_gn',1) ),
	 set_prolog_flag(debug,true),
	 '$set_debugger_state'(debug, true),
	 '__NB_setval__'('$trace',off),
	 '$start_user_code',
	 print_message(informational,debug(debug)),
	 '$init_debugger'(zip).

'$start_user_code' :-
    current_prolog_flag(debug, Can),
    '$set_debugger_state'(debug, Can),
    '$stop_creeping'(_).

nodebug :-
	set_prolog_flag(debug, false),
	 '$set_debugger_state'(debug, false),
	 '__NB_setval__'('$trace',off),
	 print_message(informational,debug(off)).

%
% remove any debugging info after an abort.
%


/** @pred trace


Switches on the debugger and enters tracing mode.


*/
trace :-
    ( '__NB_getval__'('$spy_gn',_, fail) -> true ; '__NB_setval__'('$spy_gn',1) ),
    print_message(informational,debug(trace)),
    set_prolog_flag(debug,true),
    '$set_debugger_state'(debug, true),
    '__NB_setval__'('$trace',on),
    '$init_debugger'(creep).

/** @pred notrace


Ends tracing and exits the debugger. This is the same as
nodebug/0.
 */
notrace :-
	nodebug.

/*-----------------------------------------------------------------------------

				leash

  -----------------------------------------------------------------------------*/


/** @pred leash(+ _M_)


Sets leashing mode to  _M_.
The mode can be specified as:

+ `full`
prompt on Call, Exit, Redo and Fail

+ `tight`
prompt on Call, Redo and Fail

+ `half`
prompt on Call and Redo

+ `loose`
prompt on Call

+ `off`
never prompt

+ `none`
never prompt, same as `off`

The initial leashing mode is `full`.

The user may also specify directly the debugger ports
where he wants to be prompted. If the argument for leash
is a number  _N_, each of lower four bits of the number is used to
control prompting at one the ports of the box model. The debugger will
prompt according to the following conditions:

+ if `N/\ 1 =\= 0`  prompt on fail
+ if `N/\ 2 =\= 0` prompt on redo
+ if `N/\ 4 =\= 0` prompt on exit
+ if `N/\ 8 =\= 0` prompt on call

Therefore, `leash(15)` is equivalent to `leash(full)` and
`leash(0)` is equivalent to `leash(off)`.

Another way of using `leash` is to give it a list with the names of
the ports where the debugger should stop. For example,
`leash([call,exit,redo,fail])` is the same as `leash(full)` or
`leash(15)` and `leash([fail])` might be used instead of
`leash(1)`.

 @}

*/
leash(X) :- var(X),
	'$do_error'(instantiation_error,leash(X)).
leash(X) :-
	'$init_debugger'(zip),
	'$leashcode'(X,Code),
	set_value('$leash',Code),
	'$show_leash'(informational,Code), !.
leash(X) :-
	'$do_error'(type_error(leash_mode,X),leash(X)).

'$show_leash'(Msg,0) :-
	print_message(Msg,leash([])).
'$show_leash'(Msg,Code) :-
	'$check_leash_bit'(Code,0x8,L3,call,LF),
	'$check_leash_bit'(Code,0x4,L2,exit,L3),
	'$check_leash_bit'(Code,0x2,L1,redo,L2),
	'$check_leash_bit'(Code,0x1,[],fail,L1),
	print_message(Msg,leash(LF)).

'$check_leash_bit'(Code,Bit,L0,_,L0) :- Bit /\ Code =:= 0, !.
'$check_leash_bit'(_,_,L0,Name,[Name|L0]).

'$leashcode'(full,0xf) :- !.
'$leashcode'(on,0xf) :- !.
'$leashcode'(half,0xb) :- !.
'$leashcode'(loose,0x8) :- !.
'$leashcode'(off,0x0) :- !.
'$leashcode'(none,0x0) :- !.
%'$leashcode'([L|M],Code) :- !, '$leashcode_list'([L|M],Code).
'$leashcode'([L|M],Code) :- !,
	'$list2Code'([L|M],Code).
'$leashcode'(N,N) :- integer(N), N >= 0, N =< 0xf.

'$list2Code'(V,_) :- var(V), !,
	'$do_error'(instantiation_error,leash(V)).
'$list2Code'([],0) :- !.
'$list2Code'([V|L],_) :- var(V), !,
	'$do_error'(instantiation_error,leash([V|L])).
'$list2Code'([call|L],N) :- '$list2Code'(L,N1), N is 0x8 + N1.
'$list2Code'([exit|L],N) :- '$list2Code'(L,N1), N is 0x4 + N1.
'$list2Code'([redo|L],N) :- '$list2Code'(L,N1), N is 0x2 + N1.
'$list2Code'([fail|L],N) :- '$list2Code'(L,N1), N is 0x1 + N1.

/*-----------------------------------------------------------------------------

				debugging

-----------------------------------------------------------------------------*/

debugging :-
	'$init_debugger'(zip),
	prolog:debug_action_hook(nospyall), !.
debugging :-
	( current_prolog_flag(debug, true) ->
	    print_message(help,debug(debug))
	    ;
	    print_message(help,debug(off))
	),
	findall(M:(N/A),(recorded('$spy','$spy'(T,M),_),functor(T,N,A)),L),
	print_message(help,breakpoints(L)),
	get_value('$leash',Leash),
	'$show_leash'(help,Leash).

notrace(G) :-
	 strip_module(G, M, G1),
	 ( '$$save_by'(CP),
	   '$debug_stop'( State ),
	   '$call'(G1, CP, G, M),
	   '$$save_by'(CP2),
	   (CP == CP2 -> ! ; '$debug_state'( NState ), ( true ; '$debug_restart'(NState), fail ) ),
	   '$debug_restart'( State )
     ;
	'$debug_restart'( State ),
	fail
    ).

'$creep_at_port'(retry) :-
    '$get_debugger_state'(debug, true),
    '__NB_getval__'('$trace',Trace,fail),
    Trace = on,
    !,
    '$enable_debugging'.
'$creep_at_port'(fail) :-
    '$get_debugger_state'(debug, true),
   '__NB_getval__'('$trace',Trace,fail),
    Trace = on,
    !,
    '$enable_debugging'.

'$init_debugger' :- 
    '$init_debugger'(zip).

'$init_debugger'(Creep) :- 
    '$debugger_io',
    '$init_debugger_trace'(Creep),
    '__NB_setval__'('$if_skip_mode',no_skip),
    '__NB_setval__'('$spy_glist',[]),
    '__NB_setval__'('$spy_gn',1).
 
'$init_debugger_trace'(creep) :-
	'__NB_setval__'('$trace',on),
	!,
	'$set_debugger_state'( creep,  0, stop, on, true ).
'$init_debugger_trace'(zip) :-
	'__NB_setval__'('$trace',off),
	'$set_debugger_state'( zip, 0, stop, off, true ).

%% @pred $enter_debugging(G,Mod,CP,G0,NG)
%%
%% Internal predicate called by top-level;
%% enable creeping on a goal by just switching execution to debugger.
%%
'$enter_debugging'(G,Mod,_CP,_G0,_NG) :-
    '$creepcalls'(G,Mod),
    !.
'$enter_debugging'(G,_Mod,_CP,_G0,G).

'$enter_debugging'(G,Mod,GN) :-
    current_prolog_flag( debug, Deb ),
    '$set_debugger_state'( debug, Deb ),
    ( Deb = false
    ->
    true
    ;
    '$stop_at_this_goal'(G,Mod,GN)
    ->
    '$creep'
    ;
    true
    ).

'$exit_debugger'(Mod:G, GN) :-
    current_prolog_flag( debug, Deb ),
    '$set_debugger_state'( debug, Deb ),
    ( Deb = false
    ->
    true
    ;
    '$stop_at_this_goal'(G,Mod,GN)
    ->
    '$creep'
    ;
    true
    ).

'$enable_debugging' :-
    '$re_enter_creep_mode'.
 
 
%% @pred $re_enter_creep_mode1
%%
%% Internal predicate called when exiting through a port;
%% enable creeping on the next goal.
%%

'$re_enter_creep_mode' :-
    current_prolog_flag( debug, Deb ),
    '$set_debugger_state'( debug, Deb ),
    '$creep'.


'$cannot_debug'(G, Module, GoalNo) :-
     (
	 current_prolog_flag( debug, false )
    ;
      '$is_private'(G,Module)
     ;
      functor(G,Na,_), atom_concat('$',_,Na)
    ;
      \+ '$debuggable'(G, Module,GoalNo)
     ).

/**
  * @pred $stop_at_this_goal( Goal, Module, Id)
  *
  * debugger should prompt the user if:
  * - creep on
  * - spy point enabled
  * - the goal is older than ourselves: Id is bound
  *   and Id <= StateGoal
  *
  */
'$dotrace'(G,Module, GoalNo) :-
    current_prolog_flag( debug, true ),
    \+ '$is_private'(G,Module),
    '$debuggable'(G, Module,GoalNo).

'$debuggable'(G, Module,_GoalNo) :-
    '$pred_being_spied'(G,Module),
    '$get_debugger_state'( spy,  stop ),
    !.
'$debuggable'(_G, _Module,GoalNo) :-
	'$get_debugger_state'( creep, zip ),
    !,
    nonvar(GoalNo),
    '$get_debugger_state'( goal_number, TargetGoal ),
    noncar(TargetGoal),
    GoalNo =< TargetGoal.
'$debuggable'(_G, _Module,_GoalNo).

'$leap'(GoalNo) :-
    '$get_debugger_state'( creep, leap),
    !,
    (
     var(GoalNo)
    ->
     true
    ;
     '$get_debugger_state'( goal_number, TargetGoal ),
     number(GoalNo),
     number(TargetGoal), 
     GoalNo > TargetGoal                                                                                             
    ).


'$run_deb'(Port,GN0,GN) :-
    '$stop_creeping'(_),
    '$cross_run_deb'(Port,GN0,GN).

'$cross_run_deb'(call,_Ctx,_GN).
'$cross_run_deb'(internal,_Ctx,_GN).
'$cross_run_deb'(redo,Ctx,_GN) :-
    '$continue_debugging'(Ctx).
'$cross_run_deb'(fail,Ctx,_GN) :-
        '$continue_debugging'(Ctx).
'$cross_run_deb'(exit,Ctx,_GN) :-
    '$continue_debugging'(Ctx).
'$cross_run_deb'(answer,Ctx,_GN) :-
	'$continue_debugging'(Ctx).
'$cross_run_deb'(exception(_),_GN0,_GN) :-
	'$set_debugger_state'(debug,false).
'$cross_run_deb'(external_exception(_),_GN0,_GN) :-
	'$set_debugger_state'(debug,false).

'$exit_goal'(false, _GN) :-
	'$set_debugger_state'(debug,false).
'$exit_goal'(true, GN):-
    '$continue_debugging'(GN).

'$continue_debugging'(_) :-
    current_prolog_flag(debug, false),
    !.
'$continue_debugging'(_) :-
    '__NB_getval__'('$trace',on, fail),
    '$get_debugger_state'(creep,zip),
    '$set_debugger_state'(creep,creep),
    fail.
'$continue_debugging'(outer) :-
    '$set_debugger_state'(debug,true),
    	!,
    '$creep'.
'$continue_debugging'(inner).

'$restart_debugging':-
    '$set_debugger_state'(debug,Debug),
    '$get_debugger_state'(creep,Creep),
    '$may_creep'(Debug,Creep),
    !,
    '$creep'.
'$restart_debugging'.

'$may_creep'(true,creep).
'$may_creep'(true,leap).

/**

@}

*/
