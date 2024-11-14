/**
  * @file spy.yap
 * @brief debugger operation.
 */
:- system_module_( '$_debug', [debug/0,
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
        throw_error/2]).

:- use_system_module( '$_init', ['$system_module'/1]).

:- use_system_module( '$_modules', ['$meta_expansion'/6]).

:- use_system_module( '$_preds', ['$clause'/4]).

:- multifile prolog:debug_action_hook/1.

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

 % $u_spy does most of the work
'$u_spy'(V,S,M) :-
    var(V) , !,
    throw_error(instantiation_error,M:spy(V,S)).
'$u_spy'((M:S),P,_) :- !,
    '$u_spy'(S,P,M).
'$u_spy'([],_,_) :- !.
'$u_spy'([F|L],S,M) :- !, ( '$u_spy'(F,S,M) ; '$u_spy'(L,S,M) ).
'$u_spy'(F/N,S,M) :- !,
    functor(T,F,N),
    '$imported_predicate'(M:T,M0:T0),
    functor(T0,F0,N0),
   '$do_u_spy'(S, F0, N0, T0, M0).
'$u_spy'(A,S,M) :- atom(A), !,
    '$u_spy_name'(A,S,M).
'$u_spy'(P,spy,M) :-
    !,
    throw_error(type_error(predicate_indicator,P),spy(M:P)).
'$u_spy'(P,nospy,M) :-
    throw_error(type_error(predicate_indicator,P),nospy(M:P)).

'$u_spy_name'(A0,Command,M0) :-
    strip_module(M0:A0,M,A),
 	(
      '$spy_gen'(M,A,S,N)
	*->
	'$u_spy'(A/N,Command,M),
	fail
    ;
    Error =..[S,M:A],
    print_message(warning,no_match(Error))
    ).

'$spy_gen'(M,A,S,N) :-
    atom(A),
    !,
    current_predicate(A,M:S),
    functor(S,A,N).

 %
 % protect against evil arguments.
 %
'$do_u_spy'(spy,A,N,T,M) :-
    recorded('$spy','$spy'(T,M),_),
    !,
    print_message(informational,breakp(bp(debugger,plain,M,A,N),add,already)).
'$do_u_spy'(spy,F,N,T,M) :-
    !,
    recorda('$spy','$spy'(T,M),_),
    '$set_spy'(T,M),
    print_message(informational,breakp(bp(debugger,plain,M,F,N),add,ok)).
'$do_u_spy'(nospy,F,N,T,M) :-
    recorded('$spy','$spy'(T,M),R), !,
    erase(R),
    '$rm_spy'(T,M),
    print_message(informational,breakp(bp(debugger,plain,M,F,N),remove,last)).
'$do_u_spy'(nospy,F,N,_,M) :-
    print_message(informational,breakp(no,breakpoint_for,M:F/N)).

'$pred_being_spied'(G, M) :-
    recorded('$spy','$spy'(G,M),_), !.

/**
@pred spy( + _P_ ).

Sets spy-points on all the predicates represented by
 _P_.  _P_ can either be a single specification or a list of
specifications. Each one must be of the form  _Name/Arity_
or  _Name_. In the last case all predicates with the name
 _Name_ will be spied. As in C-Prolog, system predicates andpredicates written in C, cannot be spied.
 
*/
spy Spec :-
    '$init_debugger',
	 prolog:debug_action_hook(spy(Spec)), !.
spy L :-
    '$current_module'(M),
    '$u_spy'(L, spy, M), fail.
spy _ :-
    debug.

/** @pred nospy( + _P_ )


Removes spy-points from all predicates specified by  _P_.
The possible forms for  _P_ are the same as in `spy P`.


*/
nospy Spec :-
    '$init_debugger',
	 prolog:debug_action_hook(nospy(Spec)), !.
nospy L :-
    '$current_module'(M),
    '$u_spy'(L, nospy, M), fail.
nospy _.

/** @pred nospyall

Removes all existing spy-points.
*/
nospyall :-
    '$init_debugger',
    prolog:debug_action_hook(nospyall), !.
nospyall :-
    recorded('$spy','$spy'(T,M),_), functor(T,F,N),
    '$u_spy'(F/N,nospy,M), fail.
nospyall.

 % debug mode -> debug flag = 1
/** @pred debug

Enables the Prolof debugging. Notice that tracing is disabled, even if it was active.
*/
debug :-
    ( '__NB_getval__'('$spy_gn',_, fail) -> true ; '__NB_setval__'('$spy_gn',1) ),
    set_prolog_flag(debug,true),
    set_prolog_flag(trace,false),
    '$start_user_code',
    print_message(informational,debug(debug)),
    '$init_debugger'.

'$start_user_code'.

nodebug :-
    set_prolog_flag(debug, false),
    set_prolog_flag(trace,false),
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
    '$init_debugger',
    set_prolog_flag(debug,true),
    set_prolog_flag(trace,true),
    nb_setval(creep,creep).


/** @pred notrace


Ends tracing and exits the debugger. This is the same as
nodebug/0.
 */
notrace :
    set_prolog_flag(trace,false),
    print_message(informational,debug(off)).

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

 
*/
leash(X) :- var(X),
    throw_error(instantiation_error,leash(X)).
leash(X) :-
    '$init_debugger',
    '$leashcode'(X,Code),
    set_value('$leash',Code),
    '$show_leash'(informational,Code), !.
leash(X) :-
    throw_error(type_error(leash_mode,X),leash(X)).

'$show_leash'(Msg,0) :-
    print_message(Msg,leash([])).
'$show_leash'(Msg,Code) :-
    '$check_leash_bit'(Code,0x8,L3,call,LF),
    '$check_leash_bit'(Code,0x4,L2,exit,L3),
    '$check_leash_bit'(Code,0x2,L1,redo,L2),
    '$check_leash_bit'(Code,0x1,[],fail,L1),
    print_message(Msg,leash(LF)).

'$has_leash'(Port) :-
    get_value('$leash',L),
    '$leash_id'(Port, Bit),
    Bit /\L =:= Bit.

'$leash_id'(exception(_),0x10).
'$leash_id'(call,0x8).
'$leash_id'(redo, 0x4).
'$leash_id'(fail, 0x2).
'$leash_id'(exit, 0x1).

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
    throw_error(instantiation_error,leash(V)).
'$list2Code'([],0) :- !.
'$list2Code'([V|L],_) :- var(V), !,
    throw_error(instantiation_error,leash([V|L])).
'$list2Code'([call|L],N) :- '$list2Code'(L,N1), N is 0x8 + N1.
'$list2Code'([exit|L],N) :- '$list2Code'(L,N1), N is 0x4 + N1.
'$list2Code'([redo|L],N) :- '$list2Code'(L,N1), N is 0x2 + N1.
'$list2Code'([fail|L],N) :- '$list2Code'(L,N1), N is 0x1 + N1.

/*-----------------------------------------------------------------------------

				debugging

-----------------------------------------------------------------------------*/

debugging :-
    '$init_debugger',
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

/*
notrace(G) :-
	 strip_module(G, M, G1),
	 ( current_choice_point(CP),
	   '$debug_stop'( State ),
	   '$call'(G1, CP, G, M),
	   current_choice_point(CP2),
	   (CP == CP2 -> ! ; '$debug_state'( NState ), ( true ; '$debug_restart'(NState), fail ) ),
	   '$debug_restart'( State )
     ;
	'$debug_restart'( State ),
	fail
    ).
*/
'$init_debugger' :-
    '$debugger_io',
    '__NB_setval__'('$spy_glist',[]),
    '__NB_setval__'('$spy_gdlist',[]),
    '__NB_setval__'('$spy_gn',1),
    '$init_debugger_trace'.

'$init_debugger_trace' :-
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0),
    fail.
'$init_debugger_trace' :-
    current_prolog_flag( trace,true),
    !,
    nb_setval(creep,creep).
'$init_debugger_trace' :-
    nb_setval(creep,zip).



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
  %cannot debug is called at the call port. UNUSED
/*
'$cannot_debug'(G, Module, GoalNo) :-
     (
	 current_prolog_flag( debug, false )
    ;
      '$is_private'(G,Module)
     ;
      functor(G,Na,_), atom_concat('$',_,Na)
    ;
      \+ '$debuggable'(G, [call], Module,GoalNo)
     ),
     !.
     */

'$debuggable'(_G, _Module, _, _GoalNo) :-
    current_prolog_flag(debug, false),
    !,
    fail.

'$zip_at_port'(_Port,_GoalNo,_) :-
    current_prolog_flag( debug, false),
    !.
'$zip_at_port'(_,_GoalNo,M:T) :-
    nb_getval('$spy_on', stop),
    recorded('$spy','$spy'(T,M),_),
    !,
    fail.
'$zip_at_port'(Port,GoalNo,_) :-
    nb_getval(creep,zip),
    nb_getval('$spy_target', TargetGoal ),
    number(GoalNo),
    number(TargetGoal),
    (Port == redo
      ->
      GoalNo > TargetGoal-1
    ;
      GoalNo > TargetGoal
    ).      
      


'$run_deb'(_Port,Ctx,_GN) :-
    '$continue_debugging'(Ctx).


'$exit_goal'(true, GN):-
    '$continue_debugging'(GN).

'$continue_debugging'(_) :-
    current_prolog_flag(debug, false),
    !.
'$continue_debugging'(_) :-
      current_prolog_flag( trace,true),
    nb_getval(creep,creep),
    fail.
'$continue_debugging'(inner).

'$continue_debugging'(_) :-
    current_prolog_flag(debug, false),
    !.
'$restart_debugging':-
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0),
    fail.
'$restart_debugging':-
     nb_getval(creep,creep), 
    !,
    '$creep'.
'$restart_debugging'.

'$may_creep'(true,creep).
'$may_creep'(true,leap).

/**

@}

*/
