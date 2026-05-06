/**********************************************************************a***
*									 *
  *	 YAP Prolog 							*
*									 *
  *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		debug.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	YAP debugger						 *
*									 *
*************************************************************************/

/**
 @file pl/debug.yap
@brief the debugger
*/

%:- system_module('$debug',[], []).

/**
  @defgroup Deb_Interaction Interacting with the debugger
@ingroup YAPProgramming

@brief YAP includes a procedural debugger, based on Byrd's four port model.

In this
model, execution is seen at the procedure level: each activation of a
procedure is seen as a box with control flowing into and out of that
box.

In the four port model control is caught at four key points: before
entering the procedure, after exiting the procedure (meaning successful
evaluation of all queries activated by the procedure), after backtracking but
before trying new alternative to the procedure and after failing the
procedure. Each one of these points is named a port:

```
           *--------------------------------------*
   Call    |                                      |    Exit
---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
           |                                      |
           |  descendant(X,Z) :-                  |
   Fail    |                                      |    Redo
           *--------------------------------------*
```



+ `Call`

    The call port is activated before initial invocation of
procedure. Afterwards, execution will try to match the goal with the
head of existing clauses for the procedure.

+ `Exit`

    This port is activated if the procedure succeeds.
Control will  now leave the procedure and return to its ancestor.

+ `Redo`

    If the goal, or goals, activated after the call port
fail  then backtracking will eventually return control to this procedure
through  the redo port.

+ `Fail`

    If all clauses for this predicate fail, then the
invocation fails,  and control will try to redo the ancestor of this
invocation.


To start debugging, the user will either call `trace` or spy the
relevant procedures, entering debug mode, and start execution of the
program. When finding the first spy-point, YAP's debugger will take
control and show a message of the form:
v
```
* (1)  call:  quicksort([1,2,3],_38) ?
```

The debugger message will be shown while creeping, or at spy-points,
and it includes four or five fields:

+
The first three characters are used to point out special states of the
debugger. If the port is exit and the first character is '?', the
current call is non-deterministic, that is, it still has alternatives to
be tried. If the second character is a `\*`, execution is at a
spy-point. If the third character is a `>`, execution has returned
either from a skip, a fail or a redo command.
+
The second field is the activation number, and uniquely identifies the
activation. The number will start from 1 and will be incremented for
each activation found by the debugger.
+
In the third field, the debugger shows the active port.
+
The fourth field is the goal. The goal is written by
`write_term/3` on the standard error stream, using the options
given by debugger_print_options.


If the active port is leashed, the debugger will prompt the user with a
`?`, and wait for a command. A debugger command is just a
character, followed by a return. By default, only the call and redo
entries are leashed, but the leash/1 predicate can be used in
order to make the debugger stop where needed.

There are several commands available, but the user only needs to
remember the help command, which is `h`. This command shows all the
available options, which are:

+ `c` - creep

    this command makes YAP continue execution and stop at the next
leashed port.

+ `return` - creep

    the same as c

+ `l` - leap

    YAP will execute until it meets a port for a spied predicate; this mode
keeps all computation history for debugging purposes, so it is more
expensive than standard execution. Use <tt>k</tt> or <tt>z</tt> for fast execution.

+ `k` - quasi-leap

    similar to leap but faster since the computation history is
not kept; useful when leap becomes too slow.

+ `z` - zip


    same as <tt>k</tt>
 `s` - skip

    YAP will continue execution without showing any messages until
returning to the current activation. Spy-points will be  ignored in this
mode. Note that this command keeps all debugging history, use <tt>t</tt> for fast execution. This command is meaningless, and therefore illegal, in the fail
and exit ports.

+ `t` - fast-skip

    similar to skip but faster since computation history is not
kept; useful if skip becomes slow.

+ `f [ _GoalId_]` - fail

    If given no argument, forces YAP to fail the goal, skipping the fail
port and backtracking to the parent.
If <tt>f</tt> receives a goal number as
the argument, the command fails all the way to the goal. If goal  _GoalId_ has completed execution, YAP fails until meeting the first active ancestor.

+ `r` [ _GoalId_] - retry

    This command forces YAP to jump back call to the port. Note that any
side effects of the goal cannot be undone. This command is not available
at the call port.  If <tt>f</tt> receives a goal number as the argument, the
command retries goal  _GoalId_ instead. If goal  _GoalId_ has
vcompleted execution, YAP fails until meeting the first active ancestor.

q+ `a` - abort

    execution will be aborted, and the interpreter will return to the
top-level. YAP disactivates debug mode, but spypoints are not removed.

+ `n` - nodebug

    stop debugging and continue execution. The command will not clear active
§spy-points.

+ `e` - exit

    leave YAP.

+ `h` - help

    show the debugger commands.

+ `!` Query

    execute a query. YAP will not show the result of the query.

+ `b` - break

    break active execution and launch a break level. This is  the same as `!break`.

+ `+` - spy this goal

    start spying the active goal. The same as `! spy  G` where  _G_
is the active goal.

+ `-` - nospy this goal

    stop spying the active goal. The same as `! nospy G` where  _G_ is                     
the active goal.

+ `p` - print

    shows the active goal using print/1

+ `d` - display

    shows the active goal using display/1

+ `<Depth` - debugger write depth

    sets the maximum write depth, both for composite terms and lists, that
will be used by the debugger. For more
information about `write_depth/2` ( (see Input/Output Control)).

+ `<` - full term

    resets to the default of ten the debugger's maximum write depth. For
more information about `write_depth/2` ( (see Input/Output Control)).

+ `A` - alternatives

    show the list of backtrack points in the current execution.

+ `g [ _N_]`

    show the list of ancestors in the current debugging environment. If it
receives  _N_, show the first  _N_ ancestors.


The debugging information, when fast-skip `quasi-leap` is used, will
be lost.

@}

*/



/*-----------------------------------------------------------------------------

				spy

-----------------------------------------------------------------------------*/


/**
   * @defgroup DebImplementation Implementation of the Debugger
   * @ingroup Implementation
   * @brief Prolog code to do debugging.
   *
   * @{
   * The debugger is an interpreter. with main predicates:
   * - $trace: this is the API
   * - trace_goal: reduce a query to a goal
   * - trace_goal: execute:
   *    + using the source, Luke
   *    + hooking into the WAM procedure call mechanism
   *    + asking Prolog to do it (system_library-builtins)
   *
   *	|flag	        | description	| initial | possible values
   *    |   ----------------------------------------------------------------
   *	| spy_gn	| last goal number 	| 1	| 1...
   *	| spy_trace	| trace	 		| 0	| 0, 1
   *	| spy_status	| step	 	 	| creep	| creep,leap,skip
   *	| ...	|  	| stop at goal	 	| -1	| Integer >= 1
   *	| ...	| 	| stop at spy-points	| stop	| stop,
   *    | '$within_debugger'| | | |
   *
   *
 */


%
/**
  * @pred $spy( +Goal )
  *(Goal)`
*/

'$spy'(MGoal) :-
    '$spy'(MGoal,outer).
	
'$creep'(MGoal) :-
    '$spy'(MGoal).

%%
%% @pred ´$spy'(ModuleGoal, CallerContext)
%%
%% This predicate controls entry and exit from the debugger. There are 3 options:
%% - inner: the caller is being controlled by the debugger, so we will stay on the debugger;
%% - top: the caller is either the Prolog top-level or a controlled meta-goal (eg, findall/3);
%%    on success or failure we will go back to the top-level and thus exit the debugger;
%% - outer: we were called while running the goal (eg, a spied goal). We will enter the debugger
%%    but may keep on debugging or exit the debugger.
 
'$spy'(true,_) :-
    !.
'$spy'(MGoal,outer) :-
    setup_call_catcher_cleanup(
 '$start_debugger',
'$debugger':trace(MGoal, outer, GN),
Port,
 '$debug_next'(Port, outer, GN)).
'$spy'(MGoal,top) :-
    setup_call_cleanup(
 '$start_debugger',
'$debugger':trace(MGoal, top, _),
 '$stop_debugger'
).
'$spy'(MGoal,inner) :-
    trace(MGoal,inner, _). 

%% @pred trace( 0:Goal )
%
% If the `debug` flag is true and we are not in zip mode,  execute
% _Goal_ in tracing mode; otherwise, execute the meta-call.
%
% T
%
trace(MGoal) :-
    '$spy'(MGoal, outer).

/** @pred trace( +Goal, +Context, ?GoalNo )
  *
  * This launches a goal from the debugger with the  call. It must:
  *  - disable user interaction;
  *  - verify whether debugging is still ok;
  *  - enter the debugger core.
  * The top gated_call should set up creeping for the next call.
  *
  * @param _Mod_:_Goal_ is the goal to be examined.
  * @return `call(Goal)`
*/
%%! The first case matches system_predicates or zip
%    trace_goal(G,M, inner, _GoalNumberN, _CP0).
'$debugger':trace(MG, Ctx, GN) :-
    strip_module(MG,M,G),
    nb_setval(creep,creep),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0),
    current_choice_point(CP0),
    '$debug_goal'(G, M, Ctx, GN, CP0).


/**
  * @pred debugger_io.
  *
  * set up the stream used for debugging,
  * - must be interactive.
  * - default is `user_input`, but /dev/tty and CONIN$ can be used directly if
  *   user_input is bound to a file.
  *
*/

'$debugger_io' :-
    '$debugger_input',
    '$debugger_output'.

'$debugger_input' :-
    stream_property(_,alias(debugger_input)),
    !.
'$debugger_input' :-
    S = user_input,
    stream_property(S,tty(true)),
    %    stream_property(S,input),
    !,
    set_stream(S,alias(debugger_input)).
'$debugger_input' :-
    current_prolog_flag(unix, true ),
    !,
    open('/dev/tty', read, _S, [alias(debugger_input),bom(false)]).
'$debugger_input' :-
    current_prolog_flag(windows, true ),
    !,
    open('CONIN$', read, _S, [alias(debugger_input),bom(false)]).
'$debugger_output' :-
    stream_property(_,alias(debugger_output)),
    !.
'$debugger_output' :-
    S = user_error,
    stream_property(S,tty(true)),
    %    stream_property(S,output),
    !,
    set_stream(S,alias(debugger_output)).
'$debugger_output' :-
    current_prolog_flag(unix, true ),
    !,
    open('/dev/tty', write, _S, [alias(debugger_output)]).
'$debugger_output' :-
    current_prolog_flag(windows, true ),
    !,
    open('CONOUT$', write, _S, [alias(debugger_output)]).


'$trace_meta_call'( G, CP, GN, M ) :-
    '$debug_goal'(G, M, outer, GN, CP ).

%% @pred '$debug_goal'( +G, +M, +Status, +GoalNumber, +CP)
%
%  debug a complex query
%
'$debug_goal'(V, M, _, _, _) :-
    (
      var(V)
      ->
      throw_error(instantiation_error,call(M:V))
      ;
      var(M)
      ->
      throw_error(instantiation_error,call(M:V))
    ).
'$debug_goal'( '$call'( G, CP0, _, M), _, Ctx, GN, _) :-
    !,
    '$debug_goal'(G, M,  Ctx, GN, CP0).
'$debug_goal'( '$cleanup_on_exit'(CP0, TaskF), _, _Ctx, _, _CP) :-
    !,
    '$cleanup_on_exit'(CP0, TaskF).
'$debug_goal'( '$top_level', _, _Ctx, _, _) :-
    !,
    nb_setval(creep,zip).
'$debug_goal'('$drop_exception'(V,J), _, _, _, _) :-
    !,
    '$drop_exception'(V,J).
'$debug_goal'(expand_goal(V,J), _, _, _, _) :-
    !,
    expand_goal(V,J).

'$debug_goal'(true,_, _, _,  _CP) :-
    !.
'$debug_goal'(false,_, _, _,  _CP) :-
    !.
'$debug_goal'(fail,_, _, _,  _CP) :-
    !.
'$debug_goal'(!,_, _, _,  CP) :-
    !,
    cut_by(CP).
'$debug_goal'(current_choice_point(CP),_, _,  _,CP) :-
    !.
'$debug_goal'(query_to_answer(G,Vs,Port, Bindings,GF,Goals),_, _, _, _) :-
    !,
    setup_call_cleanup(
'$stop_debugger',
    query_to_answer(G, Vs,Port, Bindings,GF,Goals),
    break,
'$start_debugger'
).
'$debug_goal'(cut_by(M), _, _, _,  _) :-
    !,
    cut_by(M).
'$debug_goal'(M:G, _, Ctx, GN0, CP) :-
    !,
    '$yap_strip_module'(M:G, M0, G0),
    '$debug_goal'(G0, M0, Ctx, GN0, CP ).
'$debug_goal'((A,B), M, Ctx, GN0, CP) :- !,
				      '$debug_goal'(A, M, inner, GN0, CP),
				      '$debug_goal'(B, M, Ctx, _GN0, CP).
'$debug_goal'((A->B;C), M, Ctx, GN0, CP) :- !,
					 ( '$debug_goal'(call(A), M, inner, GN0, CP) ->
					   '$debug_goal'(B, M, Ctx, _GN0, CP);
					   '$debug_goal'(C, M, Ctx, _GN0, CP)).
'$debug_goal'((A*->B;C), M, Ctx, GN0, CP) :- !,
					  ('$debug_goal'(call(A), M, inner, GN0, CP) *->
					   '$debug_goal'(B, M, Ctx, _GN0, CP);
					   '$debug_goal'(C, M, Ctx, _GN0, CP)).
'$debug_goal'((A*->B), M, Ctx, GN0, CP) :- !,
					\+ '$debug_goal'(call(A), M, inner, GN0, CP),
					'$debug_goal'(B, M, Ctx, _GN0, CP).
'$debug_goal'((A;B), M, Ctx, GN0, CP) :-
    !,
    ('$debug_goal'(A, M, Ctx, GN0, CP);
     '$debug_goal'(B, M, Ctx, GN0, CP)).
'$debug_goal'((A|B), M, Ctx, GN0, CP) :-
    !,
    ('$debug_goal'(A, M, Ctx, GN0, CP);
     '$debug_goal'(B, M, Ctx, GN0, CP)).
%%< leave the debugger (zip)
'$debug_goal'(G, M, _Ctx, GN, _CP) :-
    '$zip_at_port'(call,GN,M:G),
    !,
    setup_call_cleanup(
 '$stop_debugger',
    '$execute'(M:G),    
 '$start_debugger'
    ).
'$debug_goal'(G, M, Ctx, GN, CP) :-
    '$id_goal'(GoalNumber),
%    '$interact'(call, M:G,Ctx, GoalNumber), 
    (
      '$zip_at_port'(call,GoalNumber,M:G)
      ->
    setup_call_cleanup(
 '$stop_debugger',
    '$execute'(M:G),   
 '$start_debugger'
    )
      ;
      
      catch(
    '$step_goal'(G,M,Ctx,GoalNumber),
    Error,
    trace_error(Error, GoalNumber, '$debug_goal'(G,M, Ctx, GN, CP))
      )
    ).

/**
 *
 */
'$step_goal'(true,_M, _,_GoalNumber) :-
    !.
'$step_goal'(G,M, Ctx, GoalNumber) :-
    '$interact'(call, M:G,Ctx, GoalNumber), 
    '$zip_at_port'(call,GoalNumber,M:G),
    !,
   setup_call_cleanup(
 '$stop_debugger',
    '$execute'(M:G),  
 '$start_debugger'
    ).
'$step_goal'(G,M,Ctx, GoalNumber) :-
    '$predicate_type'(G,M,T),
    '$step'(T,M:G,Ctx,GoalNumber).



'$step'(   foreign_procedure,MG,Ctx,GoalNumber) :-
    '$step'(   system_procedure,MG,Ctx,GoalNumber).
'$step'(proxy_procedure,M:G,Ctx,GoalNumber) :-
    !,
    '$import'(MDonor,M,GDonor,G,_,_),
    '$predicate_type'(GDonor,MDonor,T),
    '$step'(T,MDonor:GDonor,Ctx,GoalNumber).
'$step'(   updatable_procedure,MG,Ctx,GoalNumber) :-
    '$step'(   source_procedure,MG,Ctx,GoalNumber).
%% execute non stop will skip the first spy...
'$step'(   system_procedure,MG,Ctx,GoalNumber) :-
    !,
   '$meta_hook'(MG,NMG),
    gated_call(
(         nb_setval(creep,zip),
    '$stop_debugger'
    ),
    (
      '$execute_non_stop'(NMG)
    ),
Port,
         '$interact'(Port, NMG,Ctx, GoalNumber)
    ).
'$step'(   exo_procedure,MG,Ctx,GoalNumber) :-
    '$step'(   source_procedure,Ctx,MG,GoalNumber).
'$step'(   mega_procedure,MG,Ctx,GoalNumber) :-
    '$step'(   source_procedure,MG,Ctx,GoalNumber).
'$step'(   source_procedure,MG,Ctx,GoalNumber) :-
    current_choice_point(CP),
    gated_call(   
'$meta_hook'(MG,NM:NG),
       (
	 clause(NM:NG,Body),
	 '$debug_goal'(Body,NM,inner,GoalNumber, CP)
    ),
    Port,       
    '$interact'(Port, NM:NG, Ctx,GoalNumber)
    ).
'$step'(   undefined_procedure,MG,Ctx,GoalNumber) :-
     '$step'(   system_procedure,MG,Ctx,GoalNumber).
'$step'(   static_procedure,MG,Ctx,GoalNumber) :-
    current_choice_point(CP),
    gated_call(
'$meta_hook'(MG,NM:NG),
    (
      predicate_property(NM:NG, number_of_clauses(NCl)),
      between(1,NCl, I),
      fetch_nth_clause(I,NM:NG,_,Ref),
      '$stop_debugger',
      '$creep_clause'( NG, NM, Ref, CP )
    ),
    Port,	
    '$interact'(Port, NM:NG, Ctx, GoalNumber)
    ).


'$debug_gated_call'(Goal, GoalNumber) :-
    current_choice_point(CP0),
    '$setup_call_catcher_cleanup'('$meta_hook'(Goal,NM:NG)), 
    Cleanup = '$interact'(Port, NM:NG, outer, GoalNumber),                     
    Task0 = bottom( true, Port, Cleanup, Tag, true, CP0),
    '$tag_cleanup'(CP0, Task0),
    TaskF = top( true, Port, Cleanup, Tag, false, CP0),
    clause(NM:NG,Body),
    '$debug_goal'(Body,NM,inner,GoalNumber, CP0),
    '$cleanup_on_exit'(CP0, TaskF).


'$meta_hook'(MG,M:NG) :-
     current_prolog_flag(debug,true),
     nb_getval(creep,creep),
    '$yap_strip_module'(MG,M,G),
    functor(G,N,A),  
    N\=throw,
    functor(PredDef,N,A),
    G  =..[_|As],
    (
      recorded('$m', meta_predicate(prolog,PredDef),_)
    ->
    true
    ;
    recorded('$m', meta_predicate(M,PredDef),_)
    ),
    PredDef=..[N|Ms],
'$one_number'(Ms),
    '$debugger_prepare_meta_arguments'(As, Ms, NAs),
    NG=..[N|NAs],
    G \== NG,
    !.
'$meta_hook'(MG,MG).

'$one_number'([N|_Ms]) :-
    integer(N),
!.
'$one_number'([_|Ms]) :-
    '$one_number'(Ms),
   !.


/*'$interact'(P, MG, GoalNumber) :-
    '$zip_at_port'(P,GoalNumber,MG),
    !.
*/
'$interact'(!, _ModuleG,_, _L) :-
!.
'$interact'(P, Module:G,_Ctx, L) :-
    (
      false ->
true
;
    '$start_debugger'),
    nb_getval(creep,leap),
    !,
    ('$deterministic_port'(P) -> Deterministic = '?' ; Deterministic = ' '),
    '$enter_trace'(L, Module:G, Deterministic),
    (P == answer -> P1 = exit; P = P1),
    '$action'(l,P1,L,G,Module,Deterministic).  
'$interact'(P, ModuleG,_, L) :-
    '$zip_at_port'(P,L,ModuleG),
    !.
'$interact'(P, Module:G,_, L) :-
    ('$deterministic_port'(P) -> Deterministic = ' ' ; Deterministic = '?'),
    '$id_goal'(L),        /* get goal no.	*/
    % at this point we are done with leap or skipe
    '$enter_trace'(L, Module:G, Deterministic),
    (P == answer -> P1 = exit; P = P1),
    repeat,
    '$clear_input'(debugger_input),
    '$trace_msg'(P1,G,Module,L,Deterministic),
    (
      '$has_leash'(P1)
      ->
      prompt1(' ? '),
      get_char(user_input,C),
      '$action'(C,P1,L,G,Module,Deterministic)
      ;
      '$action'('\n',P1,L,G,Module,Deterministic),
      nl(debugger_otput)                            
    ),
    !.


/**
 * @Pred '$enter_trace'(+L, 0:G, +Module, +Info)
 *
 * call goal: prelims
 *
 * @parameter _Module_:_G_
 * @parameter _L_ is the list of active goals
 * @parameter _Info_ describes the goal
 *
 */
'$enter_trace'(Id, Module:G, Deterministic) :-
    '$id_goal'(Id),        /* get goal no.	*/
    /* get goal list		*/
    '__NB_getval__'('$spy_glist',History,History=[]),
    Info = info(id,Module,G,_CP,_Retry,Deterministic,_HasFoundAnswers),
    H  = [Info|History],
    b_setval('$spy_glist',H).	/* and update it		*/

'$id_goal'(L) :-
    var(L),
    !,
    ( '__NB_getval__'('$spy_gn',L,fail) -> true ; L = 0 ),
    /* bump it			*/
    L1 is L+1,
    /* and save it globaly		*/
    '__NB_setval__'('$spy_gn',L1).
'$id_goal'(L) :- integer(L).




/**
 * @pred '$trace_go'(+L, 0:G, +Module, +Info)
 * 
* It needs to run in two separate steps:
 *    1. Select a clause;
 *    2. Debug it.
 * We use a marker to track who we are in gated_call.
 *
 * @parameter _Module_:_G_
 * @parameter _GoalNumber_ identifies the active goal
 * @parameter _Info_ describes the goal
 *
 */

'$trace_port'(Port, GoalNumber, Goal, Module, Ctx, CP,Info) :-
    ('$deterministic_port'(Port ,Info) -> true ; true ),
    (
      true
      ;
      '$trace_port_'(Port, GoalNumber, Goal, Module, Ctx, CP,Info)
    ).

%        
% last.first


'$deterministic_port'(exit).
'$deterministic_port'(fail).
'$deterministic_port'(call).

%'$publish_port'(redo(_), internal) :- !.
%'$publish_port'(fail(_), internal) :- !.
%'$publish_port'(abort, internal) :- !.
'$publish_port'(E, exception(E)).


%%% - abort: forward throw while the call is newer than goal
%% @pred '$debug_goal''( Exception, +Goal, +Mod, +GoalID )
%
% debugger code for exceptions. Recognised cases are:
%   - abort always forwarded
%   - redo resets the goal
%   - fail gives up on the goal.
%% trace_error(_Event,  GoalNumber, G, Module, _, _, _, CP) :-
%%     writeln(trace_error(_Event,  _GoalNumber, _G, _Module,CP,_Info)),
%%     fail.
%'$reenter_debugger'(exception(Event)),
%    fail.
trace_error(error(debugger_event(fail,G0),[]), GoalNumber, _G) :-
    !,
    (
      GoalNumber > G0
      ->
      throw(error(debugger_event(fail,G0),[]))
      ;
      fail
    ).
trace_error(error(debugger_event(redo,G0),[]), GoalNumber, G) :-
    !,
    (
      GoalNumber > G0
      ->
      throw(error(debugger_event(redo,G0),[]))
      ;
      nb_setval(creep,creep),
      nb_setval('$spy_on',stop),
      nb_setval('$spy_target',0),
      G
    ).
%trace_error( error(Id,Info), _, _, _, _) :-
%    !,
%    throw( error(Id, Info) ).
%     - forward through the debugger
trace_error(Event,_,_,_,_,_) :-
    throw(Event).

% Just fail here, don't really need toc all debugger, the user knows what he
% wants to do
'$loop_fail'(_GoalNumber, _G, _Module, _Creep) :-
    '$stop_debugger',
    fail.

%
% skip a goal or a port
%
'$gg'(CP,Goal) :-
    current_choice_point(CP0),
    CP = CP0,
    Goal.



'$trace_msg'(P,G,Module,L,Det) :-
    functor(P,P0,_),
    ('$pred_being_spied'(G,Module) -> CSPY = '*' ; CSPY = ' '),
    % vsc: fix this
    %		( SL = L -> SLL = '>' ; SLL = ' '),

    SLL = ' ',
    ( Module\=prolog,
      Module\=user
      ->
      GW = Module:G
      ;
      GW = G
    ),
    format(debugger_output,'~N~a~a~a       (~d)    ~q:',[Det,CSPY,SLL,L,P0]),
    '$debugger_write'(debugger_output,GW).

'$debugger_write'(Stream, G) :-
    current_prolog_flag( debugger_print_options, OUT ), !,
							write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
    writeq(Stream, G).

'$action'('\r',P,CallNumber,G,Module,Deterministic) :- !,	% newline
						       get_char( debugger_input,C),
						       '$action'(C,P,CallNumber,G,Module,Deterministic).
'$action'('\n',_,_,_,_,_) :- !,			% newline 	creep
			     %    start_low_level_trace,
			     nb_setval(creep,creep),
			     nb_setval('$spy_on',stop),
			     nb_setval('$spy_target',0).
'$action'(!,_,_,_,_,_) :- !,			% ! 'g		execute
			  read(debugger_input, G), 
			  % don't allow yourself to be caught by creep.
			  ignore( G ),
			  skip( debugger_input, 10),
			  fail.
'$action'(<,_,_,_,_,_) :- !,			% <'Depth
			  '$new_deb_depth',
			  skip( debugger_input, 10),
			  fail.
'$action'('C',_,_,_,_,_) :-
    current_prolog_flag(system_options, Opts),
    '$memberchk'( call_tracer, Opts),
    !,			% <'Depth
    skip( debugger_input, 10),
    nb_setval(creep,creep),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
'$action'(^,_,_,G,_,_) :-
    !,			% '
    '$print_deb_sterm'(G),
    skip( debugger_input, 10),
    fail.
'$action'(a,_,_,_,_,_) :-
    !,		% 'a		abort
    skip( debugger_input, 10),
    '$stop_debugger',
    abort.
'$action'(b,_,_,_,_,_) :-
    !,			% 'b		break
    '$stop_creeping'(_),
    skip( debugger_input, 10),
    setup_call_cleanup(
'$stop_debugger',
    break,
'$start_debugger'),
    fail.
'$action'('A',_,_,_,_,_) :-
    !,			% 'b		break
    skip( debugger_input, 10),
    '$stack_dump',
    fail.
'$action'(c,_,_,_,_,_) :-
    !,			% 'c		creep
    skip( debugger_input, 10),
    nb_setval(creep,creep),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
'$action'(e,_,_,_,_,_) :-
    !,			% 'e		exit
    halt.
'$action'(f,_,CallNumber,_,_,_) :-
    !,		% 'f		fail
    '$scan_number'( ScanNumber),
    ( ScanNumber == 0 -> Goal = CallNumber ; Goal = ScanNumber ),
    throw(error(debugger_event(fail,Goal),[])).
'$action'(h,_,_,_,_,_) :-
    !,			% 'h		help
    '$action_help',
    skip( debugger_input, 10),
    fail.
'$action'(?,_,_,_,_,_) :-
    !,			% '?		help
    '$action_help',
    skip( debugger_input, 10),
    fail.
'$action'(p,_,_,G,Module,_) :-
    !,		% 'p		print
    ((Module = prolog ; Module = user) ->
     print(user_error,G), nl(user_error)
			  ;
			  print(user_error,Module:G), nl(user_error)
    ),
    skip( debugger_input, 10),
    fail.
'$action'(d,_,_,G,Module,_) :-
    !,		% 'd		display
    ((Module = prolog ; Module = user) ->
     display(user_error,G), nl(user_error)
			    ;
			    display(user_error,Module:G), nl(user_error)
    ),
    skip( debugger_input, 10),
    fail.
'$action'(l,_,CallNumber,_,_,_) :-
    !,			% 'leap
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    nb_setval(creep,leap),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
'$action'(z,_,CallNumber,_,_,_CP) :-
    !,
    % 'z		zip, fast leap
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    nb_setval(creep,zip),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',Goal).
% skip first call (for current goal),
% stop next time.
'$action'(k,_,_CallNumber,_,_,_) :-
    !,
    '$ensure_number'(_ScanNumber),
    nb_setval(creep,zip),
    nb_setval('$spy_on',stop).
% skip first call (for current goal),
% stop next time.
'$action'(n,_,_,_,_,_) :-
    !,			% 'n		nodebug
    skip( debugger_input, 10),				% '
    % tell debugger never to stop.
    nodebug.
'$action'(r,P,CallNumber,_,_,_) :-
    !,	        % r		retry
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P==call) ->
      '$ilgl'(s)				%
      ;
      true
    ),
    nb_setval(creep,creep),
    nb_setval('$spy_on',ignore),
    nb_setval('$spy_target',Goal),
    '$stop_debugger' ,
    throw(error(debugger_event(redo,Goal),[])).
'$action'(s,P,CallNumber,_,_,_) :-
    !,		% 's		skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P==call; P==redo) ->
      nb_setval(creep,creep),
      nb_setval('$spy_on',ignore),
      nb_setval('$spy_target',Goal)
      ;
      '$ilgl'(s)				%
    ).
'$action'(t,P,CallNumber,_,_,_) :-
    !,		% 't		fast skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P=call; P=redo) ->
      nb_setval(creep,zip),
      nb_setval('$spy_on',ignore),
      nb_setval('$spy_target',Goal)
      ;
      '$ilgl'(t)				%
    ).
'$action'(q,P,CallNumber,_,_,_) :-
    !,		% qst skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P=call; P=redo) ->
      nb_setval(creep,leap),
      nb_setval('$spy_on',stop),
      nb_setval('$spy_target',Goal)
      ;
      '$ilgl'(t)				%
    ).
'$action'(+,_,_,G,M,_) :-
    !,			%%		spy this
    functor(G,F,N), spy(M:(F/N)),
		    skip( debugger_input, 10),
		    fail.
'$action'(-,_,_,G,M,_) :-
    !,			%% 	nospy this
    functor(G,F,N),
    nospy(M:(F/N)),
    skip( debugger_input, 10),
    fail.
'$action'(g,_,_,_,_,_) :-
    !,			% g		ancestors
    '$ensure_number'(HowMany),
    '$show_ancestors'(HowMany),
    fail.
'$action'('T',exception(G),_,_,_,_) :-
    !,	% T		throw
    throw( G ).
'$action'(C,_,_,_,_,_) :-
    skip( debugger_input, 10),
    '$ilgl'(C),
    fail.

'$show_ancestors'(HowMany) :-
    '__NB_getval__'('$spy_glist',[_|History], fail),
    (
      History == []
      ->
      print_message(help, ancestors([]))
      ;
      '$show_ancestors'(History,HowMany),
      nl(user_error)
    ).

'$show_ancestors'([],_).
'$show_ancestors'([_|_],0) :- !.
'$show_ancestors'([info(L,M,G,_CP,Retry,Det,_Exited)|History],HowMany) :-
    '$show_ancestor'(L,M,G,Retry,Det,HowMany,HowMany1),
    '$show_ancestors'(History,HowMany1).

% skip exit port, we're looking at true ancestors
'$show_ancestor'(_,_,_,_,Det,HowMany,HowMany) :-
    nonvar(Det), !.
% look at retry
'$show_ancestor'(GoalNumber, M, G, Retry, _, HowMany, HowMany1) :-
    nonvar(Retry), !,
		   HowMany1 is HowMany-1,
		   '$trace_msg'(redo, G, M, GoalNumber, _), nl(user_error).
'$show_ancestor'(GoalNumber, M, G, _, _, HowMany, HowMany1) :-
    HowMany1 is HowMany-1,
    '$trace_msg'(call, G, M, GoalNumber, _), nl(user_error).


'$action_help' :-
    format(user_error,'newline  creep       a       abort~n', []),
    format(user_error,'c        creep       e       exit~n', []),
    format(user_error,'f Goal   fail        h       help~n', []),
    format(user_error,'l        leap        r Goal  retry~n', []),
    format(user_error,'s        skip        t       fastskip~n', []),
    format(user_error,'q        quasiskip   k       quasileap~n', []),
    format(user_error,'b        break       n       no debug~n', []),
    format(user_error,'p        print       d       display~n', []),
    format(user_error,'<D       depth D     <       full term~n', []),
    format(user_error,'+        spy this    -       nospy this~n', []),
    format(user_error,'^        view subg   ^^      view using~n', []),
    format(user_error,'A        choices     g [N]   ancestors~n', []),
    format(user_error,'T        throw       ~n', []),
    format(user_error,'! g execute goal~n', []).

'$ilgl'(C) :-
    print_message(warning, trace_command(C)),
    print_message(help, trace_help),
    fail.

'$ensure_number'(Nb) :-
    '$scan_number'(Nb),
    !.
'$ensure_number'(0).

'$scan_number'(Nb) :-
    '$fetch_codes'(S),
    S=[_|_],
    catch(number_codes( Nb, S), _, fail).

'$fetch_codes'(S) :-
    get_code(debugger_input,Code),
    (Code == 10 -> S = [] ; S = [Code|NS], '$fetch_codes'(NS) ).
 
'$print_deb_sterm'(G) :-
    '$get_sterm_list'(L), !,
			  '$deb_get_sterm_in_g'(L,G,A),
			  recorda('$debug_ub_skel',L,_),
			  format(user_error,'~n~w~n~n',[A]).
'$print_deb_sterm'(_) :- skip( debugger_input, 10).

'$get_sterm_list'(L) :-
    get_code( debugger_input_input,C),
    '$deb_inc_in_sterm_oldie'(C,L0,CN),
    '$get_sterm_list'(L0,CN,0,L).

'$deb_inc_in_sterm_oldie'(94,L0,CN) :-
    !,
    get_code( debugger_input,CN),
    ( recorded('$debug_sub_skel',L0,_) -> true ;
      CN = [] ).
'$deb_inc_in_sterm_oldie'(C,[],C).

'$get_sterm_list'(L0,C,N,L) :-
    ( C =:= "^", N =\= 0 ->
		 get_code(debugger_input, CN),
		 '$get_sterm_list'([N|L0],CN,0,L)
		 ;
		 C >= "0", C =< "9" ->
		 NN is 10*N+C-"0", get_code(debugger_input, CN),
				   '$get_sterm_list'(L0,CN,NN,L)
				   ;
				   C =:= 10 ->
				   (N =:= 0 -> L = L0 ; L=[N|L0])
    ).

'$deb_get_sterm_in_g'([],G,G).
'$deb_get_sterm_in_g'([H|T],G,A) :-
    '$deb_get_sterm_in_g'(T,G,A1),
    arg(H,A1,A).

'$new_deb_depth' :-
    get_code( debugger_input,C),
    '$get_deb_depth'(C,D),
    '$set_deb_depth'(D).

'$get_deb_depth'(10,10) :-  !. % default depth is 0
'$get_deb_depth'(C,XF) :-
    '$get_deb_depth_char_by_char'(C,0,XF).

'$get_deb_depth_char_by_char'(10,X,X) :- !.
'$get_deb_depth_char_by_char'(C,X0,XF) :-
    C >= "0", C =< "9", !,

			XI is X0*10+C-"0",
			get_code( debugger_input,NC),
			'$get_deb_depth_char_by_char'(NC,XI,XF).
% reset when given garbage.
'$get_deb_depth_char_by_char'(_C,X,X).

'$set_deb_depth'(D) :-
    current_prolog_flag(debugger_print_options,L),
    '$delete_if_there'(L, max_depth(_), max_depth(D), LN),
    set_prolog_flag(debugger_print_options,LN).

'$delete_if_there'([], _, TN, [TN]).
'$delete_if_there'([T|L], T, TN, [TN|L]) :- !.
'$delete_if_there'([Q|L], T, TN, [Q|LN]) :-
    '$delete_if_there'(L, T, TN, LN).

'$debugger_deterministic_goal'(exit).
'$debugger_deterministic_goal'(fail).
'$debugger_deterministic_goal'(!).
'$debugger_deterministic_goal'(exception(_)).
'$debugger_deterministic_goal'(external_exception(_)).


'$cps'([CP|CPs]) :-
    '$debugger':choicepoint(CP,_A_,_B,_C,_D,_E,_F),
    '$cps'(CPs).
'$cps'([]).


'$debugger_skip_debug_goal'([],[]).
'$debugger_skip_debug_goal'([CP|CPs],CPs1) :-
    '$debugger':choicepoint(CP,_,'$debugger','$debug_goal',5,(_;_),_),
    !,
    '$debugger_skip_debug_goal'(CPs,CPs1).
'$debugger_skip_debug_goal'([CP|CPs],[CP|CPs1]) :-
    !,
    '$debugger_skip_debug_goal'(CPs,CPs1).

'$debugger_skip_traces'([CP|CPs],CPs1) :-
    '$debugger':choicepoint(CP,_,prolog,'$port',7,(_;_),_),
    !,
    '$debugger_skip_traces'(CPs,CPs1).
'$debugger_skip_traces'(CPs,CPs).

'$debugger_skip_loop_spy2'([CP|CPs],CPs1) :-
    '$debugger':choicepoint(CP,_,prolog,'$loop_spy2',5,(_;_),_),
    !,
    '$debugger_skip_loop_spy2'(CPs,CPs1).
'$debugger_skip_loop_spy2'(CPs,CPs).

'$debugger_prepare_meta_arguments'([], [], []).
'$debugger_prepare_meta_arguments'([A|As], [N|Ms], [trace((MA:GA))|NAs]) :-
    '$yap_strip_module'(A,MA,GA),
    integer(N),
    N>=0,
    length(B,N),
    '$append'(B,R,As),
    GA=..[GN|GAs],
    '$append'(GAs,B,NGAs),
    NGA =.. [GN|NGAs],
    length(Ms0,N),
    '$append'(Ms0,RMs,Ms),
    NGA \= trace(_),
    !,
    '$debugger_prepare_meta_arguments'(R, RMs, NAs).
'$debugger_prepare_meta_arguments'([A|As], [_|Ms], [A|NAs]):-
    '$debugger_prepare_meta_arguments'(As, Ms, NAs).



:- meta_predicate(watch_goal(0)).
watch_goal(G) :-
    '$id_goal'(I),
    gated_call(
			    format(user_error, '% ~d ~w:~n         ~w.~n',[I,call,G]),
			    format(user_error, '% ~d goal, port ~w ~w.~n',[I,call,G]),
	    % debugging allowed.
	    G,
	    Port,
	    format(user_error, '% ~d ~w:~n         ~w.~n',[I,Port,G])
    ).


'$debugging' :- nb_getval(running_debugger_code, true  ).
%% @
