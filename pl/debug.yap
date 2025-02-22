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
@{
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
<--------- +     offspring(X,Y), descendant(Y,Z). + <---------
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
   *
   *
   *
 */


%'$trace'(G) :- write(user_error,'$spy'(G)), nl, fail.
%
/**
  * @pred $spy( +Goal )
  *(Goal)`
*/

'$spy'(ModG) :-
    '$spy'(ModG,outer).
	

'$spy'(ModG,Ctx) :-
    gated_call(
    strip_module(ModG,M,G),
    (G == true -> true ; '$trace'(M:G, Ctx)),
    Port,
    '$continue_debugging'(Port,Ctx)).


'$continue_debugging'(_,_) :-
    nb_getval(creep,zip),
    !.
'$continue_debugging'(_,top) :-
    !.
'$continue_debugging'(exit,_) :-
    !,
    '$creep'.
'$continue_debugging'(answer,_) :-
    !,
    '$creep'.
'$continue_debugging'(fail,_) :-
    !,
    '$creep'.
'$continue_debugging'(redo,_) :-
    !,
    '$creep'.

'$creep'(true) :-
    !.
'$creep'(ModG) :-
    '$spy'(ModG).

/**
  * @pred $trace( +Goal, +Context )
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
'$trace'( MG, _Ctx) :-
    '$zip_at_port'( call, _GN0, MG),
    !,
    '$execute_non_stop'(MG).
'$trace'(MG, Ctx) :-
    strip_module(MG,M,G),
    !,
    nb_setval(creep,creep),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0),
    current_choice_point(CP0),
    trace_goal(G, M, _, Ctx, CP0).
/*'$trace'(M:G, Ctx) :- % system
    '$id_goal'(GoalNumberN),
    '$meta_hook'(M:G,MNG),
catch(
    (current_choice_point(CP0),
    '$execute_non_stop'(MNG),
    current_choice_point(CPF)),
    Error,
    trace_error(Error, GoalNumberN, trace_goal(G,M, Ctx, GoalNumberN, CP0))
    ),
    (CP0==CPF
    ->
	!
    ;
true

    ).
*/

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


'$trace_meta_call'( G, CP, _, M ) :-
    trace_goal(G, M, outer, _GN, CP ).

%% @pred trace_goal( +G, +M, +GoalNumber, +CP)
%
%  debug a complex query
%
trace_goal(V, M, _, _, _) :-
    (
	var(V)
    ->
    throw_error(instantiation_error,call(M:V))
    ;
    var(M)
    ->
    throw_error(instantiation_error,call(M:V))
    ).
trace_goal( '$call'( G, CP0, _, M), _, Ctx, _, _) :-
    !,
    trace_goal(G, M,  Ctx, _, CP0).
trace_goal( '$cleanup_on_exit'(CP0, TaskF), _, _Ctx, _, _CP) :-
    !,
     '$cleanup_on_exit'(CP0, TaskF).
trace_goal( '$top_level', _, _Ctx, _, _) :-
    !,
    nb_setval(creep,zip).
trace_goal('$drop_exception'(V,J), _, _, _, _) :-
    !,
    '$drop_exception'(V,J).

trace_goal(true,_, _, _,  _CP) :-
    !.
trace_goal(false,_, _, _,  _CP) :-
    !.
trace_goal(fail,_, _, _,  _CP) :-
    !.
trace_goal(!,_, _, _,  CP) :-
    !,
    cut_by(CP).
trace_goal(current_choice_point(CP),_, _,  _,CP) :-
    !.
trace_goal(query_to_answer(G,Vs,Port, Bindings,GF,Goals),_, _, _, _) :-
    !,
    query_to_answer(G, Vs,Port, Bindings,GF,Goals).
trace_goal(cut_by(M), _, _, _,  _) :-
    !,
    cut_by(M).
trace_goal(M:G, _, Ctx, GN0, CP) :-
    !,
    '$yap_strip_module'(M:G, M0, G0),
    trace_goal(G0, M0, Ctx, GN0, CP ).
trace_goal((A,B), M, Ctx, GN0, CP) :- !,
    trace_goal(A, M, inner, GN0, CP),
    trace_goal(B, M, Ctx, _GN0, CP).
trace_goal((A->B;C), M, Ctx, GN0, CP) :- !,
    ( trace_goal(call(A), M, inner, GN0, CP) ->
      trace_goal(B, M, Ctx, _GN0, CP);
      trace_goal(C, M, Ctx, _GN0, CP)).
trace_goal((A*->B;C), M, Ctx, GN0, CP) :- !,
    (trace_goal(call(A), M, inner, GN0, CP) *->
	 trace_goal(B, M, Ctx, _GN0, CP);
     trace_goal(C, M, Ctx, _GN0, CP)).
trace_goal((A*->B), M, Ctx, GN0, CP) :- !,
    \+ trace_goal(call(A), M, inner, GN0, CP),
    trace_goal(B, M, Ctx, _GN0, CP).
trace_goal((A;B), M, Ctx, GN0, CP) :-
    !,
    (trace_goal(A, M, Ctx, GN0, CP);
     trace_goal(B, M, Ctx, GN0, CP)).
trace_goal((A|B), M, Ctx, GN0, CP) :-
    !,
    (trace_goal(A, M, Ctx, GN0, CP);
     trace_goal(B, M, Ctx, GN0, CP)).
trace_goal(true, _M, _Ctx, _GN, _CP) :-
    !.
trace_goal(G,M, Ctx, GoalNumberN, CP0) :-
    predicate_property(M:G, imported_from(M0)),
    !,
    trace_goal(G,M0, Ctx, GoalNumberN, CP0).
trace_goal(G,M, _Ctx, _GoalNumberN, _CP0) :-
    '$id_goal'(GoalNumberN),
catch(
step_goal(G,M,GoalNumberN),
    Error,
    trace_error(Error, GoalNumberN, step_goal(G,M, GoalNumberN))
    ).

 step_goal(G,M, GoalNumberN) :-
     '$interact'([call], M:G, GoalNumberN),
     nb_getval(creep,zip),
     !,
     '$step'(zipped_procedure,M:G,GoalNumberN).
 step_goal(G,M, GoalNumberN) :-
 '$predicate_type'(G,M,T),
    '$step'(T,M:G,GoalNumberN).

%%
% step a call
% 
'$step'(   exo_procedure,MG,GoalNumber) :-
    '$step'(   source_procedure,MG,GoalNumber).
'$step'(   mega_procedure,MG,GoalNumber) :-
    '$step'(   source_procedure,MG,GoalNumber).
'$step'(proxy_procedure,M:G,GoalNumber) :-
    !,
    '$import'(MDonor,M,GDonor,G,_,_),
    '$predicate_type'(GDonor,MDonor,T),
    '$step'(T,MDonor:GDonor,GoalNumber).
'$step'(   source_procedure,MG,GoalNumber) :-
    % prepare and select matching clause
   '$creep_enumerate_sources'(
     (current_choice_point(CP),
    '$meta_hook'(MG,NM:NG)),
NM:NG,
B,
Port0,
    true
),
   '$creep_run_sources'(
      true,
      NM,B, CP,GoalNumber,
       Port,
inner,
       '$interact'([Port,Port0], NM:MG, GoalNumber)
). %
'$step'(   static_procedure,MG,GoalNumber) :-
    % prepare and select matching clause
    '$creep_enumerate_refs'(
     (current_choice_point(CP),
    '$meta_hook'(MG,NMG)), NMG, N, Ref, Port0, true),
'$creep_run_refs'(
true,
 MG,
N,
Ref,
CP,
Port,
       '$interact'([Port,Port0], NMG, GoalNumber)
).
'$step'(   system_procedure,MG,GoalNumber) :-
  	gated_call(    % debugging allowed.
  '$meta_hook'(MG,NMG),
	    call(NMG),
	    Port,	
         '$interact'([Port], NMG, GoalNumber)
	).
'$step'(   zipped_procedure,MG,GoalNumber) :-
  	gated_call(    % debugging allowed.
  true,
	    call(MG),
	    Port,	
         '$interact'([Port], MG, GoalNumber)
	)
.
'$creep_enumerate_sources'(Setup, M:Goal, B,Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
    Task0 = cleanup( true, Catcher, Cleanup, Tag, true, CP0),
	TaskF = cleanup( true, Catcher, Cleanup, Tag, false, CP0),
	'$tag_cleanup'(CP0, Task0),
    clause(M:Goal,B),
	'$cleanup_on_exit'(CP0, TaskF).


'$creep_enumerate_refs'(Setup, M:Goal, _N, Ref, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
        Task0 = cleanup( true, Catcher, Cleanup, Tag, true, CP0),
	TaskF = cleanup( true, Catcher, Cleanup, Tag, false, CP0),
	'$tag_cleanup'(CP0, Task0),
	nth_clause(M:Goal,_J,Ref),
	'$cleanup_on_exit'(CP0, TaskF).


'$creep_run_sources'(Setup, M, B, CP0, GoalNumber, Catcher, Ctx, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
        Task0 = cleanup( true, Catcher, Cleanup, Tag, true, CP0),
	TaskF = cleanup( true, Catcher, Cleanup, Tag, false, CP0),
    '$tag_cleanup'(CP0, Task0),
	trace_goal(B,M,Ctx,GoalNumber, CP0),
	'$cleanup_on_exit'(CP0, TaskF).

'$creep_run_private'(Setup, M, G, _CP, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
        Task0 = cleanup( true, Catcher, Cleanup, Tag, true, CP0),
	TaskF = cleanup( true, Catcher, Cleanup, Tag, false, CP0),
    '$tag_cleanup'(CP0, Task0),
    '$execute_non_stop'(M:G),
    '$cleanup_on_exit'(CP0, TaskF).

'$creep_run_refs'(Setup, M:Goal, Ref, CP, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
    Task0 = cleanup( true, Catcher, Cleanup, Tag, true, CP0),
    TaskF = cleanup( true, Catcher, Cleanup, Tag, false, CP0),
    '$tag_cleanup'(CP0, Task0),
    '$creep_clause'( Goal, M, Ref, CP ),
    '$cleanup_on_exit'(CP0, TaskF).


'$meta_hook'(MG,M:NG) :-
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
    '$debugger_prepare_meta_arguments'(As, Ms, NAs),
    NG=..[N|NAs],
    G \== NG,
    !.
'$meta_hook'(MG,MG).

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
'$interact'(Ports, MG, GoalNumber) :-
    '$ports_to_port'(Ports,P),
    '$zip_at_port'(P,GoalNumber,MG),
    !.
'$interact'(Ports, Module:G, L) :-
     nb_getval(creep,leap),
     !,
     '$ports_to_port'(Ports,P),
    ('$deterministic_port'(Ports) -> Deterministic = '?' ; Deterministic = ' '),
     '$enter_trace'(L, Module:G, Deterministic),
     '$action'('\n',P,L,G,Module,Deterministic).  
'$interact'(Ports, Module:G, L) :-
    '$ports_to_port'(Ports,P),
    '$ports_to_port'(Ports,P),
    ('$deterministic_port'(Ports) -> Deterministic = '?' ; Deterministic = ' '),
    '$id_goal'(L),        /* get goal no.	*/
    % at this point we are done with leap or skipe
    '$enter_trace'(L, Module:G, Deterministic),
    repeat,
    '$clear_input'(debugger_input),
    '$trace_msg'(P,G,Module,L,Deterministic),
    (
    '$has_leash'(P)
->
    prompt1(' ? '),
    get_char(user_input,C),
    '$action'(C,P,L,G,Module,Deterministic)
    ;
	'$action'('\n',P,L,G,Module,Deterministic),
	nl(debugger_output)                            
    ),
    !.


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

'$trace_port'(Ports, GoalNumber, Goal, Module, Ctx, CP,Info) :-
    '$ports_to_port'(Ports, Port),
    ('$deterministic_port'(Ports,Info) -> true ; true ),
    (
    true
    ;
    '$trace_port_'(Port, GoalNumber, Goal, Module, Ctx, CP,Info)
).

%        
% last.first
%'$ports_to_port'(P, _) :- writeln(P), fail. 
'$ports_to_port'([answer], exit).
'$ports_to_port'([answer,exit], exit).
'$ports_to_port'([answer,answer], exit).
'$ports_to_port'([call], call).
'$ports_to_port'([call,none], call).
'$ports_to_port'([call,redo], redo).
%'$ports_to_port'([call,exit], internal).
'$ports_to_port'([exit], exit).
'$ports_to_port'([exit,none], exit).
'$ports_to_port'([exit,exit], exit).
'$ports_to_port'([answer,none], exit).
'$ports_to_port'([exit,answer], exit).
%'$ports_to_port'(     [exit], internal).
%'$ports_to_port'([exit,redo], internal). %impossible?
'$ports_to_port'([fail,exit], fail).
'$ports_to_port'([fail,none], fail).
'$ports_to_port'([fail,answer], redo).
'$ports_to_port'([fail,exit], fail).
'$ports_to_port'([exit,fail], fail).
'$ports_to_port'(     [fail], fail).
'$ports_to_port'(     [fail,none], fail).
'$ports_to_port'([redo,answer], exit).
'$ports_to_port'([redo,none], redo).
'$ports_to_port'([redo,exit], exit).
'$ports_to_port'([redo], redo).
%'$ports_to_port'([!,answer], internal).
%'$ports_to_port'([!,exit], internal).
%'$ports_to_port'([!,redo], internal).
%'$ports_to_port'([!,fail], internal).
'$ports_to_port'([answer,!], exit).
'$ports_to_port'([exit,!], exit).
'$ports_to_port'([redo,!], redo).
'$ports_to_port'([redo], redo).
'$ports_to_port'([fail,!], fail).
%'$ports_to_port'([!], !).
'$ports_to_port'([!,none], exit).
'$ports_to_port'([exception(E),_], NE) :- '$publish_port'(E,NE).
'$ports_to_port'([exception(E)], NE) :- '$publish_port'(E,NE).
'$ports_to_port'([external_exception(E),_], NE) :- '$publish_port'(E,NE).
'$ports_to_port'([external_exception(E)], NE) :- '$publish_port'(E,NE).


'$deterministic_port'([exit,none]).
'$deterministic_port'([exit,exit]).
'$deterministic_port'([fail,exit]).
'$deterministic_port'([exit,fail]).
'$deterministic_port'(     [exit]).
'$deterministic_port'([!,answer]).
'$deterministic_port'([!,exit]).
'$deterministic_port'([answer,!]).
'$deterministic_port'([call]).

%'$publish_port'(redo(_), internal) :- !.
%'$publish_port'(fail(_), internal) :- !.
%'$publish_port'(abort, internal) :- !.
'$publish_port'(E, exception(E)).


%%% - abort: forward throw while the call is newer than goal
%% @pred trace_goal'( Exception, +Goal, +Mod, +GoalID )
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
%%% - forward through the debugger
trace_error(Event,_,_,_,_,_) :-
    throw(Event).

% Just fail here, don't really need toc all debugger, the user knows what he
% wants to do
'$loop_fail'(_GoalNumber, _G, _Module, _Creep) :-
    current_prolog_flag(debug, true),
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
    skip( debugger_input, 10).
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
'$action'(^,_,_,G,_,_) :- !,			% '
    '$print_deb_sterm'(G),
    skip( debugger_input, 10),
    fail.
'$action'(a,_,_,_,_,_) :- !,		% 'a		abort
    skip( debugger_input, 10),
    nodebug,
    abort.
'$action'(b,_,_,_,_,_) :- !,			% 'b		break
    '$stop_creeping'(_),
    skip( debugger_input, 10),
    break,
    fail.
'$action'('A',_,_,_,_,_) :- !,			% 'b		break
    skip( debugger_input, 10),
    '$stack_dump',
    fail.
'$action'(c,_,_,_,_,_) :- !,			% 'c		creep
    skip( debugger_input, 10),
    nb_setval(creep,creep),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
'$action'(e,_,_,_,_,_) :- !,			% 'e		exit
    halt.
'$action'(f,_,CallNumber,_,_,_) :- !,		% 'f		fail
    '$scan_number'( ScanNumber),
    ( ScanNumber == 0 -> Goal = CallNumber ; Goal = ScanNumber ),
     throw(error(debugger_event(fail,Goal),[])).
'$action'(h,_,_,_,_,_) :- !,			% 'h		help
    '$action_help',
    skip( debugger_input, 10),
    fail.
'$action'(?,_,_,_,_,_) :- !,			% '?		help
    '$action_help',
    skip( debugger_input, 10),
    fail.
'$action'(p,_,_,G,Module,_) :- !,		% 'p		print
    ((Module = prolog ; Module = user) ->
	 print(user_error,G), nl(user_error)
    ;
    print(user_error,Module:G), nl(user_error)
    ),
    skip( debugger_input, 10),
    fail.
'$action'(d,_,_,G,Module,_) :- !,		% 'd		display
    ((Module = prolog ; Module = user) ->
	 display(user_error,G), nl(user_error)
    ;
    display(user_error,Module:G), nl(user_error)
    ),
    skip( debugger_input, 10),
    fail.
'$action'(l,_,CallNumber,_,_,_) :- !,			% 'leap
	( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
  nb_setval(creep,leap),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
'$action'(z,_,CallNumber,_,_,_CP) :- !,
% 'z		zip, fast leap
  ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    nb_setval(creep,zip),
    nb_setval('$spy_on',stop),
    nb_setval('$spy_target',Goal).
% skip first call (for current goal),
% stop next time.
'$action'(k,_,_CallNumber,_,_,_) :- !,
'$ensure_number'(_ScanNumber),
nb_setval(creep,zip),
nb_setval('$spy_on',stop),
    nb_setval('$spy_target',0).
% skip first call (for current goal),
% stop next time.
'$action'(n,_,_,_,_,_) :- !,			% 'n		nodebug
    skip( debugger_input, 10),				% '
    % tell debugger never to stop.
    nodebug.
'$action'(r,P,CallNumber,_,_,_) :- !,	        % r		retry
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P==call) ->
      '$ilgl'(s)				%
    ;
    true
    ),
nb_setval(creep,leap),
nb_setval('$spy_on',ignore),
    nb_setval('$spy_target',Goal),
     throw(error(debugger_event(redo,Goal),[])).
'$action'(s,P,CallNumber,_,_,_) :- !,		% 's		skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P==call; P==redo) ->
      nb_setval(creep,leap),
nb_setval('$spy_on',ignore),
    nb_setval('$spy_target',Goal)
    ;
    '$ilgl'(s)				%
    ).
'$action'(t,P,CallNumber,_,_,_) :- !,		% 't		fast skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P=call; P=redo) ->
      nb_setval(creep,zip),
nb_setval('$spy_on',ignore),
    nb_setval('$spy_target',Goal)
    ;
    '$ilgl'(t)				%
    ).
'$action'(q,P,CallNumber,_,_,_) :- !,		% qst skip
    ( '$scan_number'(ScanNumber) -> Goal = ScanNumber ; Goal = CallNumber ),
    ( (P=call; P=redo) ->
       nb_setval(creep,leap),
       nb_setval('$spy_on',stop),
    nb_setval('$spy_target',Goal)
    ;
    '$ilgl'(t)				%
    ).
'$action'(+,_,_,G,M,_) :- !,			%%		spy this
    functor(G,F,N), spy(M:(F/N)),
    skip( debugger_input, 10),
    fail.
'$action'(-,_,_,G,M,_) :- !,			%% 	nospy this
    functor(G,F,N),
    nospy(M:(F/N)),
    skip( debugger_input, 10),
    fail.
'$action'(g,_,_,_,_,_) :- !,			% g		ancestors
    '$ensure_number'(HowMany),
    '$show_ancestors'(HowMany),
    fail.
'$action'('T',exception(G),_,_,_,_) :- !,	% T		throw
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

'$deb_inc_in_sterm_oldie'(94,L0,CN) :- !,
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
    yap_hacks:choicepoint(CP,_A_,_B,_C,_D,_E,_F),
    '$cps'(CPs).
'$cps'([]).


'$debugger_skip_trace_goal'([],[]).
'$debugger_skip_trace_goal'([CP|CPs],CPs1) :-
    yap_hacks:choicepoint(CP,_,prolog,trace_goal,4,(_;_),_),
    !,
    '$debugger_skip_trace_goal'(CPs,CPs1).
'$debugger_skip_trace_goal'([CP|CPs],[CP|CPs1]) :-
    !,
    '$debugger_skip_trace_goal'(CPs,CPs1).

'$debugger_skip_traces'([CP|CPs],CPs1) :-
    yap_hacks:choicepoint(CP,_,prolog,'$port',7,(_;_),_),
    !,
    '$debugger_skip_traces'(CPs,CPs1).
'$debugger_skip_traces'(CPs,CPs).

'$debugger_skip_loop_spy2'([CP|CPs],CPs1) :-
    yap_hacks:choicepoint(CP,_,prolog,'$loop_spy2',5,(_;_),_),
    !,
    '$debugger_skip_loop_spy2'(CPs,CPs1).
'$debugger_skip_loop_spy2'(CPs,CPs).

'$debugger_prepare_meta_arguments'([], [], []).
'$debugger_prepare_meta_arguments'([(A,B)|As], [0|Ms], [(NA,NB)|NAs]) :-
	!,
	'$debugger_prepare_meta_arguments'([A,B|As],[0,0|Ms],[NA,NB|NAs]).
'$debugger_prepare_meta_arguments'([(A;B)|As], [0|Ms], [(NA;NB)|NAs]) :-
  	!,
	'$debugger_prepare_meta_arguments'([A,B|As],[0,0|Ms],[NA,NB|NAs]).
'$debugger_prepare_meta_arguments'([(A->B)|As], [0|Ms], [(NA->NB)|NAs]) :-
    !,
    '$debugger_prepare_meta_arguments'([A,B|As],[0,0|Ms],[NA,NB|NAs]).
'$debugger_prepare_meta_arguments'([(A*->B)|As], [0|Ms], [(NA*->NB)|NAs]) :-	!,
    '$debugger_prepare_meta_arguments'([A,B|As],[0,0|Ms],[NA,NB|NAs]).
'$debugger_prepare_meta_arguments'([A|As], [0|Ms], [yap_hacks:trace(MA:GA)|NAs]) :-
    '$yap_strip_module'(A,MA,GA),
    GA \= trace(_),
	!,
   	'$debugger_prepare_meta_arguments'(As, Ms, NAs).
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


trace(G) :-
    '$trace'(G,outer).


%% @}
