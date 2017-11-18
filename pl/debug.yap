/**********************************************************************a***
*									 *
  *	 YAP Prolog 							 *
*									 *
  *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		debug.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	YAP debugger						 *
*									 *
*************************************************************************/


:- system_module( '$_debug', [], ['$trace_query'/4,
        '$init_debugger'/0,
        '$skipeol'/1]).



/**
  @defgroup Deb_Interaction Interacting with the debugger
  @ingroup YAPProgramming

Debugging with YAP is similar to debugging with C-Prolog. Both systems
include a procedural debugger, based on Byrd's four port model. In this
model, execution is seen at the procedure level: each activation of a
procedure is seen as a box with control flowing into and out of that
box.

In the four port model control is caught at four key points: before
entering the procedure, after exiting the procedure (meaning successful
evaluation of all queries activated by the procedure), after backtracking but
before trying new alternative to the procedure and after failing the
procedure. Each one of these points is named a port:

~~~~~
           *--------------------------------------*
   Call    |                                      |    Exit
---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
           |                                      |
           |  descendant(X,Z) :-                  |
<--------- +     offspring(X,Y), descendant(Y,Z). + <---------
   Fail    |                                      |    Redo
           *--------------------------------------*
~~~~~



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

~~~~~
* (1)  call:  quicksort([1,2,3],_38) ?
~~~~~

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
+ `s` - skip

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
completed execution, YAP fails until meeting the first active ancestor.

+ `a` - abort

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

*/

/*-----------------------------------------------------------------------------

				spy

-----------------------------------------------------------------------------*/


 /**
   *  ### Implementation
   *
   * The debugger is an interpreter. with main predicates:
   * - $trace: this is the API
   * - $trace_query: reduce a query to a goal
   * - $trace_goal: execute:
   *    + using the source, Luke
   *    + hooking into the WAM procedure call mechanism
   *    + asking Prolog to do it (system_library-builtins)
   *
   *	|flag	        | description	| initial | possible values
   *       ----------------------------------------------------------------
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
  *
  *
  * @param _Goal_ is the goal with a spy point
  * @return `call(Goal)`
*/
'$spy'([Mod|G]) :-
	'$trace'([Mod|G]).

/**
  * @pred $trace( +Goal )
  *
  *
  * @param _Goal_ is the goal to be examined.
  * @return `call(Goal)`
*/
% handle suspended goals
% take care with hidden goals.
%
% $trace may be called from user code, so be careful.
'$trace'([Mod|G]) :-
        '$stop_creeping'(_),
	current_prolog_flag(debug, false),
	!,
	'$execute_nonstop'(G,Mod).
'$trace'([Mod|G]) :-
	CP is '$last_choice_pt',
	gated_call(
	'$debugger_input',
		   '$trace_query'(G, Mod, CP, not_expanded),
		   E,
		   '$continue_debugging'(E)
		  ).

'$continue_debugging'(_) :- !,
	current_prolog_flag(debug, false).
'$continue_debugging'(exit) :- !, '$creep'.
'$continue_debugging'(answer) :- !, '$creep'.
'$continue_debugging'(fail) :- !, '$creep'.
'$continue_debugging'(_).





'$trace'([Mod|G], A1) :-
	G =.. L,
	lists:append( L, [A1], NL),
	NG =.. NL,
	'$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2) :-
	G =.. L,
	lists:append( L, [A1, A2], NL),
	NG =.. NL,
	'$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2, A3) :-
    G =.. L,
    lists:append( L, [A1, A2, A3], NL),
    NG =.. NL,
    '$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2, A3, A4) :-
    G =.. L,
    lists:append( L, [A1,A2,A3,A4], NL),
    NG =.. NL,
    '$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2, A3, A4, A5) :-
    G =.. L,
    lists:append( L, [A1, A2, A3, A4, A5], NL),
    NG =.. NL,
    '$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2, A3, A4, A5, A6) :-
	G =.. L,
	lists:append( L, [A1, A2, A3, A4, A5, A6], NL),
	NG =.. NL,
	'$trace'([Mod|NG]).

'$trace'([Mod|G], A1, A2, A3, A4, A5, A6, A7) :-
	G =.. L,
	lists:append( L, [A1, A2, A3, A4, A5, A6, A7 ], NL),
	NG =.. NL,
	'$trace'([Mod|NG]).

/**
  * @pred debugger_input.
  * name of stream used for debugging,
  * must be always connected to a tty.
  *
  * '$debugger_input': try to connect the debugger to an open terminal.
*/
'$debugger_input' :-
	stream_property(_,alias(debugger_input)),
	!.
'$debugger_input' :-
        stream_property(S,tty(true)),
        stream_property(S,input),
	!,
	set_stream(S,alias(debugger_input)).
'$debugger_input' :-
	current_prolog_flag(unix, true ), !,
        open('/dev/tty', read, _S, [alias(debugger_input),bom(false)]).
'$debugger_input' :-
        current_prolog_flag(windows, true ), !,
        open('CONIN$', read, _S, [alias(debugger_input),bom(false)]).


'$trace_meta_call'( G, M, CP ) :-
	'$trace_query'(G, M, CP, not_expanded ).

%% @pred '$trace_query'( +G, +M, +CP, +Expanded)
%
%% debug a complex query

'$trace_query'(V, M, CP, _) :-
	'$stop_creeping'(_),
	var(V), !,
	'$trace_query'(call(V), M, CP, _).
'$trace_query'(!, _, CP, _) :-
	!,
	'$$cut_by'(CP).
'$trace_query'('$cut_by'(M), _, _, _) :-
	!,
	'$$cut_by'(M).
'$trace_query'('$$cut_by'(M), _, _, _) :-
	!,
	'$$cut_by'(M).
'$trace_query'(true, _, _, _) :- !.
%'$trace_query'(fail, _, _, _) :- !, fail.
'$trace_query'(M:G, _, CP, Expanded) :-
        !,
        '$yap_strip_module'(M:G, M0, G0),
	'$trace_query'(G0, M0, CP, Expanded ).
'$trace_query'((A,B), M, CP, Expanded) :- !,
	'$trace_query'(A, M, CP, Expanded),
	'$trace_query'(B, M, CP, Expanded).
'$trace_query'((T->A;B), M, CP, Expanded) :- !,
	( '$trace_query'(T, M, CP, Expanded) -> '$trace_query'(A, M, CP, Expanded)
	;

	  '$trace_query'(B, M, CP, Expanded)
	).
'$trace_query'((T->A|B), M, CP, Expanded) :- !,
	(
	 '$trace_query'(T, M, CP, Expanded)
	->
	  '$trace_query'(A, M, CP, Expanded)
	;
	  '$trace_query'(B, M, CP, Expanded)
	).
'$trace_query'((T->A), M, CP, Expanded) :- !,
	( '$trace_query'(T, M, CP, Expanded) -> '$trace_query'(A, M, CP,  Expanded) ).
'$trace_query'((A;B), M, CP, Expanded) :- !,
	(
	 '$trace_query'(A, M, CP, Expanded)
	;
     '$trace_query'(B, M, CP, Expanded)
	).
'$trace_query'((A|B), M, CP, Expanded) :- !,
	(
	  '$trace_query'(A, M, CP, Expanded )
	;
     '$trace_query'(B, M, CP, Expanded )
	).
'$trace_query'((\+G), M, CP, Expanded) :- !,
	\+ '$trace_query'(G, M, CP, Expanded).
'$trace_query'((not(G)), M, CP, Expanded) :- !,
	\+ '$trace_query'(G, M, CP, Expanded).
'$trace_query'(once(G), M, CP, Expanded) :- !,
	once( '$trace_query'(G, M, CP, Expanded) ).
'$trace_query'(ignore(G), M, CP, Expanded) :- !,
	ignore( '$trace_query'(G, M, CP, Expanded) ).
'$trace_query'(G, M, _CP, _) :-
        % spy a literal
	'$id_goal'(L),
        catch(
                '$trace_goal'(G, M, L, H),
                E,
                '$re_trace_query'(E, G, M, L, H)
                ).

%% @pred $trace_goal( +Goal, +Module, +CallId, +CallInfo)
%%
%% Actually debugs a
%% goal!
'$trace_goal'(G, M, GoalNumber, _H) :-
	(
	 current_prolog_flag(debug, false)
	;
	 '__NB_getval__'('$debug_status',state(zip,Border,Spy), fail),
	 Border < GoalNumber,
	 ( Spy == ignore ; '$pred_being_spied'(G, M) )
	),
	writeln(go:G:M),
	!, 
	'$execute_nonstop'(G,M).
'$trace_goal'(G, M, GoalNumber, H) :-
	'$undefined'(G, M),
	!,
	'$get_undefined_pred'(G, M,  Goal, NM),
	( ( M == NM ; NM == prolog), G == Goal
	->
	  yap_flag( unknown, Action ),
	  '$undefp'([M|G], Action )
	;
	 '$trace_goal'(Goal, NM, GoalNumber, H)
	).
% meta system
'$trace_goal'(G, M, GoalNumber, H) :-
        '$is_metapredicate'(G, prolog),
        !,
        '$debugger_expand_meta_call'(M:G, [], G1),
	strip_module(G1, MF, NG),
	gated_call(
		   '$enter_trace'(GoalNumber, G, M, H, _What),
		   '$execute_nonstop'(NG,MF),
		   Port,
		   '$trace_port'(Port, GoalNumber, G, M, true, H)
                  ).
% system_
'$trace_goal'(G, M, GoalNumber, H) :-
	(
	 '$is_opaque_predicate'(G, M)
	;
	 'strip_module'(M:G, prolog, _NG)
	),
	!,
	gated_call(
		   '$enter_trace'(GoalNumber, G, M, H),
		   '$execute_nonstop'(G,M),
		   Port,
		   '$trace_port'(Port, GoalNumber, G, M, true, H)
                  ).
'$trace_goal'(G, M, GoalNumber, H) :-
	gated_call(
		   '$enter_trace'(GoalNumber, G, M, H),
		   '$debug'( GoalNumber, G, M, H),
		   Port,
		   '$trace_port'(Port, GoalNumber, G, M, true, H)
                  ).
'$enter_trace'(L, G, Module, Info) :-
        /* get goal no.	*/
        ( var(L) ->
        '__NB_getval__'('$spy_gn',L,fail),
        /* bump it			*/
        L1 is L+1,
        /* and save it globaly		*/
        '__NB_setval__'('$spy_gn',L1)
        ;
        true
        ),
        /* get goal list		*/
        '__NB_getval__'('$spy_glist',History,true),
	H  = [Info|History],
	Info = info(L,Module,G,_Retry,_Det,_HasFoundAnswers),
	'__B_setval__'('$spy_glist',H),
	/* and update it		*/
	'$port'(call,G,Module,L,deterministic, Info).

'$id_goal'(L) :-
	var(L),
        '__NB_getval__'('$spy_gn',L,fail),
        /* bump it			*/
        L1 is L+1,
        /* and save it globaly		*/
        '__NB_setval__'('$spy_gn',L1).

'$debug'(_, G, M, _H) :-
        '__NB_getval__'('$debug_status',state(zip,_Border,_), fail),
	!,
	'$execute_nonstop'( G, M ).
'$debug'(GoalNumber, G, M, Info) :-
	'$is_source'(G,M),
	!,
	'$trace_go'(GoalNumber, G, M, Info).
'$debug'(GoalNumber, G, M, Info) :-
	'$creep_step'(GoalNumber, G, M, Info).



'$trace_go'(GoalNumber, G, M, Info) :-
	X=marker(_,M,G),
        CP is '$last_choice_pt',
        clause(M:G, Cl, _),
	'$retry_clause'(GoalNumber, G, M, Info, X),
        '$trace_query'(Cl, M, CP, expanded).

'$creep_step'(GoalNumber, G, M, Info) :-
	X=marker(_,M,G),
	 '$$save_by'(CP),
	 '$static_clause'(G,M,_,Ref),
	 '$retry_clause'(GoalNumber, G, M, Info, X),
	 '$creep',
	 '$execute_clause'(G,M,Ref,CP).

'$retry_clause'(_GoalNumber, _G, _M, _Info, MarkerV) :-
	arg(1, MarkerV, V),
	var(V),
	!,
	nb_setarg(1,MarkerV, visited).
'$retry_clause'(GoalNumber, G, Module, Info, _X) :-
	'$trace_port_'(redo, GoalNumber, G, Module, Info).

%% @pred '$re_trace_query'( Exception, +Goal, +Mod, +GoalID )
%
% debugger code for exceptions. Recognised cases are:
%   - abort always forwarded
%   - redo resets the goal
%   - fail gives up on the goal.
'$re_trace_query'(abort, _G, _Module,  _GoalNumber, _H) :-
	!,
    abort.
'$re_trace_query'(forward(fail,G0), _G, __Module, GoalNumber, _H) :-
    GoalNumber =< G0,
    !,
    fail.
'$re_trace_query'(forward(redo,G0), G, M, GoalNumber, H) :-
    GoalNumber >= G0,
    !,
    catch(
            '$trace_goal'(G, M, GoalNumber, H),
            E,
            '$re_trace_query'(E, G,M, GoalNumber, H)
            ).
'$re_trace_query'(forward(C,G0), _G, _Module, _GoalNumber, _H) :-
            throw(forward(C,G0)).

'$trace_port'(Port, GoalNumber, G, Module, _CalledFromDebugger, Info) :-
	'$stop_creeping'(_) ,
	current_prolog_flag(debug, true),
        '__NB_getval__'('$debug_status',state(Skip,Border,_), fail),
	( Skip == creep -> true; '$id_goal'(GoalNumber) ; GoalNumber =< Border),
	!,
	'__NB_setval__'('$debug_status', state(creep, 0, stop)),
	'$trace_port_'(Port, GoalNumber, G, Module, Info),
	writeln(Port:G).
'$trace_port'(_Port, _GoalNumber, _G, _Module, _CalledFromDebugger, _Info).

'$trace_port_'(call, GoalNumber, G, Module, Info) :-
	'$port'(call,G,Module,GoalNumber,deterministic, Info).
'$trace_port_'(exit, GoalNumber, G, Module, Info) :-
	nb_setarg(6, Info, true),
	'$port'(exit,G,Module,GoalNumber,deterministic, Info).
'$trace_port_'(answer, GoalNumber, G, Module, Info) :-
	'$port'(exit,G,Module,GoalNumber,nondeterministic, Info).
'$trace_port_'(redo, GoalNumber, G, Module, Info) :-
	'$port'(redo,G,Module,GoalNumber,nondeterministic, Info), /* inform user_error	*/
	'$stop_creeping'(_ ).
'$trace_port_'(fail, GoalNumber, G, Module, Info) :-
	'$port'(fail,G,Module,GoalNumber,deterministic, Info). /* inform user_error		*/
'$trace_port_'(! ,_GoalNumber,_G,_Module,_Imfo) :- /* inform user_error		*/
    !.
'$trace_port_'(exception(E), GoalNumber, G, Module, Info) :-
    '$TraceError'(E, GoalNumber, G, Module, Info).
'$trace_port_'(external_exception(E), GoalNumber, G, Module, Info) :-
    '$TraceError'(E, GoalNumber, G, Module, Info).
 

%%% - abort: forward throw while the call is newer than goal
'$TraceError'( abort, _, _, _, _).
'$TraceError'(forward(redo,_G0), _, _, _, _).
%%% - backtrack long distance
'$TraceError'(forward(fail,_G0),GoalNumber, _, _, _) :- !,
	throw(error(fail(GoalNumber))).
%%%
%%% - forward through the debugger
'$TraceError'(forward('$wrapper',Event), _, _, _, _) :-
	!,
	throw(Event).
%%% - anything else, leave to the user and restore the catch
'$TraceError'(Event, GoalNumber, G, Module, CalledFromDebugger) :-
	'$debug_error'(Event),
	'$system_catch'(
		     ('$port'(exception(Event),G,Module,GoalNumber,_,creep),fail),
		     Module,
		     Error,
		     '$TraceError'(Error, GoalNumber, G, Module, CalledFromDebugger)
		    ).


'$debug_error'(Event) :-
	'$Error'(Event), fail.
'$debug_error'(_).


% just fail here, don't really need to call debugger, the user knows what he
% wants to do
'$loop_fail'(_GoalNumber, _G, _Module,Creep) :-
	current_prolog_flag(debug, true),
	'$continue_debugging'(fail, Creep),
	fail.

%
% skip a goal or a port
%

'$gg'(CP,Goal) :-
	CP is '$last_choice_point',
	Goal.

'$port'(_P,_G,_Module,_L,_Determinic, _Info ) :-  %%> debugging done
	current_prolog_flag(debug, false),
	!.
'$port'(_P, _G, _M,GoalNumber,_Determinic, _Info ) :-   %%> leap
        '__NB_getval__'('$debug_status',state(leap,Border,_), fail),
	GoalNumber > Border,
	!.
'$port'(P,G,Module,L,Deterministic, Info) :-
	% at this point we are done with leap or skip
	repeat,
	'$trace_msg'(P,G,Module,L,Deterministic),
	(
	  '$unleashed'(P) ->
	  '$action'('\n',P,L,G,Module,Info),
	  put_code(user_error, 10)
	  ;
	 write(user_error,' ? '),
         '$clear_input'(debugger_input),
         get_char(debugger_input,C),
	 '$action'(C,P,L,G,Module,_Info)
	),
	!.

'$trace_msg'(P,G,Module,L,Deterministic) :-
	flush_output(user_output),
	flush_output(user_error),
	functor(P,P0,_),
	(P = exit, Deterministic \= deterministic -> Det = '?' ; Det = ' '),
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
	format(user_error,'~a~a~a       (~d)    ~q:',[Det,CSPY,SLL,L,P0]),
	'$debugger_write'(user_error,GW).

'$unleashed'(call) :- get_value('$leash',L), L /\ 2'1000 =:= 0. %'
'$unleashed'(exit) :- get_value('$leash',L), L /\ 2'0100 =:= 0. %'
'$unleashed'(redo) :- get_value('$leash',L), L /\ 2'0010 =:= 0. %'
'$unleashed'(fail) :- get_value('$leash',L), L /\ 2'0001 =:= 0. %'
% the same as fail.
'$unleashed'(exception(_)) :- get_value('$leash',L), L /\ 2'10000 =:= 0.  %'

'$debugger_write'(Stream, G) :-
	current_prolog_flag( debugger_print_options, OUT ), !,
	write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
	writeq(Stream, G).

'$action'('\r',P,CallNumber,G,Module,H) :- !,	% newline 	creep
	get_char( debugger_input,C),
	'$action'(C,P,CallNumber,G,Module,H).
'$action'('\n',_,_,_,_,_) :- !,			% newline 	creep
        '__NB_setval__'('$debug_status', state(creep, 0, stop)).
'$action'(!,_,_,_,_,_) :- !,			% ! 'g		execute
	read(debugger_input, G),
	% don't allow yourself to be caught by creep.
	current_prolog_flag(debug, OldDeb),
	set_prolog_flag(debug, false),
	ignore( G ),
	% at this point we are done with leap or skip
	set_prolog_flag(debug, OldDeb),
%	skip( debugger_input, 10),                        % '
	fail.
'$action'(<,_,_,_,_,_) :- !,			% <'Depth
	'$new_deb_depth',
	skip( debugger_input, 10),
	fail.
'$action'('C',_,_,_,_,_) :-
	yap_flag(system_options, Opts),
        lists:memberchk( call_tracer, Opts),
	!,			% <'Depth
	skip( debugger_input, 10),
        '__NB_setval__'('$debug_status', state(creep, 0, stop)).
'$action'(^,_,_,G,_,_) :- !,			% '
	'$print_deb_sterm'(G),
	skip( debugger_input, 10),
	fail.
'$action'(a,_,_,_,_,_) :- !,		% 'a		abort
	skip( debugger_input, 10),
    '$stop_creeping'(_),
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
	'__NB_setval__'('$debug_status',status(creep,0,stop)).
'$action'(e,_,_,_,_,_) :- !,			% 'e		exit
	halt.
'$action'(f,_,_,_,_,_) :- !,		% 'f		fail
	'$scan_number'( GoalId),    %'f
	throw(forward(fail,GoalId)).
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
'$action'(l,_,_CallNumber,_,_,_) :- !,			% 'l		leap
	skip( debugger_input, 10),
        '__NB_setval__'('$debug_status', state(leap, 0, stop)).
'$action'(z,_,_CallNumber,_,_,_H) :- !,		% 'z		zip, fast leap
	skip( debugger_input, 10),			% 'z
        '__NB_setval__'('$debug_status', state(zip, 0, stop)).
        % skip first call (for current goal),
	% stop next time.
'$action'(k,_,CallNumber,_,_,_) :- !,		% 'k		zip, fast leap
	skip( debugger_input, 10),			% '
         '__NB_setval__'('$debug_status', state(zip, CallNumber, ignore)).
        % skip first call (for current goal),
	% stop next time.
'$action'(n,_,_,_,_,_) :- !,			% 'n		nodebug
	skip( debugger_input, 10),				% '
	% tell debugger never to stop.
        '__NB_setval__'('$debug_status', state(zip, 0, ignore)),
 	nodebug.
'$action'(r,_,_,_,_,_) :- !,		        % 'r		retry
    '$scan_number'(ScanNumber),		% '
%	set_prolog_flag(debug, true),
    throw(forward(redo,ScanNumber)).
'$action'(s,P,CallNumber,_,_,_) :- !,		% 's		skip
	skip( debugger_input, 10),				% '
	( (P=call; P=redo) ->
             '__NB_setval__'('$debug_status', state(leap, CallNumber, ignore) ) ;
	    '$ilgl'(s)				% '
	).
'$action'(t,P,CallNumber,_,_,_) :- !,		% 't		fast skip
	skip( debugger_input, 10),				% '
	( (P=call; P=redo) ->
            '__NB_setval__'('$debug_status', state(zip, CallNumber, ignore))	;
	    '$ilgl'(t)				% '
	).
'$action'(q,P,CallNumber,_,_,_) :- !,		% 'qst skip
	skip( debugger_input, 10),				% '
	( (P=call; P=redo) ->
            '__NB_setval__'('$debug_status', state(leap, CallNumber, stop))	;
	    '$ilgl'(t)				% '
	).
'$action'(+,_,_,G,M,_) :- !,			% '+		spy this
	functor(G,F,N), spy(M:(F/N)),
	skip( debugger_input, 10),			% '
	fail.
'$action'(-,_,_,G,M,_) :- !,			% '-		nospy this
	functor(G,F,N), nospy(M:(F/N)),
	skip( debugger_input, 10),			% '
	fail.
'$action'(g,_,_,_,_,_) :- !,			% 'g		ancestors
        '$scan_number'(HowMany),         % '
        '$show_ancestors'(HowMany),
	fail.
'$action'('T',exception(G),_,_,_,_) :- !,	% 'T		throw
	throw( forward('$wrapper',G)).
'$action'(C,_,_,_,_,_) :-
	skip( debugger_input, 10),
	'$ilgl'(C),
	fail.

'$continue_debugging'(_, creep) :-
	'__NB_getval__'('$if_skip_mode',no_skip),
	!,
	'$creep'.
'$continue_debugging'(_, _).

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
'$show_ancestors'([info(L,M,G,Retry,Det,_Exited)|History],HowMany) :-
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

'$scan_number'(Nb) :-
	readutil:read_line_to_codes( debugger_input, S),
        number_codes(Nb,S).

'$print_deb_sterm'(G) :-
	'$get_sterm_list'(L), !,
	'$deb_get_sterm_in_g'(L,G,A),
	recorda('$debug_sub_skel',L,_),
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
	( C =:= "^", N =\= 0 -> get_code(debugger_input, CN),
				'$get_sterm_list'([N|L0],CN,0,L) ;
	  C >= "0", C =< "9" -> NN is 10*N+C-"0", get_code(debugger_input, CN),
				'$get_sterm_list'(L0,CN,NN,L);
	  C =:= 10 -> (N =:= 0 -> L = L0 ; L=[N|L0]) ).

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
'$get_deb_depth_char_by_char'(_C,_,10) :- skip( debugger_input, 10).

'$set_deb_depth'(D) :-
	yap_flag(debugger_print_options,L),
	'$delete_if_there'(L, max_depth(_), max_depth(D), LN),
	yap_flag(debugger_print_options,LN).

'$delete_if_there'([], _, TN, [TN]).
'$delete_if_there'([T|L], T, TN, [TN|L]).
'$delete_if_there'([Q|L], T, TN, [Q|LN]) :-
	'$delete_if_there'(L, T, TN, LN).

  '$debugger_deterministic_goal'(exit).
  '$debugger_deterministic_goal'(fail).
  '$debugger_deterministic_goal'(!).
  '$debugger_deterministic_goal'(exception(_)).
  '$debugger_deterministic_goal'(external_exception(_)).


'$cps'([CP|CPs]) :-
    yap_hacks:choicepoint(CP,A,B,C,D,E,F),
    write(A:B:C:D:E:F),nl,
    '$cps'(CPs).
'$cps'([]).


'$debugger_skip_trace_query'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$trace_query',4,(_;_),_), !,
	'$debugger_skip_trace_query'(CPs,CPs1).
'$debugger_skip_trace_query'(CPs,CPs).

'$debugger_skip_traces'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$port',4,(_;_),_), !,
	'$debugger_skip_traces'(CPs,CPs1).
'$debugger_skip_traces'(CPs,CPs).

'$debugger_skip_loop_spy2'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$loop_spy2',5,(_;_),_), !,
	'$debugger_skip_loop_spy2'(CPs,CPs1).
'$debugger_skip_loop_spy2'(CPs,CPs).

'$debugger_expand_meta_call'( G, VL, M:G2 ) :-
    '$expand_meta_call'( G, VL, G0 ),
    '$yap_strip_module'( G0, M, G1 ),
    (
	'$is_system_predicate'(G0,M) ->
	    '$debugger_process_meta_arguments'(G1, M, G2)
     ;
     G1 = G2
    ).

'$debugger_process_meta_arguments'(G, M, G1) :-
	'$yap_strip_module'( M:G, MM, GM ),
	functor(GM,F,N),
	'$meta_predicate'(F,MM,N,D), !, % we're in an argument
	D =.. [F|BMs],
	GM =.. [F|BGs],
	'$ldebugger_process_meta_args'(BGs, M, BMs, BG1s),
	G1 =.. [F|BG1s].
'$debugger_process_meta_arguments'(G, _M, G).

'$ldebugger_process_meta_args'([], _, [], []).
'$ldebugger_process_meta_args'([G|BGs], M, [N|BMs], ['$trace'([M1|G1])|BG1s]) :-
    number(N),
    N >= 0,
	'$yap_strip_module'( M:G, M1, G1 ),
	functor(G1, Na, _),
	Na \= '$trace',
	!,
	'$ldebugger_process_meta_args'(BGs, M, BMs, BG1s).
'$ldebugger_process_meta_args'([G|BGs], M, [_|BMs], [G|BG1s]) :-
	'$ldebugger_process_meta_args'(BGs, M, BMs, BG1s).


'$creep'(creep) :- '$creep'.
'$creep'(leap) :- '$creep'.
'$creep'(zip).
