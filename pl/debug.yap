/*************************************************************************
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


:- system_module( '$_debug', [], ['$do_spy'/4,
        '$init_debugger'/0,
        '$skipeol'/1]).



/** @defgroup Deb_Interaction Interacting with the debugger
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

% ok, I may have a spy point for this goal, or not.
%  if I do, I should check what mode I am in.
% Goal/Mode          Have Spy     Not Spied
% Creep                 Stop        Stop
% Leap                  Stop        Create CP
% Skip               Create CP     Create CP
% FastLeap              Stop        Ignore
% FastIgnore           Ignore       Ignore


%	flag		description		initial possible values

%	spy_gn		goal number		1	1...
%	spy_trace	trace		0	0, 1
%	spy_skip	leap			off	Num (stop level)
%	debug_prompt	stop at spy points	on	on,off
% a flip-flop is also used
%	when 1 spying is enabled *(the same as spy stop).


%'$spy'(G) :- write(user_error,'$spy'(G)), nl, fail.
%
% handle suspended goals
% take care with hidden goals.
%
% $spy may be called from user code, so be careful.
'$spy'([Mod|G]) :-
	current_prolog_flag(debug, false), !,
	'$execute_nonstop'(G,Mod).
'$spy'([Mod|G]) :-
	CP is '$last_choice_pt',
	'$debugger_input',
	'$do_spy'(G, Mod, CP, spy).

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
	'$do_spy'(G, M, CP, spy ).

% last argument to do_spy says that we are at the end of a context. It
% is required to know whether we are controlled by the debugger.
%'$do_spy'(V, M, CP, Flag) :-
%	writeln('$do_spy'(V, M, CP, Flag)), fail.
'$do_spy'(V, M, CP, Flag) :-
        '$stop_creeping'(_),
	var(V), !,
	'$do_spy'(call(V), M, CP, Flag).
'$do_spy'(!, _, CP, _) :-
	!, '$$cut_by'(CP).
'$do_spy'('$cut_by'(M), _, _, _) :-
	!, '$$cut_by'(M).
'$do_spy'('$$cut_by'(M), _, _, _) :-
	!, '$$cut_by'(M).
'$do_spy'(true, _, _, _) :- !.
%'$do_spy'(fail, _, _, _) :- !, fail.
'$do_spy'(M:G, _, CP, CalledFromDebugger) :- !,
	'$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'((A,B), M, CP, CalledFromDebugger) :- !,
	'$do_spy'(A, M, CP, debugger),
	'$do_spy'(B, M, CP, CalledFromDebugger).
'$do_spy'((T->A;B), M, CP, CalledFromDebugger) :- !,
	( '$do_spy'(T, M, CP, debugger) -> '$do_spy'(A, M, CP, CalledFromDebugger)
	;

	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((T->A|B), M, CP, CalledFromDebugger) :- !,
	(
	 '$do_spy'(T, M, CP, debugger)
	->
	  '$do_spy'(A, M, CP, CalledFromDebugger)
	;
        'stop_creeping'(_),
	  '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((T->A), M, CP, CalledFromDebugger) :- !,
	( '$do_spy'(T, M, CP, debugger) -> '$do_spy'(A, M, CP,  CalledFromDebugger) ).
'$do_spy'((A;B), M, CP, CalledFromDebugger) :- !,
	(
	  '$do_spy'(A, M, CP, CalledFromDebugger)
	;
     '$stop_creeping'(_),
     '$do_spy'(B, M, CP, CalledFromDebugger)
	).
'$do_spy'((A|B), M, CP, CalledFromDebugger) :- !,
	(
	  '$do_spy'(A, M, CP, CalledFromDebugger )
	;
     '$stop_creeping'(_) ,
     '$do_spy'(B, M, CP, CalledFromDebugger )
	).
'$do_spy'((\+G), M, CP, CalledFromDebugger) :- !,
	\+ '$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'((not(G)), M, CP, CalledFromDebugger) :- !,
	\+ '$do_spy'(G, M, CP, CalledFromDebugger).
'$do_spy'(G, Module, _, CalledFromDebugger) :-
        '__NB_getval__'('$spy_gn',L,fail),		/* get goal no.			*/
	L1 is L+1,			/* bump it			*/
	'__NB_setval__'('$spy_gn',L1),	/* and save it globaly		*/
        '__NB_getval__'('$spy_glist',History,true),	/* get goal list		*/
	'__B_setval__'('$spy_glist',[info(L,Module,G,_Retry,_Det,_HasFoundAnswers)|History]),
	/* and update it		*/
	'$loop_spy'(L, G, Module, CalledFromDebugger).

% we are skipping, so we can just call the goal,
% while leaving the minimal structure in place.
'$loop_spy'(GoalNumber, G, Module, CalledFromDebugger) :-
	'$current_choice_point'(CP),
	'$system_catch'('$loop_spy2'(GoalNumber, G, Module, CalledFromDebugger, CP),
		    Module, error(Event,Context),
		    '$loop_spy_event'(error(Event,Context), GoalNumber, G, Module, CalledFromDebugger)).

% handle weird things happening in the debugger.
'$loop_spy_event'('$pass'(Event), _, _, _, _) :- !,
	throw(Event).
'$loop_spy_event'(error('$retry_spy'(G0),_), GoalNumber, G, Module, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$loop_spy'(GoalNumber, G, Module, CalledFromDebugger).
'$loop_spy_event'(error('$retry_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$retry_spy'(GoalNumber),[])).
'$loop_spy_event'(error('$fail_spy'(G0),_), GoalNumber, G, Module, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$loop_fail'(GoalNumber, G, Module, CalledFromDebugger).
'$loop_spy_event'(error('$fail_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$fail_spy'(GoalNumber),[])).
'$loop_spy_event'(error('$done_spy'(G0),_), GoalNumber, _G, _, CalledFromDebugger) :-
	G0 >= GoalNumber, !,
	'$continue_debugging'(zip, CalledFromDebugger).
'$loop_spy_event'(error('$done_spy'(GoalNumber),_), _, _, _, _) :- !,
	throw(error('$done_spy'(GoalNumber),[])).
'$loop_spy_event'(Event, GoalNumber, G, Module, CalledFromDebugger) :-
	'$debug_error'(Event),
	'$system_catch'(
		     ('$trace'(exception(Event),G,Module,GoalNumber,_),fail),
		     Module,
		     error(NewEvent,NewContext),
		     '$loop_spy_event'(error(NewEvent,NewContext), GoalNumber, G, Module, CalledFromDebugger)
		    ).


'$debug_error'(Event) :-
	'$Error'(Event), fail.
'$debug_error'(_).


% just fail here, don't really need to call debugger, the user knows what he
% wants to do
'$loop_fail'(_GoalNumber, _G, _Module, CalledFromDebugger) :-
	'$continue_debugging'(fail, CalledFromDebugger),
	fail.

/**
 * core routine for the debugger
 *
 * @param _ GoalNumbera id
 * @param _ S9c
 * @param _
 * @param Retry
 * @param Det
 * @param false
 *
 * @return
*/
'$loop_spy2'(GoalNumber, G, Module, CalledFromDebugger, CP) :-
/* the following choice point is where the predicate is  called */
	   '__NB_getval__'('$spy_glist',[Info|_],true),	/* get goal list		*/
	   Info = info(_,_,_,Retry,Det,false),
	   (
	    /* call port */
	    '$enter_goal'(GoalNumber, G, Module),
	    '$spycall'(G, Module, CalledFromDebugger, Retry),
	    '$stop_creeping'(_) ,
	    % make sure we are in system mode when running the debugger.
	    (
	      '$debugger_deterministic_goal'(G) ->
	      Det=true
	    ;
	      Det=false
	    ),
	    /* go execute the continuation	*/
	    (
	       /* exit port */
         Retry = false,
         /* found an answer, so it can redo */
         nb_setarg(6, Info, true),
         '$show_trace'(exit,G,Module,GoalNumber,Det),	/* output message at exit	*/
        /* exit port */
        /* get rid of deterministic computations */
        (
         Det == true
		->
         '$$cut_by'(CP)
		;
         true
        ),
         '$continue_debugging'(exit, CalledFromDebugger)
	     ;
	     /* make sure we are in system mode when running the debugger. */
         /* backtracking from exit				*/
         /* we get here when we want to redo a goal		*/
         /* redo port */
         (
          arg(6, Info, true)
         ->
          '$stop_creeping'(_) ,
          '$show_trace'(redo,G,Module,GoalNumber,_), /* inform user_error		*/
         nb_setarg(6, Info, false)
         ;
          true
         ),
	     '$continue_debugging'(fail, CalledFromDebugger),
	     fail			/* to backtrack to spycall	*/
        )
	  ;
        '$stop_creeping'(_) ,
	    '$show_trace'(fail,G,Module,GoalNumber,_), /* inform at fail port		*/
       '$continue_debugging'(fail, CalledFromDebugger),
	    /* fail port */
	    fail
       ).

'$enter_goal'(GoalNumber, G, Module) :-
    '$zip'(GoalNumber, G, Module), !.
'$enter_goal'(GoalNumber, G, Module) :-
    '$trace'(call, G, Module, GoalNumber, _).

'$show_trace'(_, G, Module, GoalNumber,_) :-
	'$zip'(GoalNumber, G, Module), !.
'$show_trace'(P,G,Module,GoalNumber,Deterministic) :-
	'$trace'(P,G,Module,GoalNumber,Deterministic).

%
% skip a goal or a port
%
'$zip'(_GoalNumber, _G, _Module) :-
	current_prolog_flag(debug, false),
	!.
'$zip'(GoalNumber, G, Module) :-
    '__NB_getval__'('$debug_run',StopPoint,fail),
    % zip mode off, we cannot zip
    StopPoint \= off,
    (
      % skip spy points (eg, s).
      StopPoint == spy
    ->
      \+ '$pred_being_spied'(G, Module)
    ;
      % skip goals (eg, l).
      number(StopPoint)
    ->
      StopPoint < GoalNumber
    ).



%
'$spycall'(G, M, _, _) :-
    current_prolog_flag( debug, false),
    !,
    '$execute_nonstop'(G,M).
'$spycall'(G, M, _, _) :-
	'__NB_getval__'('$debug_jump',true, fail),
	!,
	( '$is_metapredicate'(G, M)
	->
	    '$expand_meta_call'(M:G, [], G1)
	;
	G = G1
	),
	'$execute_nonstop'(G1,M).
'$spycall'(G, M, CalledFromDebugger, InRedo) :-
	 '$is_metapredicate'(G, M),
	 '$debugger_expand_meta_call'(M:G, [], G10),
	 G10 \== M:G,
	 !,
	 '$debugger_input',
	 G10 = NM:NG,
	 '$spycall_f'(NG, NM, CalledFromDebugger, InRedo).
'$spycall'(G, M, CalledFromDebugger, InRedo) :-
	 '$spycall_f'(G, M, CalledFromDebugger, InRedo).

'$spycall_f'(G, M, _, _) :-
	( '$is_system_predicate'(G,M) ; '$tabled_predicate'(G,M) ),
	!,
	'$continue_debugging_goal'(yes, '$execute_nonstop'(G,M)).
'$spycall_f'(G, M, CalledFromDebugger, InRedo) :-
	'$spycall_expanded'(G, M, CalledFromDebugger, InRedo).

'$spycall_expanded'(G, M, CalledFromDebugger, InRedo) :-
	'$undefined'(G, M), !,
	'$get_undefined_pred'(G, M, Goal, NM), NM \= M,
	'$spycall'(Goal, NM, CalledFromDebugger, InRedo).
'$spycall_expanded'(G, M, _CalledFromDebugger, InRedo) :-
	CP is '$last_choice_pt',
	(
      '$is_source'( G, M )                        % use the interpreter
    ->
	(
	    '$clause'(G, M, Cl, _)
		     *->
    % I may backtrack to here from far away
		     ( '$do_spy'(Cl, M, CP, debugger) ; InRedo = true )
	)
	 ;
	 (
	     '$static_clause'(G,M,_,R)
	*->
	'$stop_creeping'(_),
    (
	'$creep'('$execute_clause'(G, M, R, CP), M)
     ;
     InRedo = true
    )
	 )
	 ;
	( '$continue_debugging_goal'(yes, '$execute_nonstop'(G,M) ) ;  InRedo = true )
    ).
    % I may backtrack to here from far away

%
%
'$creep'('$execute_clause'(G,Mod,Ref,CP),_M) :-
	(
	 '$$save_by'(CP1),
	 '$creep',
	 '$execute_clause'(G,Mod,Ref,CP),
	 '$$save_by'(CP2),
	 (CP1 == CP2 -> ! ; ( true ; '$creep', fail ) ),
	  '$stop_creeping'(_)
	;
     '$stop_creeping'(_) ,
	 fail
	).
'$creep'(G,M) :-
	(
	 '$$save_by'(CP1),
	 '$creep',
	 '$execute_nonstop'(G,M),
	 '$$save_by'(CP2),
	 (CP1 == CP2 -> ! ; ( true ; '$creep', fail ) ),
	  '$stop_creeping'(_)
	;
	  fail
	).


/**
 * call predicate M:G within the ddebugger
 *
 *
 * @return
*/
'$trace'(G,M) :-
	(
	 '$$save_by'(CP1),
	 '$creep',
	 '$execute0'( G, M ),
	 '$$save_by'(CP2),
	 (CP1 == CP2 -> ! ; ( true ; '$creep', fail ) ),
	  '$stop_creeping'
	;
	  fail
	).

'$tabled_predicate'(G,M) :-
	'$predicate_flags'(G,M,F,F),
	F /\ 0x00000040 =\= 0.

%'$trace'(P,G,Module,L,Deterministic) :-
%	'__NB_getval__'('$system_mode',On,fail), writeln(On), fail.
'$trace'(P,G,Module,L,Deterministic) :-
	% at this point we are done with leap or skip
	'__NB_setval__'('$debug_run',off),
	% but creep is default
	'__NB_setval__'('$trace',on),
	% make sure we run this code outside debugging mode.
%	set_prolog_flag(debug, false),
	repeat,
	'$trace_msg'(P,G,Module,L,Deterministic),
	(
	  '$unleashed'(P) ->
	  '$action'(10,P,L,G,Module,Debug),
	  put_code(user_error, 10)
	  ;
	 write(user_error,' ? '), get_code(debugger_input,C),
	  '$action'(C,P,L,G,Module,Debug)
	),
/*	(Debug = on
	->
	 set_prolog_flag(debug, true)
	;
	 Debug = zip
	->
	 set_prolog_flag(debug, true)
	;
	 set_prolog_flag(debug, false)
	), */
	!.

'$trace_msg'(P,G,Module,L,Deterministic) :-
	flush_output(user_output),
	flush_output(user_error),
	functor(P,P0,_),
	(P = exit, Deterministic \= true -> Det = '?' ; Det = ' '),
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
'$unleashed'(exception(_)) :- get_value('$leash',L), L /\ 2'0001 =:= 0.  %'

'$debugger_write'(Stream, G) :-
	current_prolog_flag( debugger_print_options, OUT ), !,
	write_term(Stream, G, OUT).
'$debugger_write'(Stream, G) :-
	writeq(Stream, G).

'$action'(13,P,CallNumber,G,Module,Zip) :- !,	% newline 	creep
	get_code( debugger_input,C),
	'$action'(C,P,CallNumber,G,Module,Zip).
'$action'(10,_,_,_,_,on) :- !,			% newline 	creep
	'__NB_setval__'('$debug_jump',false).
'$action'(0'!,_,_,_,_,_) :- !,			% ! 'g		execute
	read(debugger_input, G),
	% don't allow yourself to be caught by creep.
%	current_prolog_flag(debug, OldDeb),
%	 set_prolog_flag(debug, false),
	( '$execute'(G) -> true ; true),
	% at this point we are done with leap or skip
	'__NB_setval__'('$debug_run',off),
	% but creep is default
	'__NB_setval__'('$trace',on),
%	set_prolog_flag(debug, OldDeb),
%	'$skipeol'(0'!),                        % '
	fail.
    '$action'(0'<,_,_,_,_,_) :- !,			% <'Depth
    	'$new_deb_depth',
    	'$skipeol'(0'<),
    	fail.
        '$action'(0'C,_,_,_,_,_) :-
        yap_flag(system_options, Opts),
    lists:memberchk( call_tracer, Opts),
            !,			% <'Depth
        	'$skipeol'(0'C),
        '$start_low_level_trace',
        	'__NB_setval__'('$debug_jump',false).
'$action'(0'^,_,_,G,_,_) :- !,			% '
	'$print_deb_sterm'(G),
	'$skipeol'(0'^),
	fail.
'$action'(0'a,_,_,_,_,off) :- !,		% 'a		abort
	'$skipeol'(0'a),
	abort.
'$action'(0'b,_,_,_,_,_) :- !,			% 'b		break
	'$skipeol'(0'b),
	break,
	fail.
'$action'(0'A,_,_,_,_,_) :- !,			% 'b		break
	'$skipeol'(0'A),
	'$hacks':'$stack_dump',
	fail.
'$action'(0'c,_,_,_,_,on) :- !,			% 'c		creep
	'$skipeol'(0'c),
	'__NB_setval__'('$debug_jump',false).
'$action'(0'e,_,_,_,_,_) :- !,			% 'e		exit
	'$skipeol'(0'e),
	halt.
'$action'(0'f,_,CallId,_,_,_) :- !,		% 'f		fail
	'$scan_number'(0'f, CallId, GoalId),    %'f
	throw(error('$fail_spy'(GoalId),[])).
'$action'(0'h,_,_,_,_,_) :- !,			% 'h		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'?,_,_,_,_,_) :- !,			% '?		help
	'$action_help',
	'$skipeol'(104),
	fail.
'$action'(0'p,_,_,G,Module,_) :- !,		% 'p		print
	((Module = prolog ; Module = user) ->
	    print(user_error,G), nl(user_error)
	;
	    print(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'p),
	fail.
'$action'(0'd,_,_,G,Module,_) :- !,		% 'd		display
	((Module = prolog ; Module = user) ->
	    display(user_error,G), nl(user_error)
	;
	    display(user_error,Module:G), nl(user_error)
	),
	'$skipeol'(0'd),
	fail.
'$action'(0'l,_,_,_,_,on) :- !,			% 'l		leap
	'$skipeol'(0'l),
	'__NB_setval__'('$debug_run',spy),
	'__NB_setval__'('$debug_jump',false).
'$action'(0'z,_,_,_,_,zip) :- !,		% 'z		zip, fast leap
	'$skipeol'(0'z),			% 'z
	'__NB_setval__'('$debug_run',spy),
	'__NB_setval__'('$debug_jump',true).
	% skip first call (for current goal),
	% stop next time.
'$action'(0'k,_,_,_,_,zip) :- !,		% 'k		zip, fast leap
	'$skipeol'(0'k),			% '
	'__NB_setval__'('$debug_run',spy),
	'__NB_setval__'('$debug_jump',true).
	% skip first call (for current goal),
	% stop next time.
'$action'(0'n,_,_,_,_,off) :- !,			% 'n		nodebug
	'$skipeol'(0'n),				% '
	% tell debugger never to stop.
	'__NB_setval__'('$debug_run', -1),
	'__NB_setval__'('$debug_jump',true),
	nodebug.
'$action'(0'r,_,CallId,_,_,_) :- !,		        % 'r		retry
    '$scan_number'(0'r,CallId,ScanNumber),		% '
%	set_prolog_flag(debug, true),
	throw(error('$retry_spy'(ScanNumber),[])).
'$action'(0's,P,CallNumber,_,_,on) :- !,		% 's		skip
	'$skipeol'(0's),				% '
	(

     (P=call; P=redo) ->
	  '__NB_setval__'('$debug_run',CallNumber),
	  '__NB_setval__'('$debug_jump',false)
	;
	    '$ilgl'(0's)				% '
	).
'$action'(0't,P,CallNumber,_,_,zip) :- !,		% 't		fast skip
	'$skipeol'(0't),				% '
	( (P=call; P=redo) ->
	  '__NB_setval__'('$debug_run',CallNumber),
	  '__NB_setval__'('$debug_jump',true)
	;
	    '$ilgl'(0't)				% '
	).
'$action'(0'+,_,_,G,M,_) :- !,			% '+		spy this
	functor(G,F,N), spy(M:(F/N)),
	'$skipeol'(0'+),			% '
	fail.
'$action'(0'-,_,_,G,M,_) :- !,			% '-		nospy this
	functor(G,F,N), nospy(M:(F/N)),
	'$skipeol'(0'-),			% '
	fail.
'$action'(0'g,_,_,_,_,_) :- !,			% 'g		ancestors
        '$scan_number'(0'g,-1,HowMany),         % '
        '$show_ancestors'(HowMany),
	fail.
'$action'(C,_,_,_,_,_) :-
	'$skipeol'(C),
	'$ilgl'(C),
	fail.

% first argument is exit, zip or fail
% second is creep, meta_creep, spy, or debugger
%'$continue_debugging'(Exit, Debugger) :-
%	writeln('$continue_debugging'(Exit, Debugger)), fail.
% that's what follows
'$continue_debugging'(_, _) :-
	false,
%	current_prolog_flag( debug, false ),
	!.
'$continue_debugging'(_, debugger) :- !.
% do not need to debug!
% go back to original sequence.
'$continue_debugging'(zip, _) :- !.
'$continue_debugging'(_, creep) :- !,
	'$creep'.
'$continue_debugging'(fail, _) :- !.
'$continue_debugging'(_, _).

% if we are in the interpreter, don't need to care about forcing a trace, do we?
'$continue_debugging_goal'(yes,G) :- !,
	'$execute_dgoal'(G).
% do not need to debug!
'$continue_debugging_goal'(_,G) :-
	'__NB_getval__'('$debug_run',Zip, fail),
        (Zip == nodebug ;  number(Zip) ; Zip == spy ), !,
	'$execute_dgoal'(G).
'$continue_debugging_goal'(_,G) :-
	'$execute_creep_dgoal'(G).

'$execute_dgoal'('$execute_nonstop'(G,M)) :-
	 '$execute_nonstop'(G,M).
'$execute_dgoal'('$execute_clause'(G, M, R, CP)) :-
	 '$execute_clause'(G, M, R, CP).

'$execute_creep_dgoal'('$execute_nonstop'(G,M)) :-
	'$creep',
	'$execute_nonstop'(G,M).
'$execute_creep_dgoal'('$execute_clause'(G, M, R, CP)) :-
	'$creep',
	'$execute_clause'(G, M, R, CP).

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
	format(user_error,'! g execute goal~n', []).

'$ilgl'(C) :-
	print_message(warning, trace_command(C)),
	print_message(help, trace_help),
	fail.

'$skipeol'(10) :- !.
'$skipeol'(_) :- get_code( debugger_input,C), '$skipeol'(C).

'$scan_number'(_, _, Nb) :-
	get_code( debugger_input,C),
	'$scan_number2'(C, Nb), !.
'$scan_number'(_, CallId, CallId).

'$scan_number2'(10, _) :- !, fail.
'$scan_number2'(0' , Nb) :- !, % '
	get_code( debugger_input,C),
	'$scan_number2'(C , Nb).
'$scan_number2'(0'	, Nb) :- !, %'
	get_code( debugger_input,C),
	'$scan_number2'(C, Nb).
'$scan_number2'(C, Nb) :-
	'$scan_number3'(C, 0, Nb).

'$scan_number3'(10,  Nb, Nb) :- !, Nb > 0.
'$scan_number3'( C, Nb0, Nb) :-
	C >= "0", C =< "9",
	NbI is Nb0*10+(C-"0"),
	get_code( debugger_input, NC),
	'$scan_number3'( NC, NbI, Nb).

'$print_deb_sterm'(G) :-
	'$get_sterm_list'(L), !,
	'$deb_get_sterm_in_g'(L,G,A),
	recorda('$debug_sub_skel',L,_),
	format(user_error,'~n~w~n~n',[A]).
'$print_deb_sterm'(_) :- '$skipeol'(94).

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
'$get_deb_depth_char_by_char'(C,_,10) :- '$skipeol'(C).

'$set_deb_depth'(D) :-
	yap_flag(debugger_print_options,L),
	'$delete_if_there'(L, max_depth(_), max_depth(D), LN),
	yap_flag(debugger_print_options,LN).

'$delete_if_there'([], _, TN, [TN]).
'$delete_if_there'([T|L], T, TN, [TN|L]).
'$delete_if_there'([Q|L], T, TN, [Q|LN]) :-
	'$delete_if_there'(L, T, TN, LN).

'$debugger_deterministic_goal'(G) :-
	yap_hacks:current_choicepoints(CPs0),
%	$cps(CPs0),
	'$debugger_skip_traces'(CPs0,CPs1),
	'$debugger_skip_loop_spy2'(CPs1,CPs2),
	'$debugger_skip_spycall'(CPs2,CPs3),
	'$debugger_skip_loop_spy2'(CPs3,[Catch|_]),
	yap_hacks:choicepoint(Catch,_,prolog,'$catch',3,'$catch'(_,'$loop_spy_event'(_,_,G,_,_),_),_).


'$cps'([CP|CPs]) :-
    yap_hacks:choicepoint(CP,A,B,C,D,E,F),
    write(A:B:C:D:E:F),nl,
    '$cps'(CPs).
'$cps'([]).


'$debugger_skip_spycall'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$spycall',4,(_;_),_), !,
	'$debugger_skip_spycall'(CPs,CPs1).
'$debugger_skip_spycall'(CPs,CPs).

'$debugger_skip_traces'([CP|CPs],CPs1) :-
	yap_hacks:choicepoint(CP,_,prolog,'$trace',4,(_;_),_), !,
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
'$ldebugger_process_meta_args'([G|BGs], M, [N|BMs], ['$spy'([M1|G1])|BG1s]) :-
    number(N),
    N >= 0,
	'$yap_strip_module'( M:G, M1, G1 ),
	functor(G1, Na, _),
	Na \= '$trace_call',
	!,
	'$ldebugger_process_meta_args'(BGs, M, BMs, BG1s).
'$ldebugger_process_meta_args'([G|BGs], M, [_|BMs], [G|BG1s]) :-
	'$ldebugger_process_meta_args'(BGs, M, BMs, BG1s).

'$trace_call'(G1,M1) :-
    '$trace_call'( call(M1:G1 )).
'$trace_call'(G1,M1, A1) :-
    '$trace_call'( call(M1:G1, A1 )).
'$trace_call'(G1,M1, A1, A2) :-
    '$trace_call'( call(M1:G1, A1, A2 )).
'$trace_call'(G1,M1, A1, A2, A3) :-
    '$trace_call'( call(M1:G1, A1, A2, A3 )).
'$trace_call'(G1,M1, A1, A2, A3, A4) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6 ) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6  )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6, A7) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6, A7 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6, A7, A8) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6, A7, A8 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6, A7, A8, A9) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6, A7, A8, A9 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10 )).
'$trace_call'(G1,M1, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) :-
    '$trace_call'( call(M1:G1, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11 )).
'$trace_call'(G1,M1, EA1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) :-
    '$trace_call'( call(M1:G1, EA1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12 )).
