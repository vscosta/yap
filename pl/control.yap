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
* File:		control.yap						 *
* Last rev:     20/08/09						 *
* mods:									 *
* comments:	control predicates available in yap			 *
*									 *
*************************************************************************/

/**
 * @file   control.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:26:35 2015
 *
 * @brief  Control Predicates
 *
 *
*/

:- system_module( '$_control', [at_halt/1,
        b_getval/2,
        break/0,
        call/2,
        call/3,
        call/4,
        call/5,
        call/6,
        call/7,
        call/8,
        call/9,
        call/10,
        call/11,
        call/12,
        call_cleanup/2,
        call_cleanup/3,
        forall/2,
        garbage_collect/0,
        garbage_collect_atoms/0,
        gc/0,
        grow_heap/1,
        grow_stack/1,
        halt/0,
        halt/1,
        if/3,
        ignore/1,
        nb_getval/2,
        nogc/0,
        notrace/1,
        once/1,
        prolog_current_frame/1,
        prolog_initialization/1,
        setup_call_catcher_cleanup/4,
        setup_call_cleanup/3,
        version/0,
        version/1], ['$run_atom_goal'/1,
        '$set_toplevel_hook'/1]).

:- use_system_module( '$_boot', ['$call'/4,
        '$disable_debugging'/0,
        '$do_live'/0,
        '$enable_debugging'/0,
        '$system_catch'/4,
        '$version'/0]).

:- use_system_module( '$_debug', ['$init_debugger'/0]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_utils', ['$getval_exception'/3]).

:- use_system_module( '$coroutining', [freeze_goal/2]).

/**


@addtogroup YAPControl
@ingroup builtins
@{

*/

/** @pred  forall(: _Cond_,: _Action_)
 *
 *
 * For all alternative bindings of  _Cond_  _Action_ can be
 * proven. The example verifies that all arithmetic statements in the list
 *  _L_ are correct. It does not say which is wrong if one proves wrong.
 *
 * ~~~~~
 * ?- forall(member(Result = Formula, [2 = 1 + 1, 4 = 2 * 2]),
 *                  Result =:= Formula).
 * ~~~~~
 *
 *
 */
forall(Cond, Action) :- \+(Cond, \+( Action) ).

/** @pred  ignore(: _Goal_)
 *
 *
 * Calls  _Goal_ as once/1, but succeeds, regardless of whether
 * `Goal` succeeded or not. Defined as:
 *
 * ~~~~~
 * ignore(Goal) :-
 *         Goal, !.
 * ignore(_).
 * ~~~~~
 *
 *
 */
ignore(Goal) :- (Goal->true;true).

/** @pred  if(? _G_,? _H_,? _I_)
 *
 * Call goal  _H_ once per each solution of goal  _H_. If goal
 *  _H_ has no solutions, call goal  _I_.
 *
 * The built-in `if/3` is similar to `->/3`, with the difference
 * that it will backtrack over the test. Consider the following
 * small data-base:
 *
 * ~~~~~
 * a(1).        b(a).          c(x).
 * a(2).        b(b).          c(y).
 * ~~~~~
 *
 * Execution of an `if/3` query will proceed as follows:
 *
 * ~~~~~
 *    ?- if(a(X),b(Y),c(Z)).
 *
 * X = 1,
 * Y = a ? ;
 *
 * X = 1,
 * Y = b ? ;
 *
 * X = 2,
 * Y = a ? ;
 *
 * X = 2,
 * Y = b ? ;
 *
 * no
 * ~~~~~
 *
 * The system will backtrack over the two solutions for `a/1` and the
 * two solutions for `b/1`, generating four solutions.
 *
 * Cuts are allowed inside the first goal  _G_, but they will only prune
 * over  _G_.
 *
 * If you want  _G_ to be deterministic you should use if-then-else, as
 * it is both more efficient and more portable.
 *
 */
if(X0,Y,Z) :-
    '$yap_strip_module'(X0,M,X),
    (
	'$$save_by'(CP),
	 '$call'(X,CP,if(X,Y,Z),M),
	 '$execute'(X),
	 '$clean_ifcp'(CP),
	 '$call'(Y,CP,if(X,Y,Z),M)
	;
	 '$call'(Z,CP,if(X,Y,Z),M)
	).

/** @pred  call( Closure,...,? Ai,...) is iso
 *
 *
 * Meta-call with extra pattern arguments, where _Closure_ is a closure
 * that is converted into a goal by appending the _Ai_ additional
 * arguments. YAP supports up to 10 extra arguments.
 *
 */
call(X,A) :- '$execute'(X,A).

call(X,A1,A2) :- '$execute'(X,A1,A2).

call(X,A1,A2,A3) :- '$execute'(X,A1,A2,A3).

call(X,A1,A2,A3,A4) :- '$execute'(X,A1,A2,A3,A4).

call(X,A1,A2,A3,A4,A5) :- '$execute'(X,A1,A2,A3,A4,A5).

call(X,A1,A2,A3,A4,A5,A6) :- '$execute'(X,A1,A2,A3,A4,A5,A6).

call(X,A1,A2,A3,A4,A5,A6,A7) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7).

call(X,A1,A2,A3,A4,A5,A6,A7,A8) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10).

call(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) :- '$execute'(X,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11).

/** @pred call_cleanup(: _Goal_, : _CleanUpGoal_)
 *
 * This is similar to call_cleanup/1 but with an additional
 *  _CleanUpGoal_ which gets called after  _Goal_ is finished.
 *
 */
call_cleanup(Goal, Cleanup) :-
'$gated_call'( false , Goal,_Catcher, Cleanup)  .

call_cleanup(Goal, Catcher, Cleanup) :-
'$gated_call'( false , Goal, Catcher, Cleanup)  .

/** @pred setup_call_cleanup(: _Setup_,: _Goal_, : _CleanUpGoal_)


Calls `(Setup, Goal)`. For each sucessful execution of _Setup_,
calling _Goal_, the cleanup handler _Cleanup_ is guaranteed to be
called exactly once.  This will happen after _Goal_ completes, either
through failure, deterministic success, commit, or an exception.
_Setup_ will contain the goals that need to be protected from
asynchronous interrupts such as the ones received from
`call_with_time_limit/2` or thread_signal/2.  In most uses, _Setup_
will perform temporary side-effects required by _Goal_ that are
finally undone by _Cleanup_.

*/

setup_call_cleanup(Setup,Goal, Cleanup) :-
	setup_call_catcher_cleanup(Setup, Goal, _Catcher, Cleanup).

setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup) :-
    '$setup_call_catcher_cleanup'(Setup),
	call_cleanup(Goal, Catcher, Cleanup).


/** @pred  call_with_args(+ Name,...,? Ai,...)


Meta-call where  _Name_ is the name of the procedure to be called and
the  _Ai_ are the arguments. The number of arguments varies between 0
and 10. New code should use `call/N` for better portability.

If  _Name_ is a complex term, then call_with_args/n behaves as
call/n:

~~~~~
call(p(X1,...,Xm), Y1,...,Yn) :- p(X1,...,Xm,Y1,...,Yn).
~~~~~


*/

%%% Some "dirty" predicates

% Only efective if yap compiled with -DDEBUG
% this predicate shows the code produced by the compiler
'$show_code' :- '$debug'(0'f). %' just make emacs happy

/** @pred  grow_heap(+ _Size_)
Increase heap size  _Size_ kilobytes.


*/
grow_heap(X) :- '$grow_heap'(X).
/** @pred  grow_stack(+ _Size_)


Increase stack size  _Size_ kilobytes


 */
grow_stack(X) :- '$grow_stack'(X).

%
% gc() expects to be called from "call". Make sure it has an
% environment to return to.
%
%garbage_collect :- save(dump), '$gc',  save(dump2).
/** @pred  garbage_collect


The goal `garbage_collect` forces a garbage collection.


*/
garbage_collect :-
	'$gc'.



/** @pred  gc


The goal `gc` enables garbage collection. The same as
`yap_flag(gc,on)`.


*/
gc :-
	yap_flag(gc,on).
/** @pred  nogc


The goal `nogc` disables garbage collection. The same as
`yap_flag(gc,off)`.


*/
nogc :-
	yap_flag(gc,off).


/** @pred  garbage_collect_atoms


The goal `garbage_collect` forces a garbage collection of the atoms
in the data-base. Currently, only atoms are recovered.


*/
garbage_collect_atoms :-
	'$atom_gc'.

'$force_environment_for_gc'.

'$good_list_of_character_codes'(V) :- var(V), !.
'$good_list_of_character_codes'([]).
'$good_list_of_character_codes'([X|L]) :-
	'$good_character_code'(X),
	'$good_list_of_character_codes'(L).

'$good_character_code'(X) :- var(X), !.
'$good_character_code'(X) :- integer(X), X > -2, X < 256.

/** @pred prolog_initialization( _G_)


Add a goal to be executed on system initialization. This is compatible
with SICStus Prolog's initialization/1.


*/
prolog_initialization(G) :- 
    must_be_callable(G),
    '$assert_init'(G).

'$assert_init'(T) :- recordz('$startup_goal',T,_), fail.
'$assert_init'(_).

/** @pred version

Write YAP's boot message.


*/
version :- '$version'.

/** @pred version(- _Message_)

Add a message to be written when yap boots or after aborting. It is not
possible to remove messages.


*/
version(V) :- var(V),  !,
	'$do_error'(instantiation_error,version(V)).
version(T) :- atom(T), !, '$assert_version'(T).
version(T) :-
	'$do_error'(type_error(atom,T),version(T)).

'$assert_version'(T) :- recordz('$version',T,_), fail.
'$assert_version'(_).

'$set_toplevel_hook'(_) :-
	recorded('$toplevel_hooks',_,R),
	erase(R),
	fail.
'$set_toplevel_hook'(H) :-
	recorda('$toplevel_hooks',H,_),
	fail.
'$set_toplevel_hook'(_).

query_to_answer(G, V, Status, LGs) :-
    gated_call(true,
		G,
		Status,
	       true),
    '$delayed_goals'(G, V, NV, LVGs, _DCP),
    lists:append(NV, LVGs, LGs).

%% @}


%% @addtogroup Global_Variables
%% @{

/** @pred nb_getval(+ _Name_,- _Value_)
 *
 *
 * The nb_getval/2 predicate is a synonym for b_getval/2, introduced for
 * compatibility and symmetry.  As most scenarios will use a particular
 * global variable either using non-backtrackable or backtrackable
 * assignment, using nb_getval/2 can be used to document that the
 * variable is used non-backtrackable.
 *
 */
nb_getval(GlobalVariable, Val) :-
	'__NB_getval__'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, nb_getval(GlobalVariable, Val)) ->
	 nb_getval(GlobalVariable, Val)
	;
	 '$do_error'(existence_error(variable, GlobalVariable),nb_getval(GlobalVariable, Val))
	).


/** @pred  b_getval(+ _Name_, - _Value_)
 *
 *
 * Get the value associated with the global variable  _Name_ and unify
 * it with  _Value_. Note that this unification may further
 * instantiate the value of the global variable. If this is undesirable
 * the normal precautions (double negation or copy_term/2) must be
 * taken. The b_getval/2 predicate generates errors if  _Name_ is not
 * an atom or the requested variable does not exist.
 *
 * Notice that for compatibility with other systems  _Name_ <em>must</em> be already associated with a term: otherwise the system will generate an error.
 *
 *
 */
b_getval(GlobalVariable, Val) :-
	'__NB_getval__'(GlobalVariable, Val, Error),
	(var(Error)
	->
	 true
	;
	 '$getval_exception'(GlobalVariable, Val, b_getval(GlobalVariable, Val)) ->
	 true
	;
	 '$do_error'(existence_error(variable, GlobalVariable),b_getval(GlobalVariable, Val))
	).


%% @}


%% @addtogroup YAPControl
%% @{

/** @pred  break


Suspends the execution of the current goal and creates a new execution
level similar to the top level, displaying the following message:

~~~~~
 [ Break (level <number>) ]
~~~~~
telling the depth of the break level just entered. To return to the
previous level just type the end-of-file character or call the
end_of_file predicate.  This predicate is especially useful during
debugging.


*/
break :-
	'$top_level_state'( reset(State) ),
	current_prolog_flag(break_level, BL ),
	NBL is BL+1,
	set_prolog_flag(break_level, NBL ),
	live,
	!,
	set_prolog_flag(break_level, BL ),
	'$top_level_state'( State ).

/* @pred break

   This is the break predicate implementation.

   YAP maintains state as a list of values. These values may be generated from
   a diversity of sources. yap_setting/4 translates these different components
   to a common interface.
	debugger state */

'$top_level_state'(reset(Status)) :-
	findall(Setting=Val, '$yap_settings'(Setting,Val,_,reset), Status).
'$top_level_state'(Status) :-
	var(Status),
	!,
	findall(Setting=Val, '$yap_settings'(Setting,Val,_,_), Status).
'$top_level_state'(reset) :-
	!,
	(
	 '$yap_setting'(_,_,_,reset),
	 fail
	;
	 true
	).
'$top_level_state'(Status) :-
	lists:member(Setting=Val, Status),
	'$yap_settings'(Setting, _, Val, _), fail.
'$top_level_state'(_).

'$yap_settings'(Setting, Exp, Imp, Reset) :-
	'$yap_setting'(Setting, GExp, GImp, GReset),
	(nonvar(Reset) -> call(GExp, Exp), call(GReset) ;
	 var(Exp) -> call(GExp, Exp) ;
	 call(GImp, Imp ) ).

'$yap_setting'(debug,
	       current_prolog_flag(debug),
	       set_prolog_flag(debug),
	       current_prolog_flag(debug,false)
	      ).
'$yap_setting'(debugger_goal_list,
	       b_getval('$spy_glist'),
	       b_setval('$spy_glist'),
	       b_setval('$spy_glist',[])
	      ).
'$yap_setting'(debugger_goal_number,
	       b_getval('$spy_gn'),
	       b_setval('$spy_gn'),
	       b_setval('$spy_gn',0)
	      ).
'$yap_setting'(debugger_enabled,
	       '$get_debugger_state'(debug),
	       '$set_debugger_state'(debug),
	       '$set_debugger_state'(debug, true)
	      ).
 '$yap_setting'(debugger_creep_mode,
	       '$get_debugger_state'(creep),
	       '$set_debugger_state'(creep),
	       '$set_debugger_state'(creep, zip)
	      ).
 '$yap_setting'(debugger_spy_mode,
	       '$get_debugger_state'(spy),
	       '$set_debugger_state'(spy),
	       '$set_debugger_state'(spy, stop)
	      ).
'$yap_setting'(trace,
	       b_getval('$trace'),
	       b_setval('trace'),
	       b_setval('$trace',0)
	      ).
'$yap_setting'(input_stream,
	       current_input,
	       set_input,
	       set_input(user_input)
	      ).
'$yap_setting'(output_stream,
	       current_output,
	       set_output,
	       set_output(user_output)
	      ).
'$yap_setting'(error_stream,
	       current_error,
	       set_error,
	       set_error(user_error)
	      ).
	
:- meta_predicate( at_halt(:) ).

/**
  * @pred at_halt( G )
  *
  * Hook predicate: _G_ must be called on exit.
  *
  * @param _G_: the hook
  *
  * @return succeeds with side-effect.
*/
at_halt(G) :-
	recorda('$halt', G, _),
	fail.
at_halt(_).

/** @pred  halt is iso

Halts Prolog, and exits to the calling application. In YAP,
halt/0 returns the exit code `0`.
*/
halt :-
	print_message(informational, halt),
	fail.
halt :-
    halt(0).

/** @pred  halt(+  _I_) is iso

Halts Prolog, and exits to 1the calling application returning the code
given by the integer  _I_.

*/
halt(_) :-
	recorded('$halt', G, _),
	catch(once(G), Error, user:'$Error'(Error)),
	fail.
halt(X) :-
	'$sync_mmapped_arrays',
	set_value('$live','$false'),
	'$halt'(X).

/**
  * @pred prolog_current_frame(-Env)
  *
  * reports a reference to the last execution environment _Env_.
  * YAP creates an enviroment when a clause contains several sub-goals.
  * Facts and simple recursion do not need an environment,
  *
  * @param Env
  *
  * @return
*/prolog_current_frame(Env) :-
	Env is '$env'.

'$run_atom_goal'(GA) :-
	'$current_module'(Module),
	atom_to_term(GA, G, _),
	catch(once(Module:G), Error,user:'$Error'(Error)).

'$add_dot_to_atom_goal'([],[0'.]) :- !. %'
'$add_dot_to_atom_goal'([0'.],[0'.]) :- !.
'$add_dot_to_atom_goal'([C|Gs0],[C|Gs]) :-
	'$add_dot_to_atom_goal'(Gs0,Gs).

%'$module_transparent' :-
    
real:text_query(Q) :-
    catch( Q,
		Error,
		system_error( Error, Q)
	).

real:placer(_,_).

/**
@}
*/
