/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
**	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		boot.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	initializing the full prolog system			 *
*									 *
*************************************************************************/

%% @file bo0t.yap

%% @short Prolog Bootstrap and Initialization

%% @section Bootstrap Support Bootstrap Support

'$undefp0'(M:call(G) ) :-
	!,
	'$execute_nonstop'(G, M).
'$undefp0'(_:private(_L) ) :-
	!.
'$undefp0'(_:print_message(L,E) ) :-
	!,
	(L == informational
	->
         true
         ;
	 E = error(_,exception(Error))
	 ->
	    format( user_error, '~w in bootstrap: exception is:~n',[L]) ,
	    '$print_exception'(Error)
	;
	    format( user_error, '~w in bootstrap, namely ~w~n',[L,E])

	).
'$undefp0'(M: G) :-
	stream_property( loop_stream, file_name(F)),
	stream_property( loop_stream, line_number(L)),
	format(user_error,'~a:~d error undefined: call to ~w~n',[F,L,M:G]),
	!,
	fail.

/**

@}
@{

@addtogroup YAPControl
@ingroup Builtins
@{

*/


system_module(_,_,_).

use_system_module(_,_).


:- system_module( '$_init', [!/0,
        ':-'/1,
        '?-'/1,
        []/0,
        extensions_to_present_answer/1,
        fail/0,
        false/0,
        goal_expansion/2,
        goal_expansion/3,
        otherwise/0,
        term_expansion/2,
        version/2,
	    '$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5], [
        '$system_module'/1]).

:- use_system_module( '$_boot', ['$cut_by'/1]).

%:- start_low_level_trace.

% This is the YAP init file
% should be consulted first step after booting

% just create a choice-point
% the 6th argument marks the time-stamp.
'$do_log_upd_clause'(_,_,_,_,_,_).
'$do_log_upd_clause'(A,B,C,D,E,_) :-
	'$continue_log_update_clause'(A,B,C,D,E).
'$do_log_upd_clause'(_,_,_,_,_,_).


'$do_log_upd_clause_erase'(_,_,_,_,_,_).
'$do_log_upd_clause_erase'(A,B,C,D,E,_) :-
	'$continue_log_update_clause_erase'(A,B,C,D,E).
'$do_log_upd_clause_erase'(_,_,_,_,_,_).

'$do_log_upd_clause0'(_,_,_,_,_,_).
'$do_log_upd_clause0'(A,B,C,D,_,_) :-
	'$continue_log_update_clause'(A,B,C,D).
'$do_log_upd_clause0'(_,_,_,_,_,_).


'$do_static_clause'(_,_,_,_,_).
'$do_static_clause'(A,B,C,D,E) :-
	'$continue_static_clause'(A,B,C,D,E).
'$do_static_clause'(_,_,_,_,_).

'$command'(C,VL,Pos,Con) :-
    prolog_flag(strict_iso, true), !,      /* strict_iso on */
    '$yap_strip_module'(C, EM, EG),
   '$execute_command'(EM,EG,VL,Pos,Con,_Source).
'$command'(C,VL,Pos,Con) :-
    ( (Con = top ; var(C) ; C = [_|_])  ->
      '$yap_strip_module'(C, EM, EG),
      '$execute_command'(EG,EM,VL,Pos,Con,C), ! ;
      % do term expansion
      expand_term(C, EC),
      '$yap_strip_module'(EC, EM, EG),
      % execute a list of commands
      '$execute_commands'(EG,EM,VL,Pos,Con,_Source),
      % succeed only if the *original* was at end of file.
      C == end_of_file
    ).

:- c_compile('op.yap').

:- c_compile('predtypes.yap').

:- c_compile('arith.yap').
%:- stop_low_level_trace.

:- compile_expressions.

:- c_compile('top.yap').

:- c_compile('directives.yap').

:- c_compile('init.yap').


:- c_compile('imports.yap').
:- c_compile('bootutils.yap').
:- c_compile('bootlists.yap').
:- c_compile('preddecls.yap').
:- c_compile('preddyns.yap').
:- c_compile('builtins.yap').
:- c_compile('newmod.yap').
:- c_compile('meta.yap').
:- c_compile('metadecls.yap').

:- c_compile('os.yap').
:- c_compile('errors.yap').

initialize_prolog :-
	'$init_prolog'.

:- set_prolog_flag(verbose, silent).
%:- set_prolog_flag(verbose_file_search, true ).
%:- yap_flag(write_strings,on).
:- c_compile( 'preds.yap' ).
:- c_compile( 'modules.yap' ).
:- c_compile( 'grammar.yap' ).
:- c_compile('atoms.yap').

:- c_compile('control.yap').

:- c_compile('absf.yap').

:- c_compile('lf.yap').
:- c_compile('consult.yap').

:- compile('error.yap').

:- ['utils.yap',
    'flags.yap'].


:- [
    % lists is often used.
   	 '../os/yio.yap',
	 'debug.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'ground.yap',
	 'listing.yap',
    'arithpreds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 % must follow grammar
	 'eval.yap',
	 'signals.yap',
	 'profile.yap',
	 'callcount.yap',
	 'load_foreign.yap',
%	 'save.yap',
	 'setof.yap',
	 'sort.yap',
	 'statistics.yap',
	 'strict_iso.yap',
	 'tabling.yap',
	 'eam.yap',
	 'yapor.yap',
     'qly.yap',
     'udi.yap'].
%:- Stop_low_level_trace.


:- meta_predicate(log_event(+,:)).

:- dynamic prolog:'$user_defined_flag'/4.

:- multifile prolog:debug_action_hook/1.

:- multifile prolog:'$system_predicate'/2.

:-	 ['protect.yap'].


:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['arrays.yap'].
%:- start_low_level_trace.

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

/** @pred  _CurrentModule_:goal_expansion(+ _G_,+ _M_,- _NG_), user:goal_expansion(+ _G_,+ _M_,- _NG_)


YAP now supports goal_expansion/3. This is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If goal_expansion/3 succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
 way. If goal_expansion/3 fails the system will use the defaultyap+flrules.


*/
:- multifile user:goal_expansion/3.

:- dynamic user:goal_expansion/3.

:- multifile user:goal_expansion/2.

:- dynamic user:goal_expansion/2.

:- multifile system:goal_expansion/2.

:- dynamic system:goal_expansion/2.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- use_module('messages.yap').


:- 	['undefined.yap'].

:- use_module('hacks.yap').


:- use_module('attributes.yap').

:- use_module('threads.yap').

:- use_module('corout.yap').

:- use_module('dialect.yap').
:- use_module('dbload.yap').
:- use_module('ypp.yap').
:- use_module('../os/chartypes.yap').
:- ensure_loaded('../os/edio.yap').

:- ensure_loaded('spy.yap').

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:-	set_prolog_flag(generate_debug_info,true).

%
% cleanup ensure loaded and recover some data-base space.
%
%:- ( recorded('$lf_loaded',_,R), erase(R), fail ; true ).
%:- ( recorded('$module',_,R), erase(R), fail ; true ).

:- set_value('$user_module',user), '$protect'.

:- style_check([+discontiguous,+multiple,+single_var]).

%
% moved this to init_gc in gc.c to separate the alpha
%
% :- yap_flag(gc,on).

% :- yap_flag(gc_trace,verbose).

:- multifile
	prolog:comment_hook/3.

:- source.

:- module(user).


/** @pred  _CurrentModule_:term_expansion( _T_,- _X_),  user:term_expansion( _T_,- _X_)


This user-defined predicate is called by `expand_term/3` to
preprocess all terms read when consulting a file. If it succeeds:

+
If  _X_ is of the form `:- G` or `?- G`, it is processed as
a directive.
+
If  _X_ is of the form `$source_location`( _File_, _Line_): _Clause_` it is processed as if from `File` and line `Line`.

+
If  _X_ is a list, all terms of the list are asserted or processed
as directives.
+ The term  _X_ is asserted instead of  _T_.



*/
:- multifile term_expansion/2.

:- dynamic term_expansion/2.

:- multifile system:term_expansion/2.

:- dynamic system:term_expansion/2.

:- multifile swi:swi_predicate_table/4.

/** @pred  user:message_hook(+ _Term_, + _Kind_, + _Lines_)


Hook predicate that may be define in the module `user` to intercept
messages from print_message/2.  _Term_ and  _Kind_ are the
same as passed to print_message/2.  _Lines_ is a list of
format statements as described with print_message_lines/3.

This predicate should be defined dynamic and multifile to allow other
modules defining clauses for it too.


*/
:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

/** @pred  exception(+ _Exception_, + _Context_, - _Action_)


Dynamic predicate, normally not defined. Called by the Prolog system on run-time exceptions that can be repaired `just-in-time`. The values for  _Exception_ are described below. See also catch/3 and throw/1.
If this hook preodicate succeeds it must instantiate the  _Action_ argument to the atom `fail` to make the operation fail silently, `retry` to tell Prolog to retry the operation or `error` to make the system generate an exception. The action `retry` only makes sense if this hook modified the environment such that the operation can now succeed without error.

+ `undefined_predicate`
 _Context_ is instantiated to a predicate-indicator ( _Module:Name/Arity_). If the predicate fails Prolog will generate an existence_error exception. The hook is intended to implement alternatives to the SWI built-in autoloader, such as autoloading code from a database. Do not use this hook to suppress existence errors on predicates. See also `unknown`.
+ `undefined_global_variable`
 _Context_ is instantiated to the name of the missing global variable. The hook must call nb_setval/2 or b_setval/2 before returning with the action retry.

*/

:- multifile user:exception/3.

:- dynamic user:exception/3.

:- ensure_loaded('../pl/pathconf.yap').

:- yap_flag(user:unknown,error).

%% @}
