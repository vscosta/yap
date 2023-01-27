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

%% @file boot.yap

%% @short Prolog Bootstrap and Initialization

%% @section Bootstrap Support Bootstrap Support

'$undefp0'(MG) :-
    '$yap_strip_module'(MG,M,G),
    MG \= M:G,
    !,
    '$undefp0'(M:G). 
'$undefp0'(_:private(_L) ) :-
	!.
'$undefp0'(_:print_message(L,_E) ) :-
    var(L),!.
'$undefp0'(_:print_message(informational,_E) ) :-
    !.
'$undefp0'(_:system_module(_,_,_)) :-
    !.
'$undefp0'(_:private( _ )) :-
    !.
'$undefp0'(_:print_message(L,E )) :-
    format( user_error,
	    '~w in bootstrap, namely ~w~n',[L,E]).
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


use_system_module(_,_).
system_module_(_,_,_).


:- system_module_( '$_init', [!/0,
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

:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(verbose_load, false).

:- c_compile('op.yap').


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

'$command'((:- Command),VL,Pos, Option) :-
    '$if_directive'(Command),
    !,
    strip_module(Command,M,C),
    '$if_directive'(C, M, VL, Pos, Option),
    fail.
'$command'(C,VL,Pos,Con) :-
    prolog_flag(strict_iso, true), !,      /* strict_iso on */
    '$yap_strip_module'(C, EM, EG),
   '$execute_command'(EG,EM,VL,Pos,Con,_Source).
'$command'(C,VL,Pos,Con) :-
    '$current_module'(EM,EM),
    expand_term(C, Source, EC),
    '$do_command'(EC,EM,VL,Pos,Con,C,Source).

'$do_command'(EC,_,_VL,_Pos,_Con,C,_) :-
    % succeed only if the *original* was at end of file.
    C == end_of_file,
    EC \= [_|_],
    !.
'$do_command'(EC,EM,VL,Pos,top,_,Source) :-
    !,
    '$execute_command'(EC,EM,VL,Pos,top,Source).
'$do_command'(EC,EM,VL,Pos,Con,_,Source) :-
    '$execute_commands'(EC,EM,VL,Pos,Con,Source).

:- c_compile('predtypes.yap').

:- c_compile('arith.yap').
%:- stop_low_level_trace.

:- compile_expressions.

:- c_compile('top.yap').

:- c_compile('directives.yap').

:- c_compile('init.yap').

:- c_compile('control.yap').

:- c_compile('imports.yap').
:- c_compile('bootutils.yap').
:- c_compile('bootlists.yap').
:- c_compile('preddyns.yap').
:- c_compile('preddecls.yap').
:- c_compile('builtins.yap').
:- c_compile('newmod.yap').

:- c_compile('meta.yap').

:- c_compile('../os/os.yap').
:- c_compile('errors.yap').

initialize_prolog :-
	init_prolog.


%:- set_prolog_flag(verbose_file_search, true ).
%:- yap_flag(write_strings,on).
:- c_compile( 'preds.yap' ).
:- c_compile( 'modules.yap' ).
:- c_compile( 'grammar.yap' ).
:- c_compile('atoms.yap').

:- c_compile('absf.yap').

:- c_compile('lf.yap').
:- c_compile('consult.yap').

:- compile('error.yap').



:- ['utils.yap',
    'flags.yap'].

%:- start_low_level_trace.

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

:- multifile prolog:'$system_predicate'/2.

:-	 ['protect.yap'].


:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['arrays.yap'].
%:- start_low_level_trace.

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

/** @pred  user"goal_expansion(+ _G_,+ _M_,- _NG_)

also available: Module:goal_expansion(+ _G_,+ _M_,- _NG_)


YAP supports goal_expansion/3. This is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If goal_expansion/3 succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
 way. If goal_expansion/3 fails the system will use the defaultyap+flrules.


*/
:- multifile prolog:print_message/2.

:- multifile user:goal_expansion/3.

:- dynamic user:goal_expansion/3.

:- multifile user:goal_expansion/2.

:- dynamic user:goal_expansion/2.

:- multifile system:goal_expansion/2.

:- dynamic system:goal_expansion/2.

:- multifile system:goal_expansion/3.

:- dynamic system:goal_expansion/3.

:- use_module('messages.yap').


:- ['undefined.yap'].

export_from_prolog([]).
export_from_prolog([P|Ps]) :-
    mksys(P),
    export_from_prolog(Ps).

mksys(F/N) :-
    '$new_system_predicate'(F,N,prolog).
mksys(F/N) :-
    N2 is N+2,
    '$new_system_predicate'(F,N2,prolog).
mksys(op(A,B,C)) :-
    op(A,B,prolog:C).

% system_module(M,PrologExports,MExports) :-
%    export_from_prolog(PrologExports),
%    '$declare_system_module'(_, prolog, M, MExports, []).

:- use_module('hacks.yap').

%:- start_low_level_trace.
:- use_module('attributes.yap').
%:- stop_low_level_trace.
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


/** @pred user:term_expansion( _T_,- _X_)

also available  _CurrentModule_:term_expansion( _T_,- _X_),

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


:- multifile swi:swi_predicate_table/4.

:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.
/** @pred  exception(+ _Exception_, + _Context_, - _Action_)


Dynamic predicate, normally not defined. Callreded by the Prolog system on run-time exceptions that can be repaired `just-in-time`. The values for  _Exception_ are described below. See also catch/3 and throw/1.
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

