/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
**************************************************************************
*								         *
* File:		boot.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* commen    ts:	boot file for Prolog					 *
*									 *
*************************************************************************/

/**
  @file boot2.yap
  @brief YAP bootstrap, now in full Prolog.


  @addtogroup TopLevel Top-Level and Boot Predicates

  @ingroup builtins
  @{


*/



:- meta_predicate(log_event(+,:)).

:- dynamic prolog:'$user_defined_flag'/4.

:- multifile prolog:debug_action_hook/1.

:- multifile prolog:'$system_predicate'/2.

:- '$opdec'(1150,fx,(mode),prolog).

:- dynamic 'extensions_to_present_answer'/1.
:- 	['arrays.yap'].

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

/** @pred prolog:goal_expansion( :G,+ M,- NG)
    @pred user:goalexpansion(+ G,+ M,- NG)

The goal_expansion/3 hook  is an user-defined
procedure that is called after term expansion when compiling or
asserting goals for each sub-goal in a clause. The first argument is
bound to the goal and the second to the module under which the goal
 _G_ will execute. If goal_expansion/3 succeeds the new
sub-goal  _NG_ will replace  _G_ and will be processed in the same
 way. If goal_expansion/3 fails the system will use the default
expandion mechanism.

This hook is called:
- at compilation time;
- when running a query in the top-level

Older versions of YAP would call this procedure  at every meta-call.


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

:- use_module('pathconf.yap').



:- use_module('attributes.yap').
:- use_module('corout.yap').
:- use_module('dialect.yap').
:- use_module('dbload.yap').
:- use_module('ypp.yap').
:- use_module('../os/chartypes.yap').
:- use_module('../os/edio.yap').


yap_hacks:cut_by(CP) :- '$$cut_by'(CP).

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
% moved this to init_gc in sgc.c to separate the alpha
%
% :- yap_flag(gc,on).
%
% :- yap_flag(gc_trace,verbose`

:- multifile
	prolog:comment_hook/3.

:- source.

:- module(user).

:-  current_prolog_flag(android,true)->use_module(user:'android.yap') ; true.


/** @pred  term_expansion( _T_,- _X_)
    user:term_expansion( _T_,- _X_)


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

:- multifile system:swi_predicate_table/4.

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

:- module(user).


:- multifile user:exception/3.

:- dynamic user:exception/3.


:- set_prolog_flag(unknown,error).

%% @}

