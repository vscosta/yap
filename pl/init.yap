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
* File:		init.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	initializing the full prolog system			 *
*									 *
*************************************************************************/

:- system_module( '$_init', [!/0,
        (:-)/1,
        (?-)/1,
        []/0,
        extensions_to_present_answer/1,
        fail/0,
        false/0,
        goal_expansion/2,
        goal_expansion/3,
        otherwise/0,
        prolog_booting/0,
        term_expansion/2,
        version/2,
	'$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5], [
        '$system_module'/1]).

:- use_system_module( '$_boot', ['$cut_by'/1]).

'prolog_booting'.

% This is yap's init file
% should be consulted first step after booting

% These are pseudo declarations
% so that the user will get a redefining system predicate
fail :- fail.

false :- fail.

otherwise.

!.

(:- G) :- '$execute'(G), !.

(?- G) :- '$execute'(G).

'$$!'(CP) :- '$cut_by'(CP).

[] :- true.

:- set_value('$doindex',true).

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

:- '$handle_throw'(_,_,_), !.

:- bootstrap('errors.yap').
:- bootstrap('lists.yap').
:- bootstrap('consult.yap').
:- bootstrap('preddecls.yap').


:- bootstrap('atoms.yap').
:- bootstrap('os.yap').
:- bootstrap('absf.yap').

:- [	 'utils.yap',
	 'control.yap',
	 'arith.yap',
	 'directives.yap',
	 'flags.yap'
   ].

:- compile_expressions.

:- [
    % lists is often used.
   	 'yio.yap',
	 'debug.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'grammar.yap',
	 'ground.yap',
	 'listing.yap',
	 'preds.yap',
         'arithpreds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 'modules.yap',
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
	 'threads.yap',
	 'eam.yap',
	 'chtypes.yap',
	 'yapor.yap',
         'qly.yap',
         'udi.yap'].

:- meta_predicate(log_event(+,:)).

:- dynamic prolog:'$user_defined_flag'/4.

:- dynamic prolog:'$parent_module'/2.

:- multifile prolog:debug_action_hook/1.

:-	 ['protect.yap'].

:- source.

version(yap,[6,3]).

:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['arrays.yap'].

:- use_module('messages.yap').
:- use_module('hacks.yap').
:- use_module('attributes.yap').
:- use_module('corout.yap').
:- use_module('dialect.yap').
:- use_module('history.pl').
:- use_module('dbload.yap').
:- use_module('swi.yap').
:- use_module('../swi/library/predopts.pl').
:- use_module('../swi/library/menu.pl').
:- use_module('../library/ypp.yap').



yap_hacks:cut_by(CP) :- '$$cut_by'(CP).

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:-	'$swi_set_prolog_flag'(generate_debug_info,true).


:- recorda('$dialect',yap,_).

%
% cleanup ensure loaded and recover some data-base space.
%
:- ( recorded('$lf_loaded',_,R), erase(R), fail ; true ).
:- ( recorded('$module',_,R), erase(R), fail ; true ).

:- set_value('$user_module',user), '$protect'.

:- style_check([-discontiguous,-multiple,-single_var]).

%
% moved this to init_gc in gc.c to separate the alpha
%
% :- yap_flag(gc,on).

% :- yap_flag(gc_trace,verbose).

:- multifile
	prolog:comment_hook/3.

:- module(user).

:- multifile goal_expansion/3.

:- dynamic goal_expansion/3.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- multifile system:goal_expansion/2.

:- dynamic system:goal_expansion/2.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- multifile term_expansion/2.

:- dynamic term_expansion/2.

:- multifile system:term_expansion/2.

:- dynamic system:term_expansion/2.

:- multifile swi:swi_predicate_table/4.

:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

:- multifile user:exception/3.

:- dynamic user:exception/3.

:- yap_flag(user:unknown,error). 

:- stream_property(user_input, tty(true)) -> set_prolog_flag(readline, true) ; true.



