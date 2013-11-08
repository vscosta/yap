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

:- [	 'utils.yap',
	 'control.yap',
	 'arith.yap',
	 'directives.yap',
	 'flags.yap'].

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


'$system_module'('$attributes').
'$system_module'('$coroutining').
'$system_module'('$hacks').
'$system_module'('$history').
'$system_module'('$messages').
'$system_module'('$predopts').
'$system_module'('$swi').
'$system_module'('$win_menu').


yap_hacks:cut_by(CP) :- '$$cut_by'(CP).

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:- multifile user:library_directory/1.

:- dynamic user:library_directory/1.

:- multifile user:commons_directory/1.

:- dynamic user:commons_directory/1.

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

:- multifile prolog:message/3.

:- dynamic prolog:message/3.

:- multifile
	prolog:comment_hook/3.

:- module(user).

:- multifile goal_expansion/3.

:- dynamic goal_expansion/3.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- multifile system:goal_expansion/2.

:- dynamic system:goal_expansion/2.

:- multifile user:prolog_file_type/2.

:- dynamic user:prolog_file_type/2.

user:prolog_file_type(yap, prolog).
user:prolog_file_type(pl, prolog).
user:prolog_file_type(prolog, prolog).
user:prolog_file_type(A, prolog) :-
	current_prolog_flag(associate, A),
	A \== prolog,
	A \==pl,
	A \== yap.
%user:prolog_file_type(qlf, prolog).
%user:prolog_file_type(qlf, qlf).
user:prolog_file_type(A, executable) :-
	current_prolog_flag(shared_object_extension, A).


:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- multifile term_expansion/2.

:- dynamic term_expansion/2.

:- multifile system:term_expansion/2.

:- dynamic system:term_expansion/2.

:- multifile file_search_path/2.

:- dynamic file_search_path/2.

:- multifile generate_message_hook/3.

:- dynamic generate_message_hook/3.

:- multifile swi:swi_predicate_table/4.

:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

:- multifile user:portray_message/2.

:- dynamic user:portray_message/2.

:- multifile user:exception/3.

:- dynamic user:exception/3.

file_search_path(library, Dir) :-
	library_directory(Dir).
file_search_path(commons, Dir) :-
	commons_directory(Dir0).
file_search_path(swi, Home) :-
	current_prolog_flag(home, Home).
file_search_path(yap, Home) :-
        current_prolog_flag(home, Home).
file_search_path(system, Dir) :-
	prolog_flag(host_type, Dir).
file_search_path(foreign, yap('lib/Yap')).
file_search_path(path, C) :-
    (   getenv('PATH', A),
	(   current_prolog_flag(windows, true)
	->  atomic_list_concat(B, ;, A)
	;   atomic_list_concat(B, :, A)
	),
	lists:member(C, B)
    ).

:- yap_flag(unknown,error). 

:- stream_property(user_input, tty(true)) -> set_prolog_flag(readline, true) ; true.

