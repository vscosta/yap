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

'$$!'(CP) :- '$cut_by'(CP).

[] :- true.

:- set_value('$doindex',true).

% force having indexing code for throw.
:- '$handle_throw'(_,_,_), !.

:- bootstrap('errors.yap').
:- bootstrap('consult.yap').

:- [	 'utils.yap',
	 'control.yap',
	 'arith.yap',
	 'directives.yap',
	 'flags.yap'].

:- compile_expressions.

lists:append([], L, L).
lists:append([H|T], L, [H|R]) :-
	lists:append(T, L, R).


:- [
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
	 'sockets.yap',
	 'sort.yap',
	 'setof.yap',
	 'statistics.yap',
	 'strict_iso.yap',
	 'tabling.yap',
	 'threads.yap',
	 'eam.yap',
	 'chtypes.yap',
	 'yapor.yap',
         'udi.yap'].

:- dynamic prolog:'$user_defined_flag'/4.

:- dynamic prolog:'$parent_module'/2.

:- multifile prolog:debug_action_hook/1.

:- source.

%   memberchk(+Element, +Set)
%   means the same thing, but may only be used to test whether a known
%   Element occurs in a known Set.  In return for this limited use, it
%   is more efficient when it is applicable.

lists:memberchk(X,[X|_]) :- !.
lists:memberchk(X,[_|L]) :-
	lists:memberchk(X,L).

%   member(?Element, ?Set)
%   is true when Set is a list, and Element occurs in it.  It may be used
%   to test for an element or to enumerate all the elements by backtracking.
%   Indeed, it may be used to generate the Set!

lists:member(X,[X|_]).
lists:member(X,[_|L]) :-
	lists:member(X,L).

:- no_source.


:-	 ['protect.yap'].

version(yap,[6,0]).

system_mode(verbose,on)  :- set_value('$verbose',on).
system_mode(verbose,off) :- set_value('$verbose',off).

:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['corout.yap',
	 'arrays.yap'].

:- use_module('messages.yap').
:- use_module('hacks.yap').

'$system_module'('$messages').
'$system_module'('$hacks').

yap_hacks:cut_by(CP) :- '$$cut_by'(CP).

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:- default_sequential(off).

:- multifile user:library_directory/1.

:- dynamic user:library_directory/1.

:- multifile user:commons_directory/1.

:- dynamic user:commons_directory/1.

:- recorda('$dialect',yap,_).

%
% cleanup ensure loaded and recover some data-base space.
%
:- ( recorded('$loaded','$loaded'(_,_,_),R), erase(R), fail ; true ).

:- set_value('$user_module',user), '$protect'.

:- style_check([]).

%
% moved this to init_gc in gc.c to separate the alpha
%
% :- yap_flag(gc,on).

% :- yap_flag(gc_trace,verbose).

:- system_mode(verbose,on).

:- multifile prolog:message/3.

:- dynamic prolog:message/3.

:- module(user).

:- multifile goal_expansion/3.

:- dynamic goal_expansion/3.

:- multifile goal_expansion/2.

:- dynamic goal_expansion/2.

:- multifile term_expansion/2.

:- dynamic term_expansion/2.

:- multifile file_search_path/2.

:- dynamic file_search_path/2.

:- multifile generate_message_hook/3.

:- dynamic generate_message_hook/3.

:- multifile swi:swi_predicate_table/4.

:- multifile user:message_hook/3.

:- dynamic user:message_hook/3.

:- multifile user:portray_message/2.

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

%:- yap_flag(unknown,error). 

