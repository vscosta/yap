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
otherwise.
fail :- fail.
false :- fail.
!.
(:- G) :- '$execute'(G), !.
'$$!'(CP) :- '$cut_by'(CP).
[] :- true.
','(A,B) :-  '$current_module'(Module), '$meta_call'((A,B),Module).
';'(A,B) :-  '$current_module'(Module), '$meta_call'((A;B),Module).
'|'(A,B) :-  '$current_module'(Module), '$meta_call'((A;B),Module).
'->'(A,B) :- '$current_module'(Module), '$meta_call'((A->B),Module).
\+(G) :-     '$current_module'(Module), '$meta_call'(\+(G),Module).
not(G) :-    '$current_module'(Module), '$meta_call'(not(G),Module).


:- set_value('$doindex',true).

% force having indexing code for throw.
:- '$handle_throw'(_,_,_), !.

:- 	['errors.yap',
	 'utils.yap',
	 'arith.yap',
	 'directives.yap'].

:- compile_expressions.

:- [	 'yio.yap',
	 'debug.yap',
	 'consult.yap',
	 'checker.yap',
	 'depth_bound.yap',
	 'grammar.yap',
	 'ground.yap',
	 'listing.yap',
	 'preds.yap',
	 % modules must be after preds, otherwise we will have trouble
	 % with meta-predicate expansion being invoked
	 'modules.yap',
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
	 'yapor.yap'].

:-	 ['protect.yap'].

version(yap,[4,1]).

system_mode(verbose,on)  :- set_value('$verbose',on).
system_mode(verbose,off) :- set_value('$verbose',off).

:- op(1150,fx,(mode)).

:- dynamic 'extensions_to_present_answer'/1.

:- 	['corout.yap',
	 'arrays.yap'].

:- '$change_type_of_char'(36,7). % Make $ a symbol character

:- default_sequential(off).

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

:- initialization((system_mode(verbose,on),module(user))).

:- module(user).

:- multifile goal_expansion/3.

:- dynamic_predicate(goal_expansion/3, logical).

:- multifile term_expansion/2.

:- dynamic_predicate(term_expansion/2, logical).

:- multifile file_search_path/2.

:- dynamic_predicate(file_search_path/2, logical).

file_search_path(library, Dir) :-
     library_directory(Dir).
file_search_path(system, Dir) :-
     prolog_flag(host_type, Dir).

:- multifile library_directory/1.

:- dynamic_predicate(library_directory/1, logical).

library_directory(D) :-
	getenv('YAPSHAREDIR', D).

:- get_value(system_library_directory,D), assert(library_directory(D)).
