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
* File:		utilities for messing around in YAP internals.		 *
* comments:	error messages for YAP					 *
*									 *
* Last rev:     $Date: 2008-03-24 23:48:47 $,$Author: vsc $						 *
*									 *
*									 *
*************************************************************************/

%% @file pl/hacks.yap
%% @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
%%  @date   Thu Oct 19 12:02:56 2017
%%
%% @brief Access and Manipulation of YAD's internals

:- system_module('$yap_hacks',
		 [],
		 [ctrace/1,
		  fully_strip_module/3,
		  scratch_goal/4
	 ]).

/**
  *
  * @defgroup Hacks Access to YAP internal data-structures
  * @ingroup Builtins
  * @{
  * The _hacks_ predicate collection predicaates
  * provides a 
  limoted introspection t the eecution stack and of error representation. Most of this functionnality requires to first load the module `library(hacks)`
*/


/** @pred hacks:context_variables(-NamedVariables)
  Access variable names.

  Unify NamedVariables with a list of terms _Name_=_V_
  giving the names of the variables occurring in the last term read.
  Notice that variable names option must have been on.
*/


'$scratch_goal'(N,0,Mod,Mod:N) :-
	!.
'$scratch_goal'(N,A,Mod,NG) :-
	list_of_qmarks(A,L),
	G=..[N|L],
	(
	  beautify_goal(G,Mod,[NG],[])
	;
	  G = NG
	),
	!.

list_of_qmarks(0,[]) :- !.
list_of_qmarks(I,[?|L]) :-
	I1 is I-1,
	list_of_qmarks(I1,L).

fully_strip_module( T, M, TF) :-
    '$yap_strip_module'( T, M, TF).


yap_hacks:export_beautify(A,NA) :-
    beautify(A,NA).
    
%%
% @pred beautify(Goal, ModuleProcessGoal)
%
% This helper routine should be called with a Prolog
% goal or clause body It will push the modules inside
% the Prolog connectives so that the goal becomes a little
% more easier to understand.
%
beautify(Goal, NicerGoal) :- 
    current_source_module(M,M),
    beautify(Goal, M, NicerGoal).

beautify((A,B),M,(CA,CB)) :-
    !,
    beautify(A,M,CA),
    beautify(B,M,CB).
beautify((A;B),M,(CA;CB)) :-
    !,
    beautify(A,M,CA),
    beautify(B,M,CB).
beautify((A->B),M,(CA->CB)) :-
    !,
    beautify(A,M,CA),
    beautify(B,M,CB).
beautify((A *->B),M,(CA *->CB)) :-
    !,
    beautify(A,M,CA),
    beautify(B,M,CB).
beautify(M:A,_,CA) :-
    !,
    beautify(A,M,CA).
beautify(A,prolog, NA) :-
    beautify_goal(A,prolog,[NA],[]),
    !.
beautify(A,prolog,A) :-
    !.
beautify(A,M,CA) :-
    current_source_module(M,M),
    !,
    beautify(A,CA).
beautify(A,M,M:A).

beautify_goal('$yes_no'(G,_Query), (?-G)) -->
	!,
	{ Call =.. [(?), G] },
	[Call].
beautify_goal('$do_yes_no'(G,Mod), prolog) -->
	[Mod:G].
beautify_goal(query(G,VarList), prolog) -->
	[query(G,VarList)].
beautify_goal('$enter_top_level', prolog) -->
	['TopLevel'].
% The user should never know these exist.
beautify_goal('$csult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_goal('$use_module'(Files,Mod,Is),prolog) -->
	[use_module(Mod,Files,Is)].
beautify_goal('$continue_with_command'(reconsult,V,P,G,Source),prolog) -->
	['Assert'(G,V,P,Source)].
beautify_goal('$continue_with_command'(consult,V,P,G,Source),prolog) -->
	['Assert'(G,V,P,Source)].
beautify_goal('$continue_with_command'(top,V,P,G,_),prolog) -->
	['Query'(G,V,P)].
beautify_goal('$continue_with_command'(Command,V,P,G,Source),prolog) -->
	['TopLevel'(Command,G,V,P,Source)].
beautify_goal('$system_catch'(G,Mod,Exc,Handler),prolog) -->
	[catch(Mod:G, Exc, Handler)].
beautify_goal('$catch'(G,Exc,Handler),prolog) -->
	[catch(G, Exc, Handler)].
beautify_goal('$execute_command'(Query,M,V,P,Option,Source),prolog) -->
	[toplevel_query(M:Query, V, P, Option, Source)].
beautify_goal('$process_directive'(Gs,_Mode,_VL),prolog) -->
	[(:- Gs)].
beautify_goal('$loop'(Stream,Option),prolog) -->
	[execute_load_file(Stream, consult=Option)].
beautify_goal('$load_files'(Files,Opts,?),prolog) -->
	[load_files(Files,Opts)].
beautify_goal('$load_files'(_,_,Name),prolog) -->
	[Name].
beautify_goal('$reconsult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_goal('$undefp'(Mod:G),prolog) -->
	['CallUndefined'(Mod:G)].
beautify_goal('$undefp'(?),prolog) -->
	['CallUndefined'(?:?)].
beautify_goal(repeat,prolog) -->
	[repeat].
beautify_goal('$recorded_with_key'(A,B,C),prolog) -->
	[recorded(A,B,C)].
beautify_goal('$findall_with_common_vars'(Templ,Gen,Answ),prolog) -->
	[findall(Templ,Gen,Answ)].
beautify_goal('$bagof'(Templ,Gen,Answ),prolog) -->
	[bagof(Templ,Gen,Answ)].
beautify_goal('$setof'(Templ,Gen,Answ),prolog) -->
	[setof(Templ,Gen,Answ)].
beautify_goal('$findall'(T,G,S,A),prolog) -->
	[findall(T,G,S,A)].
beautify_goal('$listing'(G,M,_Stream),prolog) -->
	[listing(M:G)].
beautify_goal('$call'(G,_CP,?,M),prolog) -->
	[call(M:G)].
beautify_goal('$call'(_G,_CP,G0,M),prolog) -->
	[call(M:G0)].
beautify_goal('$current_predicate'(Na,M,S,_),prolog) -->
	[current_predicate(Na,M:S)].
beautify_goal('$list_clauses'(Stream,M,Pred),prolog) -->
	[listing(Stream,M:Pred)].

/**
 * @pred ctrace(Goal)
 *
 * This predicate is only available if the YAP
 * compile option was set. It generates a
 * step-by-step trace of the execution of _Goal_
 *
 */
:- meta_predicate(ctrace(0)).

ctrace(G) :-
    gated_call(start_low_level_trace,
	       G,
	       _,
	       stop_low_level_trace).

/**
 * @pred context_variables(+VarAndNames<)
 *
 * makes available a list with the variable names of the last interaction.
 *
 */
yap_hacks:context_variables(Vs) :-
    b_getval(name_variables, Vs).

%% @}

