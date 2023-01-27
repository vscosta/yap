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

/**
  * @file   hacks.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:02:56 2017
  *
  * @brief  Low-level access
  *
  * @defgroup Hacks Low-level access
  * @ingroup builtins
  *
*/

%% @file pl/hacks.yap

:- system_module('$hacks',
		 [],
		 [ctrace/1,
		  display_stack_info/4,
		  display_stack_info/6,
		  display_pc/4,
		  fully_strip_module/3,
		  code_location/3
		 ]).

/** hacks:context_variables(-NamedVariables)
  Access variable names.

  Unify NamedVariables with a list of terms _Name_=_V_
  giving the names of the variables occurring in the last term read.
  Notice that variable names option must have been on.
*/

hacks:context_variables(NamedVariables) :-
	'$context_variables'(NamedVariables).


clean_goal(G,Mod,NG) :-
	beautify_hidden_goal(G,Mod,[NG],[]), !.
clean_goal(G,_,G).

scratch_goal(N,0,Mod,Mod:N) :-
	!.
scratch_goal(N,A,Mod,NG) :-
	list_of_qmarks(A,L),
	G=..[N|L],
	(
	  beautify_hidden_goal(G,Mod,[NG],[])
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


beautify_hidden_goal('$yes_no'(G,_Query), prolog) -->
	!,
	{ Call =.. [(?), G] },
	[Call].
beautify_hidden_goal('$do_yes_no'(G,Mod), prolog) -->
	[Mod:G].
beautify_hidden_goal(query(G,VarList), prolog) -->
	[query(G,VarList)].
beautify_hidden_goal('$enter_top_level', prolog) -->
	['TopLevel'].
% The user should never know these exist.
beautify_hidden_goal('$csult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_hidden_goal('$use_module'(Files,Mod,Is),prolog) -->
	[use_module(Mod,Files,Is)].
beautify_hidden_goal('$continue_with_command'(reconsult,V,P,G,Source),prolog) -->
	['Assert'(G,V,P,Source)].
beautify_hidden_goal('$continue_with_command'(consult,V,P,G,Source),prolog) -->
	['Assert'(G,V,P,Source)].
beautify_hidden_goal('$continue_with_command'(top,V,P,G,_),prolog) -->
	['Query'(G,V,P)].
beautify_hidden_goal('$continue_with_command'(Command,V,P,G,Source),prolog) -->
	['TopLevel'(Command,G,V,P,Source)].
beautify_hidden_goal('$system_catch'(G,Mod,Exc,Handler),prolog) -->
	[catch(Mod:G, Exc, Handler)].
beautify_hidden_goal('$catch'(G,Exc,Handler),prolog) -->
	[catch(G, Exc, Handler)].
beautify_hidden_goal('$execute_command'(Query,M,V,P,Option,Source),prolog) -->
	[toplevel_query(M:Query, V, P, Option, Source)].
beautify_hidden_goal('$process_directive'(Gs,_Mode,_VL),prolog) -->
	[(:- Gs)].
beautify_hidden_goal('$loop'(Stream,Option),prolog) -->
	[execute_load_file(Stream, consult=Option)].
beautify_hidden_goal('$load_files'(Files,Opts,?),prolog) -->
	[load_files(Files,Opts)].
beautify_hidden_goal('$load_files'(_,_,Name),prolog) -->
	[Name].
beautify_hidden_goal('$reconsult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_hidden_goal('$undefp'(Mod:G),prolog) -->
	['CallUndefined'(Mod:G)].
beautify_hidden_goal('$undefp'(?),prolog) -->
	['CallUndefined'(?:?)].
beautify_hidden_goal(repeat,prolog) -->
	[repeat].
beautify_hidden_goal('$recorded_with_key'(A,B,C),prolog) -->
	[recorded(A,B,C)].
beautify_hidden_goal('$findall_with_common_vars'(Templ,Gen,Answ),prolog) -->
	[findall(Templ,Gen,Answ)].
beautify_hidden_goal('$bagof'(Templ,Gen,Answ),prolog) -->
	[bagof(Templ,Gen,Answ)].
beautify_hidden_goal('$setof'(Templ,Gen,Answ),prolog) -->
	[setof(Templ,Gen,Answ)].
beautify_hidden_goal('$findall'(T,G,S,A),prolog) -->
	[findall(T,G,S,A)].
beautify_hidden_goal('$listing'(G,M,_Stream),prolog) -->
	[listing(M:G)].
beautify_hidden_goal('$call'(G,_CP,?,M),prolog) -->
	[call(M:G)].
beautify_hidden_goal('$call'(_G,_CP,G0,M),prolog) -->
	[call(M:G0)].
beautify_hidden_goal('$current_predicate'(Na,M,S,_),prolog) -->
	[current_predicate(Na,M:S)].
beautify_hidden_goal('$list_clauses'(Stream,M,Pred),prolog) -->
	[listing(Stream,M:Pred)].

:- meta_predicate(ctrace(0)).

ctrace(G) :-
    gated_call(start_low_level_trace,
	       G,
	       _,
	       stop_low_level_trace).

