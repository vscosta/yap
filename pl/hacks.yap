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
  * @file   pl/hacks.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:02:56 2017
  *
  * @brief  Access to the YAP engine internal data
  */

:- module('$hacks',
	  [display_stack_info/4,
	   display_stack_info/6,
	   display_pc/4,
       fully_strip_module/3,
	   code_location/3]).

/**
 * @namespace yap_hacks
 *
 * @defgroup Hacks Low-level access
 * @ingroup builtins
 * @{
 *
**/


/** yap_hacks:context_variables(-NamedVariables)
  Access variable names.

  Unify NamedVariables with a list of terms _Name_=_V_
  giving the names of the variables occurring in the last term read.
  Notice that variable names option must have been on.
*/

yap_hacks:context_variables(NamedVariables) :-
	'$context_variables'(NamedVariables).


prolog:'$stack_dump' :-
	yap_hacks:current_choicepoints(CPs),
	yap_hacks:current_continuations([Env|Envs]),
	yap_hacks:continuation(Env,_,ContP,_),
	length(CPs, LCPs),
	length(Envs, LEnvs),
	format(user_error,'~n~n~tStack Dump~t~40+~n~nAddress~tChoiceP~16+ Clause          Goal~n',[LCPs,LEnvs]),
	display_stack_info(CPs, Envs, 20, ContP, StackInfo, []),
	run_formats(StackInfo, user_error).

run_formats([], _).
run_formats([Com-Args|StackInfo], Stream) :-
	format(Stream, Com, Args),
	run_formats(StackInfo, Stream).

display_stack_info(CPs,Envs,Lim,PC) :-
	display_stack_info(CPs,Envs,Lim,PC,Lines,[]),
	flush_output(user_output),
	flush_output(user_error),
	print_message_lines(user_error, '', Lines).

code_location(Info,Where,Location) :-
	integer(Where) , !,
	pred_for_code(Where,Name,Arity,Mod,Clause),
	construct_code(Clause,Name,Arity,Mod,Info,Location).
code_location(Info,_,Info).

construct_code(-1,Name,Arity,Mod,Where,Location) :- !,
	number_codes(Arity,ArityCode),
	atom_codes(ArityAtom,ArityCode),
	atom_concat([Where,' at ',Mod,':',Name,'/',ArityAtom,' at indexing code'],Location).
construct_code(0,_,_,_,Location,Location) :- !.
construct_code(Cl,Name,Arity,Mod,Where,Location) :-
	number_codes(Arity,ArityCode),
	atom_codes(ArityAtom,ArityCode),
	number_codes(Cl,ClCode),
	atom_codes(ClAtom,ClCode),
	atom_concat([Where,' at ',Mod,':',Name,'/',ArityAtom,' (clause ',ClAtom,')'],Location).

'$prepare_loc'(Info,Where,Location) :- integer(Where), !,
	pred_for_code(Where,Name,Arity,Mod,Clause),
	'$construct_code'(Clause,Name,Arity,Mod,Info,Location).
'$prepare_loc'(Info,_,Info).

display_pc(PC, PP, Source) -->
	{ integer(PC) },
	{ pred_for_code(PC,Name,Arity,Mod,Clause) },
	pc_code(Clause, PP, Name, Arity, Mod, Source).

pc_code(0,_PP,_Name,_Arity,_Mod, 'top level or system code' - []) --> !.
pc_code(-1,_PP,Name,Arity,Mod, '~a:~q/~d' - [Mod,Name,Arity]) --> !,
	{ functor(S, Name,Arity),
	nth_clause(Mod:S,1,Ref),
	clause_property(Ref, file(File)),
	clause_property(Ref, line_count(Line)) },
	[ '~a:~d:0, ' - [File,Line] ].
pc_code(Cl,Name,Arity,Mod, 'clause ~d for ~a:~q/~d'-[Cl,Mod,Name,Arity]) -->
	{ Cl > 0 },
	{ functor(S, Name,Arity),
	nth_clause(Mod:S,Cl,Ref),
	clause_property(Ref, file(File)),
	clause_property(Ref, line_count(Line)) },
	[ '~a:~d:0, ' - [File,Line] ].

display_stack_info(_,_,0,_) --> !.
display_stack_info([],[],_,_) --> [].
display_stack_info([CP|CPs],[],I,_) -->
	show_lone_cp(CP),
	{ I1 is I-1 },
	display_stack_info(CPs,[],I1,_).
display_stack_info([],[Env|Envs],I,Cont) -->
	show_env(Env, Cont, NCont),
	{ I1 is I-1 },
	display_stack_info([], Envs, I1, NCont).
display_stack_info([CP|LCPs],[Env|LEnvs],I,Cont) -->
	{
	 yap_hacks:continuation(Env, _, NCont, CB),
	 I1 is I-1
	},
	( { CP == Env, CB < CP } ->
	    % if we follow choice-point and we cut to before choice-point
	    % we are the same goal
	   show_cp(CP, ''), %
           display_stack_info(LCPs, LEnvs, I1, NCont)
	;
          { CP > Env } ->
	   show_cp(CP, ' < '),
	   display_stack_info(LCPs,[Env|LEnvs],I1,Cont)
	;
	   show_env(Env,Cont,NCont),
	   display_stack_info([CP|LCPs],LEnvs,I1,NCont)
	).

show_cp(CP, Continuation) -->
	{ yap_hacks:choicepoint(CP, Addr, Mod, Name, Arity, Goal, ClNo) },
	( { Goal = (_;_) }
          ->
	  { scratch_goal(Name,Arity,Mod,Caller) },
	  [ '0x~16r~t*~16+ ~d~16+ ~q ~n'-
		[Addr, ClNo, Caller] ]

	    ;
	  [ '0x~16r~t *~16+~a ~d~16+ ~q:' -
		[Addr, Continuation, ClNo, Mod]]
	),
	{ prolog_flag( debugger_print_options, Opts) },
	{clean_goal(Goal,Mod,G)},
	['~@.~n' -  write_term(G,Opts)].

show_env(Env,Cont,NCont) -->
	{
	 yap_hacks:continuation(Env, Addr, NCont, _),
	format('0x~16r 0x~16r~n',[Env,NCont]),
	 yap_hacks:cp_to_predicate(Cont, Mod, Name, Arity, ClId)
	},
        [ '0x~16r~t  ~16+ ~d~16+ ~q:' -
		[Addr, ClId, Mod] ],
	{scratch_goal(Name, Arity, Mod, G)},
	{ prolog_flag( debugger_print_options, Opts) },
	['~@.~n' - write_term(G,Opts)].

clean_goal(G,Mod,NG) :-
	fail, beautify_hidden_goal(G,Mod,[NG],[]), !.
clean_goal(G,_,G).

scratch_goal(N,0,Mod,Mod:N) :-
	!.
scratch_goal(N,A,Mod,NG) :-
	list_of_qmarks(A,L),
	G=..[N|L],
	(
	  fail,beautify_hidden_goal(G,Mod,[NG],[])
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
beautify_hidden_goal('$query'(G,VarList), prolog) -->
	[query(G,VarList)].
beautify_hidden_goal('$enter_top_level', prolog) -->
	['TopLevel'].
% The user should never know these exist.
beautify_hidden_goal('$csult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_hidden_goal('$use_module'(Files,Mod,Is),prolog) -->
	[use_module(Mod,Files,Is)].
beautify_hidden_goal('$continue_with_command'(reconsult,V,P,G,Source),prolog) -->
	[assert(G,V,P,Source)].
beautify_hidden_goal('$continue_with_command'(consult,V,P,G,Source),prolog) -->
	[assert(G,V,P,Source)].
beautify_hidden_goal('$continue_with_command'(top,V,P,G,_),prolog) -->
	[query(G,V,P)].
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
beautify_hidden_goal('$load_files'(Files,M,Opts,?),prolog) -->
	[load_files(M:Files,Opts)].
beautify_hidden_goal('$load_files'(_,_,_,Name),prolog) -->
	[Name].
beautify_hidden_goal('$reconsult'(Files,Mod),prolog) -->
	[reconsult(Mod:Files)].
beautify_hidden_goal('$undefp'([Mod|G]),prolog) -->
	['undefined'(Mod:G)].
beautify_hidden_goal('$undefp'(?),prolog) -->
	[undefined(?:?)].
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

%% @}
