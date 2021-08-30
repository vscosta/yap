/**
 * @file   library/hacks.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 19:00:25 2015
 *
 * @brief  Prolog hacking
 *
 *
*/
/**
  * @addtogroup Hacks Prolog state manipulation.
  * @ingroup YAPLibrary
  * @{
  * @brief Manipulate the Prolog stacks, including setting and resetting
  * choice-points.
  *
**/

:- module(yap_hacks, [
		      parent_choicepoint/1,
		      parent_choicepoint/2,
%		      cut_by/1,
		      cut_at/1,
		      current_choice_points/1,
		      choicepoint/7,
		      current_continuations/1,
		      continuation/4,
		      stack_dump/0,
		      stack_dump/1,
		      enable_interrupts/0,
		      disable_interrupts/0,
		      virtual_alarm/3,
		      alarm/3,
              	      fully_strip_module/3,
		      context_variables/1
                     ]).



/**
 * @pred stack_dump
 *
 * Write the current ancestor stack to the outout. Ancestors may have:
 * - terminated
 * - still have sub-goals to execute, if so, they left an _environment_
 * - still have clauses they may nacktrack to; if so, they left a _choice point_
 *
 */
stack_dump :-
	stack_dump(-1).

/**
 * @pred stack_dump(+N)
 *
 * Report the last _N_ entries in the stack (see stack_dump/0)
 */

stack_dump(Max) :-
	current_choice_points(CPs),
	current_continuations([Env|Envs]),
	continuation(Env,_,ContP,_),
	length(CPs, LCPs),
	length(Envs, LEnvs),
	format(user_error,'~n~n~tStack Dump~t~40+~n~nAddress~tChoiceP~16+ Cur/Next Clause        Goal~n',[LCPs,LEnvs]),
	display_stack_info(CPs, Envs, Max, ContP).

display_stack_info(CPs,Envs,Lim,PC) :-
	display_stack_info(CPs,Envs,Lim,PC,Lines,[]),
	flush_output(user_output),
	flush_output(user_error),
	run_formats(Lines, user_error).


run_formats([], _).
run_formats([Com-Args|StackInfo], Stream) :-
	format(Stream, Com, Args),
	run_formats(StackInfo, Stream).

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


/**
 * @pred virtual_alarm(+Interval, 0:Goal, -Left)
 *
 * Activate  an alarm to execute _Goal_ in _Interval_ seconds. If the alarm was active,
 * bind _Left_ to the previous value.
 *
 * If _Interval_ is 0, disable the current alarm.
 */
virtual_alarm(Interval, Goal, Left) :-
	Interval == 0, !,
	'$virtual_alarm'(0, 0, Left0, _),
	on_signal(sig_vtalarm, _, Goal),
	Left = Left0.
virtual_alarm(Interval, Goal, Left) :-
	integer(Interval), !,
	on_signal(sig_vtalarm, _, Goal),
	'$virtual_alarm'(Interval, 0, Left, _).
virtual_alarm([Interval|USecs], Goal, [Left|LUSecs]) :-
	on_signal(sig_vtalarm, _, Goal),
	'$virtual_alarm'(Interval, USecs, Left, LUSecs).


    %% @}
