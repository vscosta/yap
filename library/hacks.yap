/**
 * @file   library/hacks.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 19:00:25 2015
 *
 * @brief  Prolog hacking
 *
 *
*/

:- module(yap_hacks, [
		      current_choicepoint/1,
		      parent_choicepoint/1,
		      parent_choicepoint/2,
		      cut_by/1,
		      cut_at/1,
		      current_choicepoints/1,
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
  * @addtogroup Hacks
  * @{
  * @brief Manipulate the Prolog stacks, including setting and resetting
  * choice-points.
  *
**/

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
	current_choicepoints(CPs),
	current_continuations([Env|Envs]),
	continuation(Env,_,ContP,_),
	length(CPs, LCPs),
	length(Envs, LEnvs),
	format(user_error,'~n~n~tStack Dump~t~40+~n~nAddress~tChoiceP~16+ Cur/Next Clause        Goal~n',[LCPs,LEnvs]),
	'$hacks':display_stack_info(CPs, Envs, Max, ContP, StackInfo, []),
	run_formats(StackInfo, user_error).

run_formats([], _).
run_formats([Com-Args|StackInfo], Stream) :-
	format(Stream, Com, Args),
	run_formats(StackInfo, user_error).


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
