%   File   : hacks.yap
%   Author : Vitor Santos Costa
%   Updated: 2007
%   Purpose: Prolog hacking

:- module(yap_hacks, [
		      current_choicepoint/1,
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
		      virtual_alarm/3
              ]).

stack_dump :-
	stack_dump(-1).

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

virtual_alarm(Interval, Goal, Left) :-
	Interval == 0, !,
	virtual_alarm(0, 0, Left0, _),
	on_signal(sig_vtalarm, _, Goal),
	Left = Left0.
virtual_alarm(Interval, Goal, Left) :-
	integer(Interval), !,
	on_signal(sig_vtalarm, _, Goal),
	virtual_alarm(Interval, 0, Left, _).
virtual_alarm(Interval.USecs, Goal, Left.LUSecs) :-
	on_signal(sig_vtalarm, _, Goal),
	virtual_alarm(Interval, USecs, Left, LUSecs).

