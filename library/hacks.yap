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
		  stack_dump/1
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
	display_stack_info(CPs, Envs, Max, ContP, StackInfo, StackInfo, []),
	run_formats(StackInfo, user_error).

run_formats([], _).
run_formats([Com-Args|StackInfo], Stream) :-
	format(Stream, Com, Args),
	run_formats(StackInfo, user_error).



