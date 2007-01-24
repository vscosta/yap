%   File   : hacks.yap
%   Author : Vitor Santos Costa
%   Updated: 2007
%   Purpose: Prolog hacking

:- module(yap_hacks, [
		  cut_by/1,
		  current_choicepoints/1,
		  choicepoint/6,
		  current_continuations/1,
		  continuation/4,
		  stack_dump/0
              ]).

stack_dump :-
	current_choicepoints(CPs),
	current_continuations([Env|Envs]),
	continuation(Env,_,ContP,_),
	length(CPs, LCPs),
	length(Envs, LEnvs),
	format(user_error,'~n~n~tStack Dump~t~40+~n~nAddress~tChoiceP~16+ Cur/Next Clause        Goal~n',[LCPs,LEnvs]),
	display_stack_info(CPs,Envs,ContP).

display_stack_info([],[],_).
display_stack_info([CP|CPs],[],_) :-
	show_lone_cp(CP),
	display_stack_info(CPs,[],_).
display_stack_info([],[Env|Envs],Cont) :-
	show_env(Env, Cont, NCont),
	display_stack_info([], Envs, NCont).
display_stack_info([CP|LCPs],[Env|LEnvs],Cont) :-
	continuation(Env, _, NCont, CB), 
	( CP == Env, CB < CP ->
	    % if we follow choice-point and we cut to before choice-point
	    % we are the same goal
	   show_cp(CP, 'Cur'), %
           display_stack_info(LCPs, LEnvs, NCont)
	;
          CP > Env ->
	   show_cp(CP, 'Next'),
	   display_stack_info(LCPs,[Env|LEnvs],Cont)
	;
	   show_env(Env,Cont,NCont),
	   display_stack_info([CP|LCPs],LEnvs,NCont)
	).

show_cp(CP, Continuation) :-
	choicepoint(CP, Addr, Mod, Name, Arity, Goal, ClNo),
	( Goal = (_;_)
          ->
	  format(user_error,'0x~16r~t*~16+ Cur~t~d~16+ ~q:~q/~d( ? ; ? )~n',
		[Addr, ClNo, Mod, Name, Arity])
	    ;
	  prolog_flag( debugger_print_options, Opts),
	  format(user_error,'0x~16r~t *~16+ ~a~t ~d~16+ ~q:~@~n',
		[Addr, Continuation, ClNo, Mod, write_term(Goal,Opts)])
	).

show_env(Env,Cont,NCont) :-
	continuation(Env, Addr, NCont, _),
	cp_to_predicate(Cont, Mod, Name, Arity, ClId),
        format(user_error,'0x~16r~t  ~16+ Cur~t ~d~16+ ~q:~q~@~n',
		[Addr, ClId, Mod, Name, show_args(Arity)]).

show_args(0) :- !.
show_args(I) :-
	format('(?',[]),
	I1 is I-1,
	show_inner_args(I1),
	format(')',[]).

show_inner_args(0) :- !.
show_inner_args(I) :-
	format(', ?',[]),
	I1 is I-1,
	show_inner_args(I1).


