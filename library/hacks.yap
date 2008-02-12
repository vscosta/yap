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
	display_stack_info(CPs,Envs,Max,ContP).

display_stack_info(_,_,0,_) :- !.
display_stack_info([],[],_,_).
display_stack_info([CP|CPs],[],I,_) :-
	show_lone_cp(CP),
	I1 is I-1,
	display_stack_info(CPs,[],I1,_).
display_stack_info([],[Env|Envs],I,Cont) :-
	show_env(Env, Cont, NCont),
	I1 is I-1,
	display_stack_info([], Envs, I1, NCont).
display_stack_info([CP|LCPs],[Env|LEnvs],I,Cont) :-
	continuation(Env, _, NCont, CB),
	I1 is I-1,
	( CP == Env, CB < CP ->
	    % if we follow choice-point and we cut to before choice-point
	    % we are the same goal
	   show_cp(CP, 'Cur'), %
           display_stack_info(LCPs, LEnvs, I1, NCont)
	;
          CP > Env ->
	   show_cp(CP, 'Next'),
	   display_stack_info(LCPs,[Env|LEnvs],I1,Cont)
	;
	   show_env(Env,Cont,NCont),
	   display_stack_info([CP|LCPs],LEnvs,I1,NCont)
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


