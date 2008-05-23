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
* File:		signals.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	signal handling in YAP					 *
*									 *
*************************************************************************/

:- meta_predicate on_signal(+,?,:), alarm(+,:,-).

'$creep'(G) :-
	% get the first signal from the mask
	'$first_signal'(Sig), !,
	% process it
	'$do_signal'(Sig, G).
'$creep'([M|G]) :-
	% noise, just go on with our life.
	'$execute'(M:G).

'$do_signal'(sig_wake_up, G) :-
 	'$awoken_goals'(LG),
	% if more signals alive, set creep flag
	'$continue_signals',
	'$wake_up_goal'(G, LG).
'$do_signal'(sig_creep, [M|G]) :-
        '$start_creep'([M|G]).
'$do_signal'(sig_delay_creep, [M|G]) :-
	'$execute'(M:G),
        '$creep'.
'$do_signal'(sig_iti, [M|G]) :-
	'$thread_gfetch'(Goal),
	% if more signals alive, set creep flag
	'$continue_signals',
	'$current_module'(M0),
	'$execute0'(Goal,M0),
	'$execute'(M:G).
'$do_signal'(sig_trace, [M|G]) :-
	'$continue_signals',
	trace,
	'$execute'(M:G).
'$do_signal'(sig_debug, [M|G]) :-
	'$continue_signals',
	debug,
	'$execute'(M:G).
'$do_signal'(sig_break, [M|G]) :-
	'$continue_signals',
	break,
	'$execute0'(G,M).
'$do_signal'(sig_statistics, [M|G]) :-
	'$continue_signals',
	statistics,
	'$execute0'(G,M).
'$do_signal'(sig_stack_dump, [M|G]) :-
	'$continue_signals',
	'$stack_dump',
	'$execute0'(G,M).
% Unix signals
'$do_signal'(sig_alarm, G) :-
	'$signal_handler'(sig_alarm, G).
'$do_signal'(sig_hup, G) :-
	'$signal_handler'(sig_hup, G).
'$do_signal'(sig_usr1, G) :-
	'$signal_handler'(sig_usr1, G).
'$do_signal'(sig_usr2, G) :-
	'$signal_handler'(sig_usr2, G).
'$do_signal'(sig_pipe, G) :-
	'$signal_handler'(sig_pipe, G).

'$signal_handler'(Sig, [M|G]) :-
	'$signal_do'(Sig, Goal),
	% if more signals alive, set creep flag
	'$continue_signals',
	'$current_module'(M0),
	'$execute0'((Goal,M:G),M0).

% do not debug if we are not in debug mode. 
'$start_creep'([Mod|G]) :-
	nb_getval('$debug',off), !,
	'$execute_nonstop'(G,Mod).
'$start_creep'([Mod|G]) :-
	nb_getval('$system_mode',on), !,
	'$execute_nonstop'(G,Mod).
% notice that the last signal to be processed must always be creep
'$start_creep'([_|'$cut_by'(CP)]) :- !,
	'$$cut_by'(CP),
	'$creep'.
'$start_creep'([_|true]) :- !,
	'$creep'.
'$start_creep'([Mod|G]) :-
	'$hidden_predicate'(G,Mod), !,
	'$creep',
	'$execute_nonstop'(G,Mod).
'$start_creep'([Mod|G]) :-
	'$system_predicate'(G, Mod),
	'$protected_env', !,
	'$creep',
	'$execute_nonstop'(G,Mod).
% do not debug if we are zipping through.  
'$start_creep'([Mod|G]) :-
	nb_getval('$debug_zip',on),
	'$zip'(-1, G, Mod), !,
	'$creep',
	'$execute_nonstop'(G,Mod).
'$start_creep'([Mod|G]) :-
	CP is '$last_choice_pt',	
	'$do_spy'(G, Mod, CP, yes).

'$signal_do'(Sig, Goal) :-
	recorded('$signal_handler', action(Sig,Goal), _), !.
'$signal_do'(Sig, Goal) :-
	'$signal_def'(Sig, Goal).

% reconsult init files.
'$signal_def'(sig_hup, (( exists('~/.yaprc') -> [-'~/.yaprc'] ; true ),
		      ( exists('~/.prologrc') -> [-'~/.prologrc'] ; true ),
		      ( exists('~/prolog.ini') -> [-'~/prolog.ini'] ; true ))).
% die on signal default.
'$signal_def'(sig_usr1, throw(error(signal(usr1,[]),true))).
'$signal_def'(sig_usr2, throw(error(signal(usr2,[]),true))).
'$signal_def'(sig_pipe, throw(error(signal(pipe,[]),true))).
% ignore sig_alarm by default
'$signal_def'(sig_alarm, true). 


on_signal(Signal,OldAction,default) :-
	'$reset_signal'(Signal, OldAction).
on_signal(Signal,OldAction,Action) :-
	var(Action), !,
	'$check_signal'(Signal, OldAction),
	Action = OldAction.
on_signal(Signal,OldAction,Action) :-
	'$reset_signal'(Signal, OldAction),
	'$current_module'(M),
	recordz('$signal_handler', action(Signal,M:Action), _).

'$reset_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), Ref), !,
	erase(Ref).
'$reset_signal'(_, default).

'$check_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), _), !.
'$check_signal'(_, default).


alarm(Interval, Goal, Left) :-
	integer(Interval), !,
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, 0, Left, _).
alarm(Interval.USecs, Goal, Left.LUSecs) :-
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, USecs, Left, LUSecs).

raise_exception(Ball) :- throw(Ball).

on_exception(Pat, G, H) :- catch(G, Pat, H).

read_sig :-
	recorded('$signal_handler',X,_),
	writeq(X),nl,
	fail.
read_sig.


'$protected_env' :-
	yap_hacks:current_continuations([Env|Envs]),
	yap_hacks:continuation(Env,_,Addr,_),
%'$envs'(Envs, Addr),
	'$skim_envs'(Envs,Addr,Mod,Name,Arity),
	\+ '$external_call_seen'(Mod,Name,Arity).


'$envs'([Env|Envs], Addr0) :-
        yap_hacks:cp_to_predicate(Addr0,Mod0,Name0,Arity0,ClId),
	format(user_error,'~a:~w/~w ~d~n',[Mod0,Name0,Arity0,ClId]),
        yap_hacks:continuation(Env,_,Addr,_),
	 '$envs'(Envs, Addr).
'$envs'([], _) :- format(user_error,'*****done*****~n',[]).

'$skim_envs'([Env|Envs],Addr0,Mod,Name,Arity) :-
	yap_hacks:cp_to_predicate(Addr0, Mod0, Name0, Arity0, _ClId),
	'$debugger_env'(Mod0,Name0,Arity0), !,
        yap_hacks:continuation(Env,_,Addr,_),
	'$skim_envs'(Envs,Addr,Mod,Name,Arity).
'$skim_envs'(_,Addr,Mod,Name,Arity) :-
	yap_hacks:cp_to_predicate(Addr, Mod, Name, Arity, _ClId).

'$debugger_env'(prolog,'$start_creep',1).

'$external_call_seen'(prolog,Name,Arity) :- !,
	 '$allowed'(Name,Arity).
'$external_call_seen'(_,_,_).

 '$allowed'('$spycall',4).
 '$allowed'('$query',2).

