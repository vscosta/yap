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
'$do_signal'(sig_creep, G) :-
	'$start_creep'(G).
'$do_signal'(sig_iti, G) :-
	'$thread_gfetch'(Goal),
	% if more signals alive, set creep flag
	'$continue_signals',
	'$current_module'(M0),
	'$execute0'(G,M0).
'$do_signal'(sig_trace, G) :-
	'$continue_signals',
	trace.
'$do_signal'(sig_debug, G) :-
	'$continue_signals',
	'$current_module'(M0),
	debug,
	'$execute0'(G,M0).
'$do_signal'(sig_break, G) :-
	'$continue_signals',
	'$current_module'(M0),
	break,
	'$execute0'(G,M0).
'$do_signal'(sig_statistics, G) :-
	'$continue_signals',
	'$current_module'(M0),
	statistics,
	'$execute0'(G,M0).
'$do_signal'(sig_stack_dump, G) :-
	'$continue_signals',
	'$current_module'(M0),
	'$stack_dump',
	'$execute0'(G,M0).
% Unix signals
'$do_signal'(sig_alarm, G) :-
	'$signal_handler'(sig_alarm, G).
'$do_signal'(sig_hup, G) :-
	'$signal_handler'(sig_hup, G).
'$do_signal'(sig_usr1, G) :-
	'$signal_handler'(sig_usr1, G).
'$do_signal'(sig_usr2, G) :-
	'$signal_handler'(sig_usr2, G).

'$signal_handler'(Sig, [M|G]) :-
	'$signal_do'(Sig, Goal),
	% if more signals alive, set creep flag
	'$continue_signals',
	'$current_module'(M0),
	'$execute0'((Goal,M:G),M0).

% notice that the last signal to be processed must always be creep
'$start_creep'([_|'$cut_by'(CP)]) :- !,
	'$cut_by'(CP),
	'$creep'.
'$start_creep'([_|true]) :- !,
	'$creep'.
'$start_creep'([Mod|G]) :-
	'$hidden_predicate'(G,Mod), !,
	'$creep',
	'$execute_nonstop'(G,Mod).
'$start_creep'([Mod|G]) :-
	'$stop_debugging',
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
'$signal_def'(sig_usr1, (print_message(error, 'Received user signal 1'),halt)).
'$signal_def'(sig_usr2, (print_message(error, 'Received user signal 2'),halt)).
% ignore sig_alarm by default
'$signal_def'(sig_alarm, true). 


on_signal(Signal,OldAction,default) :-
	'$reset_signal'(Signal, OldAction).
on_signal(Signal,OldAction,Action) :-
	var(Action), !,
	'$check_signal'(OldAction),
	Action = OldAction.
on_signal(Signal,OldAction,Action) :-
	'$reset_signal'(Signal, OldAction),
	'$current_module'(M),
	recordz('$sig_handler', action(Signal,M:Action), _).

'$reset_signal'(Signal, OldAction) :-
	recorded('$sig_handler', action(Signal,OldAction), Ref), !,
	erase(Ref).
'$reset_signal'(_, default).

'$check_signal'(Signal, OldAction) :-
	recorded('$sig_handler', action(Signal,OldAction), _), !.
'$reset_signal'(_, default).


alarm(Interval, Goal, Left) :-
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, Left).

raise_exception(Ball) :- throw(Ball).

on_exception(Pat, G, H) :- catch(G, Pat, H).

