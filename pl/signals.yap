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
	( '$access_yap_flags'(10,0) ->
	    % we're not allowed to creep for now,
	    % maybe we're inside builtin.
	    '$late_creep',
	    '$execute'(M:G)
	    ;
	    '$start_creep'([M|G])
	).
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
	'$do_not_creep',
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
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, Left).

raise_exception(Ball) :- throw(Ball).

on_exception(Pat, G, H) :- catch(G, Pat, H).

read_sig :-
	recorded('$signal_handler',X,_),
	writeq(X),nl,
	fail.
read_sig.


