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
% never creep on entering system mode!!!
% don't creep on meta-call.
'$do_signal'(sig_creep, [M|G]) :-
	'$creep_allowed', !,
	(
	 ( G = '$notrace'(G0) ;  G = '$oncenotrace'(G0) ; G = '$execute0'(G0,M) ; '$system_module'(M), G = G0 )
	->
	 (
	  '$execute_nonstop'(G0,M),
	  '$signal_creep'
	 ;
	  '$signal_creep',
	  fail
	 )
	;
	 '$start_creep'([M|G])
	).
% 
'$do_signal'(sig_creep, [M|G]) :-
	( G = '$notrace'(G0) ;  G = '$oncenotrace'(G0) ; G = '$execute0'(G0,M) ; '$system_module'(M), G = G0 ),
	!,
	(
	 '$execute_nonstop'(G0,M),
	 '$signal_creep'
	;
	 '$signal_creep',
	 fail
	).
% 
'$do_signal'(sig_creep, [M|G]) :-
        '$signal_creep',
	'$execute_nonstop'(G,M).
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
% the next one should never be called...
'$do_signal'(fail, [_|_]) :-
	fail.
'$do_signal'(sig_stack_dump, [M|G]) :-
	'$continue_signals',
	'$stack_dump',
	'$execute0'(G,M).
% Unix signals
'$do_signal'(sig_alarm, G) :-
	'$signal_handler'(sig_alarm, G).
'$do_signal'(sig_vtalarm, G) :-
	'$signal_handler'(sig_vtalarm, G).
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

% '$execute0' should be ignored.
'$start_creep'([_|'$execute0'(G,M)]) :-
	!,
	'$start_creep'([M|G]).
% '$call'() is a complicated thing
'$start_creep'([M0|'$call'(G, CP, G0, M)]) :-
	!,
	'$creep',
	'$execute_nonstop'('$call'(G, CP, G0, M),M0).
% donotrace: means do not trace! So,
% ignore and then put creep back for the continuation.
'$start_creep'([M0|'$notrace'(G)]) :-
	!,
	(
	 CP0 is '$last_choice_pt',
	 '$execute_nonstop'(G,M0),
	 CP1 is '$last_choice_pt',
	 % exit port: creep
	 '$creep',
	 (
	  % if deterministic just creep all you want.
	  CP0 = CP1 ->
	  !
	 ;
	  % extra disjunction protects reentry into usergoal
	  (
	    % cannot cut here
	    true
	   ;
	    % be sure to disable creep on redo port
	    '$disable_creep',
	    fail
	  )
	 )
	;
	   % put it back again on fail
	 '$creep',
	 fail	   
	).
'$start_creep'([M0|'$oncenotrace'(G)]) :-
	!,
	('$execute_nonstop'(G,M0),
	 CP1 is '$last_choice_pt',
	 % exit port: creep
	 '$creep',
	 !
	;
	   % put it back again on fail
	 '$creep',
	 fail	   
	).
% do not debug if we are not in debug mode. 
'$start_creep'([Mod|G]) :-
	'$debug_on'(DBON), DBON = false, !,
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
	'$execute_nonstop'(G,Mod),
	'$creep'.
% do not debug if we are zipping through.  
'$start_creep'([Mod|G]) :-
	nb_getval('$debug_run',Run),
	Run \= off,
	'$zip'(-1, G, Mod), !,
        '$signal_creep',
	'$execute_goal'(G, Mod).
'$start_creep'([Mod|G]) :-
	CP is '$last_choice_pt',	
	'$do_spy'(G, Mod, CP, no).

'$execute_goal'(G, Mod) :-
	(
	 '$is_metapredicate'(G, Mod)
	->
	'$meta_call'(G,Mod)
	;
	'$execute_nonstop'(G,Mod)
	).


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


'$signal'(sig_hup).
'$signal'(sig_usr1).
'$signal'(sig_usr2).
'$signal'(sig_pipe).
'$signal'(sig_alarm).
'$signal'(sig_vtalarm).

on_signal(Signal,OldAction,NewAction) :-
	var(Signal), !,
	(nonvar(OldAction) -> throw(error(instantiation_error,on_signal/3)) ; true),
        '$signal'(Signal),
	on_signal(Signal, OldAction, NewAction).
on_signal(Signal,OldAction,default) :-
	'$reset_signal'(Signal, OldAction).
on_signal(Signal,OldAction,Action) :-
	var(Action), !,
	throw(error(system_error,'Somehow the meta_predicate declarations of on_signal are subverted!')).
on_signal(Signal,OldAction,Action) :-
	Action = (_:Goal),
	var(Goal), !,
	'$check_signal'(Signal, OldAction),
	Goal = OldAction.
on_signal(Signal,OldAction,Action) :-
	'$reset_signal'(Signal, OldAction),
        % 13211-2 speaks only about callable
	( Action = M:Goal -> true ; throw(error(type_error(callable,Action),on_signal/3)) ),
	% the following disagrees with 13211-2:6.7.1.4 which disagrees with 13211-1:7.12.2a
	% but the following agrees with 13211-1:7.12.2a
	( nonvar(M) -> true ; throw(error(instantiation_error,on_signal/3)) ),
	( atom(M) -> true ; throw(error(type_error(callable,Action),on_signal/3)) ),
	( nonvar(Goal) -> true ; throw(error(instantiation_error,on_signal/3)) ),
	recordz('$signal_handler', action(Signal,Action), _).

'$reset_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), Ref), !,
	erase(Ref).
'$reset_signal'(_, default).

'$check_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), _), !.
'$check_signal'(_, default).


alarm(Interval, Goal, Left) :-
	Interval == 0, !,
	'$alarm'(0, 0, Left0, _),
	on_signal(sig_alarm, _, Goal),
	Left = Left0.
alarm(Interval, Goal, Left) :-
	integer(Interval), !,
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, 0, Left, _).
alarm(Number, Goal, Left) :-
	float(Number), !,
	Secs is integer(Number),
	USecs is integer((Number-Secs)*1000000) mod 1000000,
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



