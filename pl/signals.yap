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

%%! @addtogroup OS
%%  @{
:- system_module( '$_signals', [alarm/3,
				on_exception/3,
				on_signal/3,
				raise_exception/1,
				read_sig/0], []).

:- use_system_module( '$_boot', ['$meta_call'/2]).

:- use_system_module( '$_debug', ['$trace'/1]).

:- use_system_module( '$_threads', ['$thread_gfetch'/1]).

/** @pred  alarm(+ _Seconds_,+ _Callable_,+ _OldAlarm_)


  Arranges for YAP to be interrupted in  _Seconds_ seconds, or in
  [ _Seconds_| _MicroSeconds_]. When interrupted, YAP will execute
  _Callable_ and then return to the previous execution. If
  _Seconds_ is `0`, no new alarm is scheduled. In any event,
  any previously set alarm is canceled.

  The variable  _OldAlarm_ unifies with the number of seconds remaining
  until any previously scheduled alarm was due to be delivered, or with
  `0` if there was no previously scheduled alarm.

  Note that execution of  _Callable_ will wait if YAP is
  executing built-in predicates, such as Input/Output operations.

  The next example shows how  _alarm/3_ can be used to implement a
  simple clock:

  ~~~~~
  loop :- loop.

  ticker :- write('.'), flush_output,
  get_value(tick, yes),
  alarm(1,ticker,_).

  :- set_value(tick, yes), alarm(1,ticker,_), loop.
  ~~~~~

  The clock, `ticker`, writes a dot and then checks the flag
  `tick` to see whether it can continue ticking. If so, it calls
  itself again. Note that there is no guarantee that the each dot
  corresponds a second: for instance, if the YAP is waiting for
  user input, `ticker` will wait until the user types the entry in.

  The next example shows how alarm/3 can be used to guarantee that
  a certain procedure does not take longer than a certain amount of time:

  ~~~~~
  loop :- loop.

  :-   catch((alarm(10, throw(ball), _),loop),
  ball,
  format('Quota exhausted.~n',[])).
  ~~~~~
  In this case after `10` seconds our `loop` is interrupted,
  `ball` is thrown,  and the handler writes `Quota exhausted`.
  Execution then continues from the handler.

  Note that in this case `loop/0` always executes until the alarm is
  sent. Often, the code you are executing succeeds or fails before the
  alarm is actually delivered. In this case, you probably want to disable
  the alarm when you leave the procedure. The next procedure does exactly so:

  ~~~~~
  once_with_alarm(Time,Goal,DoOnAlarm) :-
  catch(execute_once_with_alarm(Time, Goal), alarm, DoOnAlarm).

  execute_once_with_alarm(Time, Goal) :-
  alarm(Time, alarm, _),
  ( call(Goal) -> alarm(0, alarm, _) ; alarm(0, alarm, _), fail).
  ~~~~~

  The procedure `once_with_alarm/3` has three arguments:
  the  _Time_ to wait before the alarm is
  sent; the  _Goal_ to execute; and the goal  _DoOnAlarm_ to execute
  if the alarm is sent. It uses catch/3 to handle the case the
  `alarm` is sent. Then it starts the alarm, calls the goal
  _Goal_, and disables the alarm on success or failure.


*/
/** @pred  on_signal(+ _Signal_,? _OldAction_,+ _Callable_)


  Set the interrupt handler for soft interrupt  _Signal_ to be
  _Callable_.  _OldAction_ is unified with the previous handler.

  Only a subset of the software interrupts (signals) can have their
  handlers manipulated through on_signal/3.
  Their POSIX names, YAP names and default behavior is given below.
  The "YAP name" of the signal is the atom that is associated with
  each signal, and should be used as the first argument to
  on_signal/3. It is chosen so that it matches the signal's POSIX
  name.

  on_signal/3 succeeds, unless when called with an invalid
  signal name or one that is not supported on this platform. No checks
  are made on the handler provided by the user.

  + sig_up (Hangup)
  SIGHUP in Unix/Linux; Reconsult the initialization files
  ~/.yaprc, ~/.prologrc and ~/prolog.ini.
  + sig_usr1 and sig_usr2 (User signals)
  SIGUSR1 and SIGUSR2 in Unix/Linux; Print a message and halt.


  A special case is made, where if  _Callable_ is bound to
  `default`, then the default handler is restored for that signal.

  A call in the form `on_signal( _S_, _H_, _H_)` can be used
  to retrieve a signal's current handler without changing it.

  It must be noted that although a signal can be received at all times,
  the handler is not executed while YAP is waiting for a query at the
  prompt. The signal will be, however, registered and dealt with as soon
  as the user makes a query.

  Please also note, that neither POSIX Operating Systems nor YAP guarantee
  that the order of delivery and handling is going to correspond with the
  order of dispatch.
*/
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
'$do_signal'(sig_creep, MG) :-
	'$disable_debugging',
	'$start_creep'(MG, outer, _).
'$do_signal'(sig_iti, MG) :-
	'$thread_gfetch'(Goal),
				% if more signals alive, set creep flag
	'$continue_signals',
	'$current_module'(M0),
	'$execute0'(Goal,M0),
	'$execute'(MG).
'$do_signal'(sig_trace, MG) :-
	'$continue_signals',
	trace,
	'$execute'(MG).
'$do_signal'(sig_debug, MG) :-
	'$continue_signals',
	debug,
	'$execute'(MG). 
  '$do_signal'(Sig, _) :-
	  '$signal_do'(Sig,G),
	  '$execute'(G). 
  

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

				% reconsult init files. %
'$signal_def'(sig_hup, (( exists('~/.yaprc') -> [-'~/.yaprc'] ; true ),
			( exists('~/.prologrc') -> [-'~/.prologrc'] ; true ),
			( exists('~/prolog.ini') -> [-'~/prolog.ini'] ; true ))).
				% die on signal default. %
'$signal_def'(sig_usr1, throw(signal(usr1,[]))).
'$signal_def'(sig_usr2, throw(signal(usr2,[]))).
'$signal_def'(sig_pipe, throw(signal(pipe,[]))).
'$signal_def'(sig_fpe, throw(signal(fpe,[]))).
				% ignore sig_alarm by default %
'$signal_def'(sig_alarm, true).


'$signal'(sig_hup).
'$signal'(sig_usr1).
'$signal'(sig_usr2).
'$signal'(sig_pipe).
'$signal'(sig_alarm).
'$signal'(sig_vtalarm).
'$signal'(sig_fpe).

on_signal(Signal,OldAction,NewAction) :-
	var(Signal), !,
	(nonvar(OldAction) -> throw(error(instantiation_error,on_signal/3)) ; true),
        '$signal'(Signal),
	on_signal(Signal, OldAction, NewAction).
on_signal(Signal,OldAction,default) :-
	'$reset_signal'(Signal, OldAction).
on_signal(_Signal,_OldAction,Action) :-
	var(Action), !,
	throw(error('SYSTEM_ERROR_INTERNAL','Somehow the meta_predicate declarations of on_signal are subverted!')).
on_signal(Signal,OldAction,Action) :-
	'$yap_strip_module'(Action,_Mod,Goal),
	var(Goal), !,
	'$check_signal'(Signal, OldAction),
	Goal = OldAction.
on_signal(Signal,OldAction,Action) :-
	'$yap_strip_module'(Action,M,Goal),
	'$reset_signal'(Signal, OldAction),
				% 13211-2 speaks only about callable %
				% the following disagrees with 13211-2:6.7.1.4 which disagrees with 13211-1:7.12.2a %
				% but the following agrees with 13211-1:7.12.2a %
	( nonvar(M) -> true ; throw(error(instantiation_error,on_signal/3)) ),
	( atom(M) -> true ; throw(error(type_error(callable,Action),on_signal/3)) ),
	must_be_callable(Goal ),
	recordz('$signal_handler', action(Signal,Action), _).

'$reset_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), Ref), !,
	erase(Ref).
'$reset_signal'(_, default).

'$check_signal'(Signal, OldAction) :-
	recorded('$signal_handler', action(Signal,OldAction), _), !.
'$check_signal'(_, default).


/**
 * @pred alarm(+Interval, 0:Goal, -Left)
 *
 * Activate  an alarm to execute _Goal_ in _Interval_ seconds. If the alarm was active,
 * bind _Left_ to the previous value.
 *
 * If _Interval_ is 0, disable the current alarm.
 */
alarm(Interval, Goal, Left) :-
	Interval == 0, !,
	'$alarm'(0, 0, Left0, _),
	on_signal(sig_alarm, _, Goal),
	Left = Left0.
alarm(Interval, Goal, Left) :-
	integer(Interval), !,
	on_signal(sig_alarm, _, Goal),	'$alarm'(Interval, 0, Left, _).
alarm(Number, Goal, Left) :-
	float(Number), !,
	Secs is integer(Number),
	USecs is integer((Number-Secs)*1000000) mod 1000000,
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Secs, USecs, Left, _).
alarm([Interval|USecs], Goal, [Left|LUSecs]) :-
	on_signal(sig_alarm, _, Goal),
	'$alarm'(Interval, USecs, Left, LUSecs).

raise_exception(Ball) :- throw(Ball).

on_exception(Pat, G, H) :- catch(G, Pat, H).

read_sig :-
	recorded('$signal_handler',X,_),
	writeq(X),nl,
	fail.
read_sig.

				% %
				% make thes predicates non-traceable. %

:- '$set_private'(current_choicepoint(_DCP), yap_hacks).
:- '$set_private'('$current_choice_point'(_DCP), _).
:- '$set_private'('$$cut_by'(_DCP), prolog).
:- '$set_private'(true, yap_hacks).
:- '$set_private'(true, prolog).
:- '$set_private'('$call'(_,_,_,_), prolog).
:- '$set_private'('$execute_nonstop'(_,_), prolog).
:- '$set_private'('$execute_clause'(_,_,_,_), prolog).
:- '$set_private'(restore_regs(_,_), attributes).
:- '$set_private'('$undefp'(_,_), prolog).
:- '$set_private'('$Error'(_), prolog).
:- '$set_private'('$LoopError'(_,_), prolog).
:- '$set_private'('$TraceError'(_,_,_,_,_), prolog).

%%! @}
