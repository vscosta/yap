
:- protocol(monitorp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Monitor protocol.']).


	:- public(monitor_activated/0).

	:- mode(monitor_activated, zero_or_one).

	:- info(monitor_activated/0, [
		comment is 'True if monitor is currently active.']).


	:- public(activate_monitor/0).

	:- mode(activate_monitor, one).

	:- info(activate_monitor/0, [
		comment is 'Activates all spy points and start monitoring.']).


	:- public(suspend_monitor/0).

	:- mode(suspend_monitor, one).

	:- info(suspend_monitor/0, [
		comment is 'Suspends monitoring, deactivating all spy points.']).


	:- public(reset_monitor/0).

	:- mode(reset_monitor, one).

	:- info(reset_monitor/0, [
		comment is 'Resets monitor, deactivating and deleting all spy points.']).


	:- public(spy_point/4).

	:- mode(spy_point(?event, ?object, ?callable, ?object), zero_or_more).

	:- info(spy_point/4, [
		comment is 'Current spy point.',
		argnames is ['Event', 'Object', 'Message', 'Sender']]).


	:- public(set_spy_point/4).

	:- mode(set_spy_point(?event, ?object, ?callable, ?object), one).

	:- info(set_spy_point/4, [
		comment is 'Sets a spy point.',
		argnames is ['Event', 'Object', 'Message', 'Sender']]).


	:- public(del_spy_points/4).

	:- mode(del_spy_points(@event, @object, @callable, @object), one).

	:- info(del_spy_points/4, [
		comment is 'Deletes all matching spy points.',
		argnames is ['Event', 'Object', 'Message', 'Sender']]).


:- end_protocol.
