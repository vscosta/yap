
:- protocol(event_registryp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Event registry protocol.']).

	:- public(monitors/1).
	:- mode(monitors(-list), one).
	:- info(monitors/1, [
		comment is 'Returns a list of all current monitors.',
		argnames is ['Monitors']]).

	:- public(monitored/1).
	:- mode(monitored(-list), one).
	:- info(monitored/1, [
		comment is 'Returns a list of all currently monitored objects.',
		argnames is ['Objects']]).

	:- public(monitor/4).
	:- mode(monitor(?object, ?nonvar, ?object, ?object), zero_or_more).
	:- info(monitor/4, [
		comment is 'True if the arguments describe a currently defined monitored event.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(set_monitor/4).
	:- mode(set_monitor(?object, ?nonvar, ?object, +object), zero_or_one).
	:- info(set_monitor/4, [
		comment is 'Sets a monitor for the set of matching events.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(del_monitors/4).
	:- mode(del_monitors(?object, ?nonvar, ?object, ?object), one).
	:- info(del_monitors/4, [
		comment is 'Deletes all matching monitored events.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(del_monitors/0).
	:- mode(del_monitors, one).
	:- info(del_monitors/0, [
		comment is 'Deletes all monitored events.']).

:- end_protocol.
