
:- object(after_event_registry,
	implements(event_registryp)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'After events registry predicates.']).

	monitors(Monitors) :-
		findall(Monitor, current_event(after, _, _, _, Monitor), List),
		{sort(List, Monitors)}.

	monitored(Objects) :-
		findall(Object, current_event(after, Object, _, _, _), List),
		{sort(List, Objects)}.

	monitor(Object, Message, Sender, Monitor) :-
		current_event(after, Object, Message, Sender, Monitor).

	set_monitor(Object, Message, Sender, Monitor) :-
		define_events(after, Object, Message, Sender, Monitor).

	del_monitors(Object, Message, Sender, Monitor) :-
		abolish_events(after, Object, Message, Sender, Monitor).

	del_monitors :-
		abolish_events(after, _, _, _, _).

:- end_object.
