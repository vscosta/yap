
:- category(monitor,
	implements(monitorp)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Monitor predicates.']).


	:- private(spy_point_/4).
	:- dynamic(spy_point_/4).

	:- mode(spy_point_(?event, ?object, ?callable, ?object), zero_or_more).

	:- info(spy_point_/4, [
		comment is 'Stores current spy points.',
		argnames is ['Event', 'Object', 'Message', 'Sender']]).


	monitor_activated :-
		self(Self),
		once(current_event(_, _, _, _, Self)).


	activate_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self),
		forall(
			::spy_point_(Event, Object, Message, Sender),
			define_events(Event, Object, Message, Sender, Self)).


	suspend_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self).


	reset_monitor :-
		self(Self),
		abolish_events(before, _, _, _, Self),
		abolish_events(after, _, _, _, Self).
		::retractall(spy_point_(_, _, _, _)).


	spy_point(Event, Object, Message, Sender) :-
		::spy_point_(Event, Object, Message, Sender).


	set_spy_point(Event, Object, Message, Sender) :-
		::retractall(spy_point_(Event, Object, Message, Sender)),
		once((var(Event); Event = before; Event = after)),
		::assertz(spy_point_(Event, Object, Message, Sender)).


	del_spy_points(Event, Object, Message, Sender) :-
		::retractall(spy_point_(Event, Object, Message, Sender)).


:- end_category.
