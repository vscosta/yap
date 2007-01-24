
:- object(msglog,
	implements(monitoring)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/01/13,
		comment is 'Monitor for recording, replaying, and saving user messages.']).


	:- public(record/0).
	:- mode(record, one).
	:- info(record/0,
		[comment is 'Starts recording messages.']).

	:- public(stop/0).
	:- mode(stop, one).
	:- info(stop/0,
		[comment is 'Stops recording messages.']).

	:- public(replay/0).
	:- mode(replay, one).
	:- info(replay/0,
		[comment is 'Replays all recorded messages.']).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0,
		[comment is 'Prints recorded messages, one per line.']).

	:- public(erase/0).
	:- mode(erase, one).
	:- info(erase/0,
		[comment is 'Erases recorded messages.']).


	:- private(log_/2).
	:- dynamic(log_/2).
	:- mode(log_(+object, +nonvar), zero_or_more).
	:- info(log_/2,
		[comment is 'Table of recorded messages.',
		 argnames is ['Object', 'Message']]).


	record :-
		self(Self),
		abolish_events(_, _, _, _, Self),
		define_events(before, _, _, user, Self).


	stop :-
		self(Self),
		abolish_events(_, _, _, _, Self).


	replay :-
		self(Self),
		abolish_events(_, _, _, _, Self),
		forall(::log_(Object, Message), {Object::Message}).


	print :-
		forall(
			::log_(Object, Message),
			(writeq(Object), write('::'), writeq(Message), write('.'), nl)).


	erase :-
		::retractall(log_(_, _)).


	before(Object, Message, _) :-
		self(Self),
		(Self = Object ->
			true
			;
			::assertz(log_(Object, Message))).


:- end_object.

