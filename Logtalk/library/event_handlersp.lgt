
:- protocol(event_handlersp).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2003/2/12,
		comment is 'Event handlers protocol.']).


	:- public(before/3).

	:- mode(before(@object, @nonvar, @object), zero_or_one).

	:- info(before/3, [
		comment is 'Before event handler.',
		argnames is ['Object', 'Message', 'Sender']]).


	:- public(after/3).

	:- mode(after(@object, @nonvar, @object), zero_or_one).

	:- info(after/3, [
		comment is 'After event handler.',
		argnames is ['Object', 'Message', 'Sender']]).


:- end_protocol.
