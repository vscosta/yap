
:- object(stop_watch,
	implements(event_handlersp),
	imports(monitor)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Message executing time monitor.']).


	:- uses(time).


	before(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('STARTING at '),
		time::cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

	after(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('ENDING at '),
		time::cpu_time(Seconds), write(Seconds), write(' seconds'), nl.


:- end_object.
