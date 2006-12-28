
:- object(stop_watch,
	implements(monitoring),
	imports(monitor)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/12/14,
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
