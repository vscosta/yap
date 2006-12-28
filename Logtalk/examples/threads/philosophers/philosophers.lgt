
:- category(chopstick).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/12/2,
		comment is 'Dining philosophers problem: chopstick representation.']).

	:- public(pick_up/0).
	:- mode(pick_up, zero_or_one).
	:- info(pick_up/0, [
		comment is 'A Philosopher picks up the chopstick.']).

	:- public(put_down/0).
	:- mode(put_down, zero_or_one).
	:- info(put_down/0, [
		comment is 'A Philosopher puts down the chopstick.']).

	:- private(available/0).
	:- dynamic(available/0).
	:- mode(available, zero_or_one).
	:- info(available/0, [
		comment is 'Chopstick state (either available or in use).']).

	% chopstick actions (picking up and putting down) are synchronized using the same mutext
	% such that a chopstick can only be handled by a single philosopher at a time:
	:- synchronized([pick_up/0, put_down/0]).

	pick_up :-
		::available,
		::retract(available).

	put_down :-
		\+ ::available,
		::asserta(available).

:- end_category.


:- object(cs1,
	imports(chopstick)).

	:- dynamic(available/0).

	available.

:- end_object.


:- object(cs2,
	imports(chopstick)).

	:- dynamic(available/0).

	available.

:- end_object.


:- object(cs3,
	imports(chopstick)).

	:- dynamic(available/0).

	available.

:- end_object.


:- object(cs4,
	imports(chopstick)).

	:- dynamic(available/0).

	available.

:- end_object.


:- object(cs5,
	imports(chopstick)).

	:- dynamic(available/0).

	available.

:- end_object.


:- category(philosopher).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/12/2,
		comment is 'Dining philosophers problem: philosopher representation.']).

	:- public(left_chopstick/1).
	:- mode(left_chopstick(?object_identifier), zero_or_one).
	:- info(left_chopstick/1, [
		comment is 'Chopstick at the left of a philosopher.',
		argnames is ['Chopstick']]).

	:- public(right_chopstick/1).
	:- mode(right_chopstick(?object_identifier), zero_or_one).
	:- info(right_chopstick/1, [
		comment is 'Chopstick at the right of a philosopher.',
		argnames is ['Chopstick']]).

	:- public(run/2).
	:- mode(run(+integer, +integer), one).
	:- info(run/2, [
		comment is 'Runs Count number of thinking/eating cycles, with each activity taking MaxTime (in seconds).',
		argnames is ['Count', 'MaxTime']]).

	:- private(message/1).
	:- synchronized(message/1).
	:- mode(message(+list), one).
	:- info(message/1, [
		comment is 'Writes all the terms on a list as an atomic operation.',
		argnames is ['Atoms']]).

	:- private(random/2).
	:- synchronized(random/2).
	:- mode(random(+integer, -integer), one).
	:- info(random/2, [
		comment is 'Ensures synchronized access to the random number generator.',
		argnames is ['Limit', 'Random']]).

	run(0, _) :-
		this(Philosopher),
		message([Philosopher, ' terminated.']).

	run(Count, MaxTime) :-
		think(MaxTime),
		(	eat(MaxTime) ->
			Count2 is Count - 1,
			run(Count2, MaxTime)
		;	run(Count, MaxTime)
		).

	think(MaxTime):-
		this(Philosopher),
		random(MaxTime, ThinkTime),
		message(['Philosopher ', Philosopher, ' thinking for ', ThinkTime, ' seconds.']),
		sleep(ThinkTime).

	% deadlock while a philosopher is trying to eat is prevented by putting
	% down the first chopstick when picking up the second one fails:
	eat(MaxTime):-
		this(Philosopher),
		random(MaxTime, EatTime),
		::left_chopstick(LeftStick),
		::right_chopstick(RightStick),
		LeftStick::pick_up,
		(	RightStick::pick_up ->
			message(['Philosopher ', Philosopher, ' eating for ', EatTime, ' seconds with chopsticks ', LeftStick, ' and ', RightStick, '.']),
			sleep(EatTime),
			::LeftStick::put_down,
			::RightStick::put_down
		;	::LeftStick::put_down,
			fail
		).

	% as the "random" library object is not multi-threading aware, we must use a  
	% synchronized wrap up predicate (random/2) to call the random number generator:
	random(Limit, Value) :-
		random::random(1, Limit, Value).

	% writing a message needs to be synchronized as it's accomplished  
	% using a combination of individual write/1 (and nl/0) calls:
	message([]) :-
		nl,
		flush_output.
	message([Atom| Atoms]) :-
		write(Atom),
		message(Atoms).

:- end_category.


:- object(p1,
	imports(philosopher)).

	:- threaded.

	left_chopstick(cs5).
	right_chopstick(cs1).

:- end_object.


:- object(p2,
	imports(philosopher)).

	:- threaded.

	left_chopstick(cs1).
	right_chopstick(cs2).

:- end_object.


:- object(p3,
	imports(philosopher)).

	:- threaded.

	left_chopstick(cs3).
	right_chopstick(cs2).

:- end_object.


:- object(p4,
	imports(philosopher)).

	:- threaded.

	left_chopstick(cs4).
	right_chopstick(cs3).

:- end_object.


:- object(p5,
	imports(philosopher)).

	:- threaded.

	left_chopstick(cs5).
	right_chopstick(cs4).

:- end_object.
