
:- object(timer).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Call executing time profiler.']).


	:- uses(time).
	:- uses(loop).


	:- public(timer/2).
	:- metapredicate(timer(::, *)).

	:- mode(timer(+callable, -number), one).

	:- info(timer/2,
		[comment is 'Returns time to execute a call.',
		 argnames is ['Call', 'Time']]).


	:- public(timer/3).
	:- metapredicate(timer(::, *, *)).

	:- mode(timer(+callable, +integer, -float), one).

	:- info(timer/3,
		[comment is 'Returns the average time needed to to execute a call.',
		 argnames is ['Call', 'Times', 'Time']]).


	timer(Call, Time) :-
		time::cpu_time(Start),
		(call(Call) -> true; true),
		time::cpu_time(End),
		Time is End - Start.


	timer(Call, Times, Time) :-
		time::cpu_time(Start),
		loop::forto(1, Times, Call),
		time::cpu_time(End),
		Time is (End - Start) / Times.


:- end_object.
