
:- protocol(timep).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Time protocol.']).


	:- public(now/3).

	:- mode(now(-integer, -integer, -integer), one).

	:- info(now/3, [
		comment is 'Returns current time.',
		argnames is ['Hours', 'Mins', 'Secs']]).


	:- public(cpu_time/1).

	:- mode(cpu_time(-number), one).

	:- info(cpu_time/1,
		[comment is 'Returns the current cpu time.',
		 argnames is ['Time']]).


	:- public(valid/3).

	:- mode(valid(+integer, +integer, +integer), zero_or_one).

	:- info(valid/3, [
		comment is 'True if the arguments represent a valid time value.',
		argnames is ['Hours', 'Mins', 'Secs']]).


:- end_protocol.
