
:- object(time(_Hours, _Mins, _Secs)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Time as parametric objects.',
		parnames is ['Hours', 'Mins', 'Secs']]).


	:- public(hours/1).
	:- mode(hours(?integer), one).

	:- public(mins/1).
	:- mode(mins(?integer), one).

	:- public(secs/1).
	:- mode(secs(?integer), one).

	:- public(now/0).
	:- mode(now, one).


	hours(Hours) :-
		parameter(1, Hours).


	mins(Mins) :-
		parameter(2, Mins).


	secs(Secs) :-
		parameter(3, Secs).


	now :-
		{'$lgt_current_time'(Hours, Mins, Secs)},
		parameter(1, Hours),
		parameter(2, Mins),
		parameter(3, Secs).


:- end_object.
