
:- object(time,
	implements(timep)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Time predicates.']).


	now(Hours, Mins, Secs) :-
		{lgt_current_time(Hours, Mins, Secs)}.


	cpu_time(Seconds) :-
		{lgt_cpu_time(Seconds)}.


	valid(Hours, Mins, Secs) :-
		integer(Hours), Hours >= 0,
		integer(Mins), Mins >= 0, Mins =< 59,
		integer(Secs), Secs >= 0, Secs =< 59.


:- end_object.
