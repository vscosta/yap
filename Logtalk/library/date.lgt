
:- object(date,
	implements(datep)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/3/7,
		comment is 'Date predicates.']).


	today(Year, Month, Day) :-
		{'$lgt_current_date'(Year, Month, Day)}.


	leap_year(Year) :-
		(	0 =:= mod(Year, 4), 0 =\= mod(Year, 100) ->
			true
		;	0 =:= mod(Year, 400)
		).


	name_of_day(1, 'Sunday', 'Sun').
	name_of_day(2, 'Monday', 'Mon').
	name_of_day(3, 'Tuesday', 'Tue').
	name_of_day(4, 'Wednesday', 'Wed').
	name_of_day(5, 'Thursday', 'Thu').
	name_of_day(6, 'Friday', 'Fri').
	name_of_day(7, 'Saturday', 'Sat').


	name_of_month( 1, 'January', 'Jan').
	name_of_month( 2, 'February', 'Feb').
	name_of_month( 3, 'March', 'Mar').
	name_of_month( 4, 'April', 'Apr').
	name_of_month( 5, 'May', 'May').
	name_of_month( 6, 'June', 'Jun').
	name_of_month( 7, 'July', 'Jul').
	name_of_month( 8, 'August', 'Aug').
	name_of_month( 9, 'September', 'Sep').
	name_of_month(10, 'October', 'Oct').
	name_of_month(11, 'November', 'Nov').
	name_of_month(12, 'December', 'Dec').


	days_in_month( 1, _, 31).
	days_in_month( 2, Year, Days) :-
		leap_year(Year) -> Days = 29; Days = 28.
	days_in_month( 3, _, 31).
	days_in_month( 4, _, 30).
	days_in_month( 5, _, 31).
	days_in_month( 6, _, 30).
	days_in_month( 7, _, 31).
	days_in_month( 8, _, 31).
	days_in_month( 9, _, 30).
	days_in_month(10, _, 31).
	days_in_month(11, _, 30).
	days_in_month(12, _, 31).


	valid(Year, Month, Day) :-
		integer(Year),
		integer(Month), Month >= 1, Month =< 12,
		integer(Day),
		days_in_month(Month, Year, Days),
		Day >= 1, Day =< Days.


:- end_object.
