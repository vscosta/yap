
:- object(date(_Year, _Month, _Day)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Dates as parametric objects.',
		parnames is ['Year', 'Month', 'Day']]).


	:- public(year/1).
	:- mode(year(?integer), one).

	:- public(month/1).
	:- mode(month(?integer), one).

	:- public(day/1).
	:- mode(day(?integer), one).

	:- public(today/0).
	:- mode(today, one).

	:- public(leap_year/0).
	:- mode(leap_year, zero_or_one).


	year(Year) :-
		parameter(1, Year).


	month(Month) :-
		parameter(2, Month).


	day(Day) :-
		parameter(3, Day).


	today :-
		{'$lgt_current_date'(Year, Month, Day)},
		parameter(1, Year),
		parameter(2, Month),
		parameter(3, Day).


	leap_year :-
		parameter(1, Year),
		(0 is mod(Year, 4),
		 0 is mod(Year, 100)
		 ;
		 0 is mod(Year, 400)),
		!.


:- end_object.
