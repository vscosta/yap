
:- protocol(datep).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Date protocol.']).


	:- public(today/3).

	:- mode(today(-integer, -integer, -integer), one).

	:- info(now/3, [
		comment is 'Returns current date.',
		argnames is ['Year', 'Month', 'Day']]).


	:- public(leap_year/1).

	:- mode(leap_year(+integer), zero_or_one).

	:- info(empty/1,
		[comment is 'True if the argument is a leap year.',
		 argnames is ['Year']]).


	:- public(name_of_day/3).

	:- mode(name_of_day(?integer, ?atom, ?atom), zero_or_more).

	:- info(valid/3, [
		comment is 'Name and short name of day.',
		argnames is ['Index', 'Name', 'Short']]).


	:- public(name_of_month/3).

	:- mode(name_of_month(?integer, ?atom, ?atom), zero_or_more).

	:- info(valid/3, [
		comment is 'Name and short name of month.',
		argnames is ['Index', 'Name', 'Short']]).


	:- public(days_in_month/3).

	:- mode(days_in_month(?integer, +integer, ?integer), zero_or_more).

	:- info(valid/3, [
		comment is 'Number of days in a month.',
		argnames is ['Month', 'Year', 'Days']]).


	:- public(valid/3).

	:- mode(valid(@integer, @integer, @integer), zero_or_one).

	:- info(valid/3, [
		comment is 'True if the arguments represent a valid date.',
		argnames is ['Year', 'Month', 'Day']]).


:- end_protocol.
