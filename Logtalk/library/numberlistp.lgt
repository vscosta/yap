
:- protocol(numberlistp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'List of numbers protocol.']).


	:- public(sum/2).

	:- mode(sum(+list, ?number), zero_or_one).

	:- info(sum/2,
		[comment is 'Calculates the sum of all list values.',
		 argnames is ['List', 'Sum']]).


	:- public(average/2).

	:- mode(average(+list, ?number), zero_or_one).

	:- info(average/2,
		[comment is 'Calculates the average of a list  of values.',
		 argnames is ['List', 'Average']]).


:- end_protocol.
