
:- protocol(numberlistp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/4/20,
		comment is 'List of numbers protocol.']).

	:- public(product/2).
	:- mode(product(+list(number), ?number), zero_or_one).
	:- info(product/2,
		[comment is 'Calculates the product of all list values.',
		 argnames is ['List', 'Product']]).

	:- public(sum/2).
	:- mode(sum(+list(number), ?number), zero_or_one).
	:- info(sum/2,
		[comment is 'Calculates the sum of all list values.',
		 argnames is ['List', 'Sum']]).

	:- public(average/2).
	:- mode(average(+list(number), ?float), zero_or_one).
	:- info(average/2,
		[comment is 'Calculates the average of a list  of values.',
		 argnames is ['List', 'Average']]).

:- end_protocol.
