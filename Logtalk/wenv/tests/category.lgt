
% this is a single-line comment

/*
this is
a block
comment
*/

:- category(category,
	implements(protocol)).

 	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2003/12/18,
		comment is 'Sample category for testing syntax coloring.']).

	:- dynamic.

    :- calls(some_other_protocol).

    :- uses(another_object).

:- end_category.
