
% this is a single-line comment

/*
this is
a block
comment
*/

:- object(class,
	implements(protocol),
	imports(category),
	instantiates(metaclass),
	specializes(superclass)).


 	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2003/12/18,
		comment is 'Sample class for testing syntax coloring.']).

	:- dynamic.

    :- calls(some_other_protocol).

    :- uses(another_object).

:- end_object.
