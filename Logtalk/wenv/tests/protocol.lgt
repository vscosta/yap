
% this is a single-line comment

/*
this is
a block
comment
*/

:- protocol(extended,
	extends(minimal)).

 	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2003/12/18,
		comment is 'Sample protocol for testing syntax coloring.']).

	:- dynamic.


	:- public(aaa/2).

	:- mode(aaa(?integer, ?integer), zero_or_one).

	:- info(position/2, [
		comment is 'Predicate brief description.',
		argnames is ['Arg1', 'Arg2']]).


	:- protected(bbb/2).

	:- mode(bbb(+integer, -float), one).

	:- info(bbb/2, [
		comment is 'Predicate brief description.',
		argnames is ['Arg1', 'Arg2']]).


	:- private(ccc/2).
	:- dynamic(ccc/2).

	:- mode(ccc(@atom, ?atom), one_or_more).

	:- info(ccc/2, [
		comment is 'Predicate brief description.',
		argnames is ['Arg1', 'Arg2']]).


:- end_protocol.
