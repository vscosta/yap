
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

:- end_protocol.
