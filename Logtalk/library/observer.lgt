
:- category(observer).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 03/02/09,
		comment is 'Smalltalk dependent protocol.']).

	:- public(update/1).
	:- mode(update(?nonvar), zero_or_one).
	:- info(update/1,
		[comment is 'Called when an observed object is updated.',
		 argnames is ['Change']]).

	update(_).

:- end_category.
