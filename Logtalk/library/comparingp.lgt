
:- protocol(comparingp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Comparing protocol using overloading of standard operators.']).

	:- public((<)/2).
	:- mode(<(+term, +term), zero_or_one).
	:- info((<)/2, [
		comment is 'True if Term1 is less than Term2.',
		argnames is ['Term1', 'Term2']]).

	:- public((=<)/2).
	:- mode(=<(+term, +term), zero_or_one).
	:- info((=<)/2, [
		comment is 'True if Term1 is less or equal than Term2.',
		argnames is ['Term1', 'Term2']]).

	:- public((>)/2).
	:- mode(>(+term, +term), zero_or_one).
	:- info((>)/2, [
		comment is 'True if Term1 is greater than Term2.',
		argnames is ['Term1', 'Term2']]).

	:- public((>=)/2).
	:- mode(>=(+term, +term), zero_or_one).
	:- info((>=)/2, [
		comment is 'True if Term1 is equal or grater than Term2.',
		argnames is ['Term1', 'Term2']]).

	:- public((=:=)/2).
	:- mode(=:=(+term, +term), zero_or_one).
	:- info((=:=)/2, [
		comment is 'True if Term1 is equal to Term2.',
		argnames is ['Term1', 'Term2']]).

	:- public((=\=)/2).
	:- mode(=\=(+term, +term), zero_or_one).
	:- info((=\=)/2, [
		comment is 'True if Term1 is not equal to Term2.',
		argnames is ['Term1', 'Term2']]).

:- end_protocol.
