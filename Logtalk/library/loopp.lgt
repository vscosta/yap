 
:- protocol(loopp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Loop control structures protocol.']).


	:- public(dowhile/2).
	:- metapredicate(dowhile(::, ::)).

	:- mode(dowhile(+callable, @callable), zero_or_one).

	:- info(dowhile/2, [
		comment is 'Do Action while Condition is true.',
		argnames is ['Action', 'Condition']]).


	:- public(forto/3).
	:- metapredicate(forto(*, *, ::)).

	:- mode(forto(+integer, +integer, @callable), zero_or_one).

	:- info(forto/3, [
		comment is 'Counting from First to Last do Call.',
		argnames is ['First', 'Last', 'Call']]).


	:- public(forto/4).
	:- metapredicate(forto(*, *, *, ::)).

	:- mode(forto(-integer, +integer, +integer, @callable), zero_or_one).

	:- info(forto/4, [
		comment is 'Do Call counting from First to Last and instantiating Count to each sucessive value.',
		argnames is ['Count', 'First', 'Last', 'Call']]).


	:- public(fordownto/3).
	:- metapredicate(fordownto(*, *, ::)).

	:- mode(fordownto(+integer, +integer, @callable), zero_or_one).

	:- info(fordownto/3, [
		comment is 'Counting from First to Last do Call.',
		argnames is ['First', 'Last', 'Call']]).


	:- public(fordownto/4).
	:- metapredicate(fordownto(*, *, *, ::)).

	:- mode(fordownto(-integer, +integer, +integer, @callable), zero_or_one).

	:- info(fordownto/4, [
		comment is 'Do Call counting from First to Last and instantiating Count to each sucessive value.',
		argnames is ['Count', 'First', 'Last', 'Call']]).


	:- public(whiledo/2).
	:- metapredicate(whiledo(::, ::)).

	:- mode(whiledo(+callable, @callable), zero_or_one).

	:- info(whiledo/2, [
		comment is 'While Condition is true do Action.',
		argnames is ['Condition', 'Action']]).


:- end_protocol.
