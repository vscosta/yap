
:- protocol(metap).


	:- info([
		version is 2,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Useful meta-predicates protocol.']).


	:- public(apply/2).

	:- mode(apply(+callable, +list), zero_or_more).

	:- info(apply/2, [
		comment is 'Applies a predicate to list of arguments.',
		argnames is ['Predicate', 'List']]).


	:- public(callable/1).

	:- mode(callable(@term), zero_or_one).

	:- info(callable/1, [
		comment is 'True if the argument can be called as a goal.',
		argnames is ['Term']]).


	:- public(filter/3).

	:- mode(filter(+callable, +list, -list), one).

	:- info(filter/3, [
		comment is 'Returns a list of all list elements that satisfy a predicate using apply/2.',
		argnames is ['Predicate', 'In', 'Out']]).


	:- public(map/3).

	:- mode(map(+callable, ?list, ?list), zero_or_more).

	:- info(map/3, [
		comment is 'Maps a predicate over a list of elements using apply/2.',
		argnames is ['Predicate', 'In', 'Out']]).


	:- public(succeeds/2).

	:- mode(succeeds(+callable, +list), zero_or_more).

	:- info(succeeds/2, [
		comment is 'True if the predicate succeeds for each list element using apply/2.',
		argnames is ['Predicate', 'List']]).


:- end_protocol.
