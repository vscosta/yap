
:- protocol(setp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Set protocol.']).


	:- public(delete/3).

	:- mode(delete(+set, @term, ?set), one).

	:- info(delete/3,
		[comment is 'Deletes an element from a set returning the set of remaining elements.',
		 argnames is ['Set', 'Element', 'Remaining']]).


	:- public(disjoint/2).

	:- mode(disjoint(+set, +set), zero_or_one).

	:- info(disjoint/2, [
		comment is 'True if the two sets have no element in common.',
		argnames is ['Set1', 'Set2']]).


	:- public(equal/2).

	:- mode(equal(+set, +set), zero_or_one).

	:- info(equal/2, [
		comment is 'True if the two sets are equal.',
		argnames is ['Set1', 'Set2']]).


	:- public(empty/1).

	:- mode(empty(+set), zero_or_one).

	:- info(empty/1, [
		comment is 'True if the set is empty.',
		argnames is ['Set']]).


	:- public(insert/3).

	:- mode(insert(+set, +term, ?set), one).

	:- info(insert/3, [
		comment is 'Inserts an element in a set, returning the resulting set.',
		argnames is ['In', 'Element', 'Out']]).


	:- public(insert_all/3).

	:- mode(insert_all(+list, +set, ?set), one).

	:- info(insert_all/3, [
		comment is 'Inserts a list of elemnts in a set, returning the resulting set.',
		argnames is ['List', 'In', 'Out']]).


	:- public(intersect/2).

	:- mode(intersect(+set, +set), zero_or_one).

	:- info(intersect/2, [
		comment is 'True if the two sets have at least one element in common.',
		argnames is ['Set1', 'Set2']]).


	:- public(intersection/3).

	:- mode(intersection(+set, +set, ?set), zero_or_one).

	:- info(intersection/3, [
		comment is 'Returns the intersection of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Intersection']]).


	:- public(length/2).

	:- mode(length(+set, ?integer), zero_or_one).

	:- info(length/2,
		[comment is 'Number of set elements.',
		 argnames is ['Set', 'Length']]).


	:- public(member/2).

	:- mode(member(+term, +set), zero_or_one).
	:- mode(member(-term, +set), zero_or_more).

	:- info(member/2,
		[comment is 'Element is a member of set Set.',
		 argnames is ['Element', 'Set']]).


	:- public(powerset/2).

	:- mode(powerset(+set, -list), one).

	:- info(powerset/2,
		[comment is 'Returns the power set of a set, represented as a list of sets.',
		 argnames is ['Set', 'Powerset']]).


	:- public(select/3).

	:- mode(select(?term, +set, ?set), zero_or_more).

	:- info(select/3,
		[comment is 'Selects an element from a set, returning the set of remaining elements.',
		 argnames is ['Element', 'Set', 'Remaining']]).


	:- public(subset/2).

	:- mode(subset(+set, +set), zero_or_one).
	:- mode(subset(?set, +set), zero_or_more).

	:- info(subset/2, [
		comment is 'True if Subset is a subset of Set.',
		argnames is ['Subset', 'Set']]).


	:- public(subtract/3).

	:- mode(subtract(+set, +set, ?set), zero_or_one).

	:- info(subtract/3, [
		comment is 'True when Difference contains all and only the elements of Set1 which are not also in Set2.',
		argnames is ['Set1', 'Set2', 'Difference']]).


	:- public(symdiff/3).

	:- mode(symdiff(+set, +set, ?set), zero_or_one).

	:- info(symdiff/3, [
		comment is 'True if Difference is the symmetric difference of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Difference']]).


	:- public(union/3).

	:- mode(union(+set, +set, ?set), zero_or_one).

	:- info(union/3, [
		comment is 'True if Union is the union of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Union']]).


:- end_protocol.
