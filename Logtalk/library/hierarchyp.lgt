
:- protocol(hierarchyp).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Common hierarchy protocol for prototype and class hierarchies.']).


	:- public(ancestor/1).

	:- mode(ancestor(?object), zero_or_more).

	:- info(ancestor/1, [
		comment is 'Returns, by backtracking, all object ancestors.',
		argnames is ['Ancestor']]).


	:- public(ancestors/1).

	:- mode(ancestors(-list), one).

	:- info(ancestors/1, [
		comment is 'List of all object ancestors.',
		argnames is ['Ancestors']]).


	:- public(leaf/1).

	:- mode(leaf(?object), zero_or_more).

	:- info(leaf/1, [
		comment is 'Returns, by backtracking, all object leaves.',
		argnames is ['Leaf']]).


	:- public(leaves/1).

	:- mode(leaves(-list), one).

	:- info(leaves/1, [
		comment is 'List of all object leaves.',
		argnames is ['Leaves']]).


	:- public(descendant/1).

	:- mode(descendant(?object), zero_or_more).

	:- info(descendant/1, [
		comment is 'Returns, by backtracking, all object descendants.',
		argnames is ['Descendant']]).


	:- public(descendants/1).

	:- mode(descendants(-list), one).

	:- info(descendants/1, [
		comment is 'List of all object descendants.',
		argnames is ['Descendants']]).


:- end_protocol.
