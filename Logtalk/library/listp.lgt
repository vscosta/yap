
:- protocol(listp).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2003/4/17,
		comment is 'List protocol.']).


	:- public(append/3).

	:- mode(append(?list, ?list, ?list), zero_or_more).

	:- info(append/3, [
		comment is 'Appends two lists.',
		argnames is ['List1', 'List2', 'List']]).


	:- public(delete/3).

	:- mode(delete(@list, @term, ?list), one).

	:- info(delete/3,
		[comment is 'Deletes from a list all ocurrences of an element returning the list of remaining elements.',
		 argnames is ['List', 'Element', 'Remaining']]).


	:- public(delete_matches/3).

	:- mode(delete_matches(@list, @term, ?list), one).

	:- info(delete_matches/3,
		[comment is 'Deletes all matching elements from a list, returning the list of remaining elements.',
		 argnames is ['List', 'Element', 'Remaining']]).


	:- public(empty/1).

	:- mode(empty(@list), zero_or_one).

	:- info(empty/1,
		[comment is 'True if the argument is an empty list.',
		 argnames is ['List']]).


	:- public(flatten/2).

	:- mode(flatten(+list, -list), one).

	:- info(flatten/2,
		[comment is 'Flattens a list of lists into a list.',
		 argnames is ['List', 'Flatted']]).


	:- public(keysort/2).

	:- mode(keysort(+list, -list), one).

	:- info(keysort/2,
		[comment is 'Sorts a list of key-value pairs in ascending order.',
		 argnames is ['List', 'Sorted']]).


	:- public(last/2).

	:- mode(last(?list, ?term), zero_or_more).

	:- info(last/2,
		[comment is 'List last element (if it exists).',
		 argnames is ['List', 'Last']]).


	:- public(length/2).

	:- mode(length(?list, ?integer), zero_or_more).

	:- info(length/2,
		[comment is 'List length.',
		 argnames is ['List', 'Length']]).


	:- public(max/2).

	:- mode(max(+list, -term), zero_or_one).

	:- info(max/2,
		[comment is 'Determines the list maximum value using standard order. Fails if the list is empty.',
		 argnames is ['List', 'Maximum']]).


	:- public(member/2).

	:- mode(member(?term, ?list), zero_or_more).

	:- info(member/2,
		[comment is 'Element is a list member.',
		 argnames is ['Element', 'List']]).


	:- public(memberchk/2).

	:- mode(memberchk(?term, ?list), zero_or_one).

	:- info(memberchk/2,
		[comment is 'Checks if a term is a member of a list.',
		 argnames is ['Element', 'List']]).


	:- public(min/2).

	:- mode(min(+list, -term), zero_or_one).

	:- info(min/2,
		[comment is 'Determines the minimum value in a list using standard order. Fails if the list is empty.',
		 argnames is ['List', 'Minimum']]).


	:- public(nth/3).

	:- mode(nth(?integer, +list, ?term), zero_or_more).

	:- info(nth/3, [
		comment is 'Nth element of a list.',
		argnames is ['Nth', 'List', 'Element']]).


	:- public(permutation/2).

	:- mode(permutation(?list, ?list), zero_or_more).

	:- info(permutation/2,
		[comment is 'The two lists are a permutation of the same list.',
		 argnames is ['List', 'Permutation']]).


	:- public(prefix/2).

	:- mode(prefix(?list, +list), zero_or_more).

	:- info(prefix/2,
		[comment is 'Prefix is a prefix of List.',
		 argnames is ['Prefix', 'List']]).


	:- public(reverse/2).

	:- mode(reverse(+list, ?list), zero_or_one).
	:- mode(reverse(?list, +list), zero_or_one).
	:- mode(reverse(-list, -list), one_or_more).

	:- info(reverse/2,
		[comment is 'Reverses a list.',
		 argnames is ['List', 'Reversed']]).


	:- public(same_length/2).

	:- mode(same_length(+list, ?list), zero_or_one).
	:- mode(same_length(?list, +list), zero_or_one).
	:- mode(same_length(-list, -list), one_or_more).

	:- info(same_length/2,
		[comment is 'The two lists have the same length.',
		 argnames is ['List1', 'List2']]).


	:- public(select/3).

	:- mode(select(?term, +list, ?list), zero_or_more).
	:- mode(select(?term, ?list, +list), zero_or_more).

	:- info(select/3,
		[comment is 'Selects an element from a list, returning the list of remaining elements.',
		 argnames is ['Element', 'List', 'Remaining']]).


	:- public(sort/2).

	:- mode(sort(+list, -list), one).

	:- info(sort/2,
		[comment is 'Sorts a list in ascending order.',
		 argnames is ['List', 'Sorted']]).


	:- public(sublist/2).

	:- mode(sublist(?list, +list), zero_or_more).

	:- info(sublist/2,
		[comment is 'The first list is a sublist of the second.',
		 argnames is ['Sublist', 'List']]).


	:- public(subtract/3).

	:- mode(subtract(+list, +list, -list), one).

	:- info(subtract/3,
		[comment is 'Removes all elements in the second list from the first list, returning the list of remaining elements.',
		 argnames is ['List', 'Elements', 'Remaining']]).


	:- public(suffix/2).

	:- mode(suffix(?list, +list), zero_or_more).

	:- info(suffix/2,
		[comment is 'Suffix is a suffix of List.',
		 argnames is ['Suffix', 'List']]).


:- end_protocol.
