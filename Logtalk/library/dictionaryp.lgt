
:- protocol(dictionaryp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Dictionary protocol.']).

	:- public(as_dictionary/2).
	:- mode(as_dictionary(@list, -dictionary), one).
	:- info(as_dictionary/2, [
		comment is 'Converts a list of key-value pairs to a dictionary.',
		argnames is ['List', 'Dictionary']]).

	:- public(as_list/2).
	:- mode(as_list(@dictionary, -list), one).
	:- info(as_list/2, [
		comment is 'Converts a dictionary to a list of key-value pairs.',
		argnames is ['Dictionary', 'List']]).

	:- public(delete/4).
	:- mode(delete(+dictionary, @ground, ?term, -dictionary), zero_or_one).
	:- info(delete/4, [
		comment is 'Deletes a matching Key-Value pair from a dictionary, returning the updated dictionary.',
		argnames is ['Dictionary_in', 'Key', 'Value', 'Dictionary_out']]).

	:- public(empty/1).
	:- mode(empty(@dictionary), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the dictionary is empty.',
		argnames is ['Dictionary']]).

	:- public(insert/4).
	:- mode(insert(+ground, @term, +dictionary, -dictionary), one).
	:- info(insert/4, [
		comment is 'Inserts a Key-Value pair into a dictionary, returning the updated dictionary.',
		argnames is ['Key', 'Value', 'Dictionary_in', 'Dictionary_out']]).

	:- public(insert_all/3).
	:- mode(insert_all(@list, +dictionary, -dictionary), one).
	:- info(insert_all/3, [
		comment is 'Inserts a list of Key-Value pairs into a dictionary, returning the updated dictionary.',
		argnames is ['List', 'Dictionary_in', 'Dictionary_out']]).

	:- public(lookup/3).
	:- mode(lookup(+ground, ?term, @dictionary), zero_or_one).
	:- mode(lookup(-ground, ?term, @dictionary), zero_or_more).
	:- info(lookup/3, [
		comment is 'Get a matching Key-Value pair from a dictionary.',
		argnames is ['Key', 'Value', 'Dictionary']]).

	:- public(keys/2).
	:- mode(keys(@dictionary, -list), one).
	:- info(keys/2, [
		comment is 'Returns a list with all dictionary keys.',
		argnames is ['Dictionary', 'List']]).

	:- public(map/3).
	:- mode(map(+functor, +dictionary, -dictionary), zero_or_one).
	:- info(map/3, [
		comment is 'Maps a binary predicate over each dictionary key-value pair returning a new pair.',
		argnames is ['Functor', 'In', 'Out']]).

	:- public(size/2).
	:- mode(size(@dictionary, ?integer), zero_or_one).
	:- info(size/2, [
		comment is 'Number of dictionary entries.',
		argnames is ['Dictionary', 'Size']]).

:- end_protocol.
