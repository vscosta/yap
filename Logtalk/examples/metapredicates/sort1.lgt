
% sort code adopted from an example in the SICStus Prolog User Manual
% metapredicate example taken from Prolog Part 2, Modules - Committee Draft


:- object(sort(_Type)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'List sorting parameterized by the type of the list elements.']).


	:- uses(list).
	:- uses(tracer).

	:- calls(comparingp).


	:- public(sort/2).

	:- mode(sort(+list, -list), one).

	:- info(sort/2, [
		comment is 'Sorts a list in ascending order.',
		argnames is ['List', 'Sorted']]).


	:- private(partition/4).

	:- mode(partition(+list, +nonvar, -list, -list), one).

	:- info(partition/4, [
		comment is 'Partition a list in two lists containing the elements smaller and larger than a pivot.',
		argnames is ['List', 'Pivot', 'Small', 'Large']]).


	sort([], []).

	sort([Head| Tail], Sorted) :-
		tracer::(
			trace(partition(Tail, Head, Small, Large)),
			trace(sort(Small, Sorted1)),
			trace(sort(Large, Sorted2))),
		list::append(Sorted1, [Head| Sorted2], Sorted).


	partition([], _, [], []).

	partition([Head| Tail], Pivot, Small, Large) :-
		parameter(1, Type),
		(	Type::(Head < Pivot) ->
			Small = [Head| Small1], Large = Large1
		;	Small = Small1, Large = [Head| Large1]
		),
		partition(Tail, Pivot, Small1, Large1).


:- end_object.
