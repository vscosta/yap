
:- object(sort(_Type)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'List sorting parameterized by the type of the list elements.',
		parnames is ['Type'],
		source is 'Example adopted from the SICStus Objects documentation.']).


	:- uses(list).

	:- calls(comparingp).


	:- public(sort/2).

	:- mode(sort(+list, -list), one).

	:- info(sort/2, [
		comment is 'Sorts a list in ascending order.',
		argnames is ['List', 'Sorted']]).


	:- private(partition/4).

	:- mode(partition(+list, +nonvar, -list, -list), one).

	:- info(partition/4, [
		comment is 'List partition in two sub-lists using a pivot.',
		argnames is ['List', 'Pivot', 'Lowers', 'Biggers']]).


	sort([], []).

	sort([P| L], S) :-
		partition(L, P, Small, Large),
		sort(Small, S0),
		sort(Large, S1),
		list::append(S0, [P| S1], S).


	partition([], _, [], []).

	partition([X| L1], P, Small, Large) :-
		parameter(1, Type),
		(	Type::(X < P) ->
			Small = [X| Small1], Large = Large1
		;	Small = Small1, Large = [X| Large1]
		),
		partition(L1, P, Small1, Large1).


:- end_object.
