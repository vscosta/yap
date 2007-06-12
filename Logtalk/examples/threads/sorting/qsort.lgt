
:- object(qsort(_Threads)).

	:- info([
		version is 1.0,
		author is 'Paul Croker and Paulo Moura',
		date is 2007/03/29,
		comment is 'Multi-threaded version of the quick sort algorithm.',
		parameters is ['Threads'- 'Number of threads to use in sorting. Valid values are 1, 2, 4, 8, 16, 32, etc.']]).

	:- threaded.

	:- public(qsort/2).
	:- mode(qsort(+list, -list), one).
	:- info(qsort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']]).

	qsort(List, Sorted) :-
		parameter(1, Threads),
		qsort(List, [], Sorted, Threads).

	qsort([], Sorted, Sorted, _).
	qsort([Pivot| Rest], Acc, Sorted, N) :-
		(	N =:= 1 ->
			quicksort([Pivot| Rest], Acc, Sorted)
		;	N2 is N//2,
			partition(Rest, Pivot, Smaller0, Bigger0),
			threaded((
				qsort(Smaller0, [Pivot| Bigger], Sorted, N2),
				qsort(Bigger0, Acc, Bigger, N2)
			)), !	% SWI-Prolog needs help with determinism detection
		).

    partition([], _, [], []).
    partition([X| Xs], Pivot, Smalls, Bigs) :-
		(	X < Pivot ->
			Smalls = [X| Rest],
			partition(Xs, Pivot, Rest, Bigs)
		;	Bigs = [X| Rest],
			partition(Xs, Pivot, Smalls, Rest)
		).

    quicksort([], Sorted, Sorted).
    quicksort([Pivot| Rest], Acc, Sorted) :- 
		partition(Rest, Pivot, Smaller0, Bigger0),
		quicksort(Smaller0, [Pivot| Bigger], Sorted),
		quicksort(Bigger0, Acc, Bigger).

:- end_object.
