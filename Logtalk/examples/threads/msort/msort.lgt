
:- object(msort(_Threads)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/03/24,
		comment is 'Single-threaded and multi-threaded versions of the merge sort algorithm.',
		parameters is ['Threads'- 'Number of threads to use in sorting. Valid values are 1, 2, and 4.']]).

	:- threaded.

	:- public(msort/2).
	:- mode(msort(+list, -list), one).
	:- info(msort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']]).

	msort(List, Sorted) :-
		parameter(1, Threads),
		msort(Threads, List, Sorted).

	msort(1, List, Sorted) :-
		st_msort(List, Sorted).
	msort(2, List, Sorted) :-
		mt_msort_2(List, Sorted).
	msort(4, List, Sorted) :-
		mt_msort_4(List, Sorted).

	st_msort([], []).
	st_msort([X], [X]).
	st_msort([X, Y| Xs], Ys) :-
		split([X, Y| Xs], X1s, X2s),
		st_msort(X1s, Y1s),
		st_msort(X2s, Y2s),
		merge(Y1s, Y2s, Ys).

	mt_msort_2(L, S) :-
		split(L, L1, L2),
		threaded((
			st_msort(L1, S1),
			st_msort(L2, S2))),
		merge(S1, S2, S).

	mt_msort_4(L, S) :-
		split(L, L1, L2),
		split(L1, L11, L12),
		split(L2, L21, L22),
		threaded((
			st_msort(L11, S11),
			st_msort(L12, S12),
			st_msort(L21, S21),
			st_msort(L22, S22))),
		threaded((
			merge(S11, S12, S1),
			merge(S21, S22, S2))),
		merge(S1, S2, S).

	split([], [], []).
	split([X| Xs], [X| Ys], Zs) :-
		split(Xs, Zs, Ys).

	merge([X| Xs], [Y| Ys], [X| Zs]) :-
		X @=< Y, !,
		merge(Xs, [Y| Ys], Zs).
	merge([X| Xs], [Y| Ys], [Y| Zs]) :-
		X @> Y, !,
		merge([X | Xs], Ys, Zs).
	merge([], Xs, Xs) :- !.
	merge(Xs, [], Xs).

:- end_object.

