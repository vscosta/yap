
:- object(numberlist,
	implements(numberlistp),
	extends(list)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/4/20,
		comment is 'List of numbers predicates.']).

	average([], 0.0).
	average([N| Ns], Average) :-
		average(Ns, 1, N, Average).

	average([], Length, Sum, Average) :-
		Average is Sum / Length.
	average([N| Ns], Lacc, Sacc, Average) :-
		Lacc2 is Lacc + 1,
		Sacc2 is Sacc + N,
		average(Ns, Lacc2, Sacc2, Average).

	min([N| Ns], Min) :-
		min(Ns, N, Min).

	min([], Min, Min).
	min([N| Ns], Aux, Min) :-
		(	N < Aux ->
			min(Ns, N, Min)
		;	min(Ns, Aux, Min)
		).

	max([N| Ns], Max) :-
		max(Ns, N, Max).

	max([], Max, Max).
	max([N| Ns], Aux, Max) :-
		(	N > Aux ->
			max(Ns, N, Max)
		;	max(Ns, Aux, Max)
		).

	product(List, Product) :-
		product(List, 1, Product).

	product([], Product, Product).
	product([N| Ns], Acc, Product) :-
		Acc2 is Acc * N,
		product(Ns, Acc2, Product).

	sum(List, Sum) :-
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([N| Ns], Acc, Sum) :-
		Acc2 is Acc + N,
		sum(Ns, Acc2, Sum).

	valid(List) :-
		nonvar(List),
		\+ \+ valid2(List).

	valid2([]).
	valid2([Head| Tail]) :-
		number(Head),
		valid2(Tail).

:- end_object.
