
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'List of variables predicates.']).

	member(Element, [Head| _]) :-
		Element == Head.
	member(Element, [_| Tail]) :-
		member(Element, Tail).


	memberchk(Element, [Head| Tail]) :-
		(	Element == Head ->
			true
		;	memberchk(Element, Tail)
		).

	prefix([], _).
	prefix([Head1| Tail1], [Head2| Tail2]) :-
		Head1 == Head2,
		prefix(Tail1, Tail2).

	valid(List) :-
		nonvar(List),
		\+ \+ valid2(List).

	valid2([]).
	valid2([Head| Tail]) :-
		var(Head),
		valid2(Tail).

:- end_object.
