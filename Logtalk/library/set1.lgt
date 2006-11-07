
:- object(set(_Type),
	extends(set)).

	:- info([
		version is 1.01,
		author is 'Paulo Moura',
		date is 2006/4/25,
		comment is 'Set predicates with elements constrained to a single type.']).

	valid(Set) :-
		nonvar(Set),
		parameter(1, Type),
		\+ \+ valid(Set, Type).

	valid([], _) :-
		!.
	valid([Element], Type) :-
		!,
		Type::valid(Element).
	valid([Element1, Element2| Set], Type) :-
		Element1 @< Element2,
		Type::valid(Element1),
		Type::valid(Element2),
		valid([Element2| Set], Type).

:- end_object.
