
:- object(list(_Type),
	extends(list)).

	:- info([
		version is 1.01,
		author is 'Paulo Moura',
		date is 2006/4/25,
		comment is 'List predicates with elements constrained to a single type.']).

	valid(List) :-
		nonvar(List),
		parameter(1, Type),
		\+ \+ valid(List, Type).

	valid([], _).
	valid([Value| List], Type) :-
		Type::valid(Value),
		valid(List, Type).

:- end_object.
