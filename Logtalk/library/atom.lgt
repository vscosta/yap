
:- object(atom,
	extends(atomic)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Atom data type predicates.']).


	valid(Atom) :-
		atom(Atom).


:- end_object.
