
:- object(object,
	instantiates(class)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Inheritance root for all objects.']).


	:- public(strict_instance/0).
	:- mode(strict_instance, zero_or_one).

	:- public(print/0).
	:- mode(print, one).


	strict_instance.


	print :-
		self(Self),
		write('Object: '), writeq(Self), nl, nl,
		write('  interface:'), nl,
		forall(
			::current_predicate(Predicate),
			(write('    '), writeq(Predicate), nl)),
		nl.


:- end_object.
