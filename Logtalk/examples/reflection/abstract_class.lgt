
:- object(abstract_class,
	instantiates(class),
	specializes(object)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Default metaclass for all abstract classes.']).


	:- public(metaclass/0).
	:- mode(metaclass, zero_or_one).


	:- public(abstract_class/0).
	:- mode(abstract_class, zero_or_one).


	abstract_class :-
		self(Self),
		Self \= abstract_class.


	metaclass :-
		self(Self),
		once((
			instantiates_class(Class, Self),
			Class::current_predicate(abstract_class/0))).


	strict_instance :-
		fail.


:- end_object.
