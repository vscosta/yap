
:- object(class,
	instantiates(class),
	specializes(abstract_class)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Instantiation root and default metaclass for all classes.']).


	:- public(new/1).
	:- mode(new(+object), zero_or_one).

	:- public(delete/1).
	:- mode(delete(+object), zero_or_one).

	:- public(instances/1).
	:- mode(instances(-list), one).


	new(Object) :-
		self(Self),
		create_object(Object, [instantiates(Self)], [], []).


	delete(Object) :-
		self(Self),
		instantiates_class(Object, Self),
		\+ instantiates_class(_, Object),
		\+ specializes_class(_, Object),
		abolish_object(Object).


	instances(Instances) :-
		self(Self),
		findall(Instance, instantiates_class(Instance, Self), Instances).


	abstract_class :-
		fail.


:- end_object.
