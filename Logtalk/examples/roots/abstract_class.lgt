
:- object(abstract_class,
	implements(abstract_classp),
	instantiates(class),
	specializes(object)).


	:- info([
		version is 2,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Default metaclass for all abstract classes.']).


	metaclass :-
		self(Self),
		instantiates_class(Class, Self),
		this(This),
		Class::ancestor(This).


	abstract_class :-
		self(Self),
		Self \= abstract_class.


	strict_instance :-
		fail.


:- end_object.
