
:- object(class,
	implements(classp),
	instantiates(class),
	specializes(abstract_class)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2005/3/12,
		comment is 'Default metaclass for all classes.']).


	:- private(instance_counter_/1).
	:- dynamic(instance_counter_/1).

	:- mode(instance_counter_(?integer), zero_or_one).

	:- info(instance_counter_/1, [
		comment is 'Stores a counter of created instances.',
		argnames is ['Counter']]).


	new(Object) :-
		::new(Object, []).


	new(Object, Options) :-
		valid_new_identifier(Object),
		self(Self),
		create_object(Object, [instantiates(Self)], [], []),
		Object::init(Options).


	clone(Object, Clone) :-
		self(Self),
		sender(Sender),
		throw(error(subclass_responsability, Self::clone(Object, Clone), Sender)).


	delete(Object) :-
		::delete(Object, []).


	delete(Object, Options) :-
		::instance(Object),
		Object::free(Options),
		abolish_object(Object).


	delete_all :-
		::delete_all([]).


	delete_all(Options) :-
		::instance(Instance),
		object_property(Instance, (dynamic)),
		::delete(Instance, Options),
		fail.

	delete_all(_) :-
		\+ (::instance(Instance),
			object_property(Instance, (dynamic))).


	instance_base_name(i).


	instance_counter_(0).


	valid_new_identifier(Identifier) :-
		var(Identifier), !,
		retract(instance_counter_(Last)),
		::instance_base_name(Base),
		functor(Base, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		repeat,
			next_integer(Last, Next),
			number_codes(Next, Codes2),
			atom_codes(Atom2, Codes2),
			atom_concat(Functor, Atom2, Identifier),
			atom_concat(Identifier, Atom, Prefix),
			\+ {current_predicate(Prefix/_)},
			asserta(instance_counter_(Next)),
		!.

	valid_new_identifier(Identifier) :-
		once((atom(Identifier); compound(Identifier))),
		functor(Identifier, Functor, Arity),
		number_codes(Arity, Codes),
		atom_codes(Atom, Codes),
		atom_concat(Functor, Atom, Prefix),
		\+ {current_predicate(Prefix/_)}.


	next_integer(N, N1) :-
		N1 is N + 1.

	next_integer(N, N2) :-
		N1 is N + 1,
		next_integer(N1, N2).


	equals(Instance1, Instance2) :-
		self(Self),
		sender(Sender),
		throw(error(subclass_responsability, Self::equals(Instance1, Instance2), Sender)).


	abstract_class :-
		fail.


:- end_object.
