
:- object(prototype).


	:- public(object_assert/0).
	:- public(self_assert/0).
	:- public(this_assert/0).

	:- public(dynamic_predicates/0).


	object_assert :-
		self(Self),
		Self::assertz(public_predicate).


	self_assert :-
		::assertz(protected_predicate).


	this_assert :-
		assertz(private_predicate).


	dynamic_predicates :-
		current_predicate(Functor/Arity),
		functor(Predicate, Functor, Arity),
		predicate_property(Predicate, (dynamic)),
		predicate_property(Predicate, Scope),
		scope(Scope),
		writeq(Functor/Arity), write(' - '), writeq(Scope), nl,
		fail.

	dynamic_predicates.


	scope(private).
	scope(protected).
	scope((public)).


:- end_object.
