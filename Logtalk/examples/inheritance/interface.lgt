
:- category(interface).


	:- public(interface/0).
	:- mode(interface, one).


	interface :-
		forall(
			(::current_predicate(Functor/Arity),
			 functor(Pred, Functor, Arity)),
			(::predicate_property(Pred, Prop), scope_property(Prop),
			 writeq(Functor/Arity), write(' - '), writeq(Prop), nl)).


	scope_property(public).
	scope_property(protected).
	scope_property(private).


:- end_category.
