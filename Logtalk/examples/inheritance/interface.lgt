
:- category(interface).


	:- public(interface/0).
	:- mode(interface, one).


	interface :-
		forall(
			(::current_predicate(Functor/Arity),
			 functor(Pred, Functor, Arity)),
			(::predicate_property(Pred, Prop), scope(Prop),
			 writeq(Functor/Arity), write(' - '), writeq(Prop), nl)).


	scope(public).
	scope(protected).
	scope(private).


:- end_category.
