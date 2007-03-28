/*
This category defines a predicate, interface/0, that prints an object 
interface, i.e. predicate names and the corresponding scope properties.

We need to encapsulate the interface/0 predicate in a category instead 
of just defining it in a root object in order to be able to list private 
object predicates.
*/

:- category(interface).

	:- public(interface/0).
	:- mode(interface, one).

	interface :-
		forall(
			(::current_predicate(Functor/Arity),
			 functor(Pred, Functor, Arity),
			 Pred \= interface),
			(::predicate_property(Pred, Prop),
			 scope_property(Prop),	% we are only interested on scope properties
			 writeq(Functor/Arity), write(' - '), writeq(Prop), nl)).

	scope_property(public).
	scope_property(protected).
	scope_property(private).

:- end_category.
